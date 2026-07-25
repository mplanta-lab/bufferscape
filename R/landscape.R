
# ---------------------------------------------------------------------------
# MAIN FUNCTION
# ---------------------------------------------------------------------------
#' Buffer-level landscape metrics for point sampling sites
#'
#' Reads one or more KML files, and for every sampling point returns the
#' land-cover composition of a circular buffer around it, computed directly
#' from vector polygons.
#'
#' @param kml Path, or vector of paths, to KML files.
#' @param categories A category dictionary. `NULL` (default) uses the built-in
#'   [mare_categories] schema; otherwise a `data.frame` or a path to an
#'   `.xlsx`/`.csv` file. See [class_dictionary()].
#' @param driver GDAL driver. Leave as `"KML"` -- see Details.
#' @param epsg EPSG code of a **projected** CRS in metres. Default 31983
#'   (SIRGAS 2000 / UTM 23S), appropriate for Rio de Janeiro.
#' @param radii Buffer radii in metres. The largest is treated as primary.
#' @param kernel,lambda Distance-decay kernel and its scale; see
#'   [decay_kernel()].
#' @param grid_res Resolution in metres of the grid used to integrate the
#'   kernel over polygon geometry. Smaller is more exact and slower; 1 m is
#'   ample for a 50 m buffer.
#' @param secondary How to treat a hyphenated code such as `"8-6"`:
#'   `"weighted"` gives the primary class `1 - secondary_weight` of the area and
#'   the secondary class the rest; `"primary"` ignores the secondary code;
#'   `"both"` gives the full area to each, so areas double-count deliberately.
#' @param secondary_weight Share of the area given to the secondary class under
#'   `secondary = "weighted"`.
#' @param trap_pattern,tank_pattern,tank_open_pattern,pool_pattern Regular
#'   expressions matching placemark names for sampling points, water tanks,
#'   the unsealed subset of tanks, and swimming pools. Pools are tested first,
#'   so a name like `"SwimmingPool"` is never mistaken for a tank. The default
#'   `trap_pattern` matches a letter prefix followed by a number
#'   (`SITE_1`, `VP_9`, `NH_12`, `trap 3`); tighten it if your point layer uses
#'   names that could collide with other placemarks.
#' @param check_kml_buffer If `TRUE`, cross-check any `*_Buffer` polygon in the
#'   file against the computed buffer and report its radius.
#' @param out_xlsx Optional path; writes every table to a workbook.
#' @param verbose Print progress.
#'
#' @details
#' # Overlapping polygons
#' Polygons may overlap -- a tree crown over a roof is both -- so category areas
#' are **not** constrained to sum to the buffer area and proportions are not
#' computed. A buffer can exceed 100% classified.
#'
#' # Geometry-integrated decay
#' The weighted area `area_w` integrates the kernel over each polygon on a
#' regular grid, then rescales by the exact `sf::st_area()`. Evaluating the
#' kernel at the polygon centroid instead -- reported as `area_w_centroid` for
#' comparison -- is badly biased for elongated features passing close to the
#' sampling point, whose centroid sits at \eqn{d \approx 0} while most of their
#' area does not. On Maré data the bias reaches 45% for an internal road, while
#' compact roofs stay under 1%.
#'
#' # Why the KML driver
#' GDAL defaults to LIBKML, which overrides `<name>` with an ExtendedData field
#' of the same name. A buffer polygon named `VP_21_Buffer` then reads as
#' `VP_21`, is no longer recognised as reference geometry, and silently enters
#' the land-cover totals as an unclassified polygon. Forcing `driver = "KML"`
#' reads `<name>` correctly.
#'
#' # Nothing is dropped silently
#' Unparseable descriptions, category codes absent from the dictionary, and
#' points matching no pattern all raise warnings and are reported in the `qc`
#' and `unmatched_ids` tables. Distances that were not measured stay `NA` and
#' are never coerced to zero.
#'
#' @return A list with elements `wide` (one row per site, modelling-ready),
#'   `long` (site x category x radius), `tanks`, `distances`, `qc`,
#'   `unmatched_ids`, `meta`, `categories`, and the projected `sf` layers
#'   `traps`, `polygons`, `tanks_sf`, `pools_sf`, `lines`, `buffer_kml`.
#'
#' @examples
#' kml <- system.file("extdata", "example_site.kml", package = "bufferscape")
#' res <- buffer_composition(kml, radii = 50, grid_res = 5, verbose = FALSE)
#' res$tanks
#' head(res$long[res$long$area_m2 > 0, c("full_name", "area_m2", "area_w")])
#'
#' @seealso [batch_composition()] to process a folder, [class_dictionary()] for
#'   custom dictionaries.
#' @export
buffer_composition <- function(
    kml,
    categories        = NULL,
    driver            = "KML",
    epsg              = 31983,
    radii             = 50,
    kernel            = "exponential",
    lambda            = 45,
    grid_res          = 1,
    secondary         = c("weighted", "primary", "both"),
    secondary_weight  = 0.3,
    trap_pattern      = "^[A-Za-z]{2,}[_ -]?\\d+$",
    tank_pattern      = "tank|caixa|reservat",
    tank_open_pattern = "open|abert|descoberta|sem[_ ]?tampa",
    pool_pattern      = "pool|piscina|swimming",
    check_kml_buffer  = TRUE,
    out_xlsx          = NULL,
    verbose           = TRUE
) {

  secondary <- match.arg(secondary)
  if (secondary_weight < 0 || secondary_weight > 1)
    stop("`secondary_weight` must be between 0 and 1.")
  msg <- function(...) if (isTRUE(verbose)) cat(..., "\n", sep = "")

  cat_map <- class_dictionary(categories)

  # ---- 1. READ ------------------------------------------------------------
  kml <- normalizePath(kml, mustWork = TRUE)
  msg("Reading ", length(kml), " KML file(s)...")
  feats <- purrr::map_dfr(kml, function(f) {
    layers <- sf::st_layers(f, do_count = FALSE)$name
    purrr::map_dfr(layers, function(L) {
      x <- suppressWarnings(sf::st_read(f, layer = L, quiet = TRUE,
                                        drivers = driver))
      if (nrow(x) == 0) return(NULL)
      if ("name" %in% names(x) && !"Name" %in% names(x))
        x <- dplyr::rename(x, Name = name)
      if ("description" %in% names(x) && !"Description" %in% names(x))
        x <- dplyr::rename(x, Description = description)
      if (!"Name" %in% names(x))        x$Name <- NA_character_
      if (!"Description" %in% names(x)) x$Description <- NA_character_
      x[, c("Name", "Description")]
    })
  })

  if (nrow(feats) == 0) stop("No features found in the supplied KML file(s).")
  feats <- sf::st_zm(feats, drop = TRUE, what = "ZM")
  gtype <- as.character(sf::st_geometry_type(feats))

  # ---- 2. SEGREGATE FEATURES ---------------------------------------------
  is_pt   <- gtype == "POINT"
  is_poly <- gtype %in% c("POLYGON", "MULTIPOLYGON")
  is_line <- gtype %in% c("LINESTRING", "MULTILINESTRING")

  keep <- function(v) which(v %in% TRUE)   # NA-safe index

  traps <- feats[keep(is_pt & stringr::str_detect(feats$Name, trap_pattern)), ]
  if (nrow(traps) == 0)
    stop("No trap points matched `trap_pattern` (", trap_pattern, ").")
  trap_names <- traps$Name
  msg("  traps found: ", paste(trap_names, collapse = ", "))

  # KML-supplied buffer polygons (e.g. "VP_21_Buffer") are excluded from
  # land-cover analysis; they are reference geometry, not classified surface.
  is_buf     <- stringr::str_detect(feats$Name, "(?i)buffer")
  buffer_kml <- feats[keep(is_poly &  is_buf), ]
  polys      <- feats[keep(is_poly & !is_buf), ]
  lines      <- feats[keep(is_line), ]

  other_pts  <- feats[keep(is_pt & !(feats$Name %in% trap_names)), ]
  # Three water-container point classes are recognised, tested in this order so
  # a "SwimmingPool" marker is never mis-read as a tank:
  #   1. pools  (pool_pattern)        - open standing water, prime oviposition
  #   2. tanks  (tank_pattern)        - split into sealed vs unsealed/open
  # Anything matching none of these is genuinely unknown and is reported.
  is_pool <- stringr::str_detect(other_pts$Name, stringr::regex(pool_pattern, ignore_case = TRUE))
  is_tank <- !is_pool &
             stringr::str_detect(other_pts$Name, stringr::regex(tank_pattern, ignore_case = TRUE))
  pools         <- other_pts[keep(is_pool), ]
  tanks         <- other_pts[keep(is_tank), ]
  unmatched_pts <- other_pts[keep(!is_pool & !is_tank), ]
  tanks$open <- stringr::str_detect(tanks$Name,
                                    stringr::regex(tank_open_pattern, ignore_case = TRUE))

  msg("  polygons: ", nrow(polys), " | lines: ", nrow(lines),
      " | tanks: ", nrow(tanks), " (open: ", sum(tanks$open), ")",
      " | pools: ", nrow(pools),
      if (nrow(buffer_kml) > 0) paste0(" | KML buffers excluded: ", nrow(buffer_kml)) else "")
  if (nrow(unmatched_pts) > 0)
    warning("Points matching no water-container pattern were ignored: ",
            paste(unique(unmatched_pts$Name), collapse = ", "), call. = FALSE)

  # ---- 3. PARSE CATEGORY CODES -------------------------------------------
  qc_unmatched_ids <- tibble::tibble()
  if (nrow(polys) > 0) {
    parsed <- parse_class_codes(polys$Description)
    polys  <- dplyr::bind_cols(polys, parsed)

    bad <- polys[is.na(polys$id_primary), ]
    if (nrow(bad) > 0)
      warning(nrow(bad), " polygon(s) had an unparseable <description> and were dropped.",
              call. = FALSE)
    polys <- polys[!is.na(polys$id_primary), ]

    known <- cat_map$id
    unk   <- setdiff(unique(c(polys$id_primary,
                              polys$id_secondary[!is.na(polys$id_secondary)])), known)
    if (length(unk) > 0) {
      qc_unmatched_ids <- tibble::tibble(unmatched_id = sort(unk))
      warning("Category id(s) present in the KML but absent from the dictionary: ",
              paste(sort(unk), collapse = ", "), call. = FALSE)
    }
  }

  # ---- 4. PROJECT ---------------------------------------------------------
  to_proj <- function(x) if (nrow(x) > 0) sf::st_transform(x, epsg) else x
  traps_p <- to_proj(traps); polys_p <- to_proj(polys)
  lines_p <- to_proj(lines); tanks_p <- to_proj(tanks)
  pools_p <- to_proj(pools); bufkml_p <- to_proj(buffer_kml)

  if (sf::st_is_longlat(traps_p))
    stop("EPSG ", epsg, " is geographic. A projected CRS in metres is required.")

  # ---- 5. PER-TRAP, PER-RADIUS LOOP --------------------------------------
  long_rows <- list(); tank_rows <- list(); qc_rows <- list()

  for (tn in trap_names) {
    trap_i <- traps_p[traps_p$Name == tn, ][1, ]

    for (rad in radii) {
      buf <- sf::st_buffer(trap_i, dist = rad)
      buf_area <- as.numeric(sf::st_area(buf))

      # -- integration grid (used for the kernel-weighted integral) --------
      gpts <- sf::st_make_grid(buf, cellsize = grid_res, what = "centers")
      gpts <- gpts[lengths(sf::st_intersects(gpts, buf)) > 0]
      gd   <- as.numeric(sf::st_distance(gpts, trap_i))
      gw   <- decay_kernel(gd, kernel, lambda)
      cell <- grid_res^2

      # -- polygons ---------------------------------------------------------
      cat_area <- tibble::tibble(id = integer(), area_m2 = numeric(),
                                 area_w = numeric(), area_w_centroid = numeric(),
                                 d_nearest_m = numeric())
      if (nrow(polys_p) > 0) {
        near <- polys_p[lengths(sf::st_intersects(polys_p, buf)) > 0, ]
        if (nrow(near) > 0) {
          clip <- suppressWarnings(sf::st_intersection(near, buf))
          clip <- clip[!sf::st_is_empty(clip), ]
          if (nrow(clip) > 0) {
            clip$area_m2 <- as.numeric(sf::st_area(clip))
            # distance from the site to the nearest point of each polygon,
            # used below for the per-class proximity covariate
            clip$d_near <- as.numeric(sf::st_distance(sf::st_geometry(clip), trap_i))
            hits <- sf::st_intersects(clip, gpts)
            clip$area_grid <- lengths(hits) * cell
            # Mean kernel weight over the polygon, then rescale by the EXACT
            # area. This removes grid-alignment jitter from the weighted area
            # while keeping the geometry-integrated (not centroid) weight.
            # Polygons smaller than one grid cell fall back to the centroid.
            mean_w <- vapply(seq_along(hits), function(i) {
              h <- hits[[i]]
              if (length(h) > 0) mean(gw[h]) else {
                cd <- as.numeric(sf::st_distance(
                  suppressWarnings(sf::st_centroid(sf::st_geometry(clip)[i])), trap_i))
                decay_kernel(cd, kernel, lambda)
              }
            }, numeric(1))
            clip$mean_w <- mean_w
            clip$area_w <- clip$area_m2 * mean_w
            # centroid-only weighting, retained for the methods comparison
            cdist <- as.numeric(sf::st_distance(
              suppressWarnings(sf::st_centroid(sf::st_geometry(clip))), trap_i))
            clip$area_w_centroid <- clip$area_m2 * decay_kernel(cdist, kernel, lambda)

            # split primary / secondary according to `secondary`
            wt_p <- switch(secondary,
                           weighted = ifelse(is.na(clip$id_secondary), 1, 1 - secondary_weight),
                           primary  = 1,
                           both     = 1)
            # Proximity is a different question from abundance: "how close is
            # the nearest patch of this class" is not "how much of it is near".
            # It takes no kernel and no area weighting - a class is either
            # present at some distance or it is not - so both the primary and
            # the secondary code of a mixed polygon count towards it.
            nearest <- dplyr::bind_rows(
              tibble::tibble(id = clip$id_primary, d = clip$d_near),
              tibble::tibble(id = clip$id_secondary, d = clip$d_near)) %>%
              dplyr::filter(!is.na(id)) %>%
              dplyr::group_by(id) %>%
              dplyr::summarise(d_nearest_m = min(d), .groups = "drop")

            prim <- tibble::tibble(id = clip$id_primary,
                                   area_m2 = clip$area_m2 * wt_p,
                                   area_w  = clip$area_w  * wt_p,
                                   area_w_centroid = clip$area_w_centroid * wt_p)
            sec <- tibble::tibble()
            if (secondary != "primary") {
              has_s <- !is.na(clip$id_secondary)
              if (any(has_s)) {
                wt_s <- if (secondary == "weighted") secondary_weight else 1
                sec <- tibble::tibble(id = clip$id_secondary[has_s],
                                      area_m2 = clip$area_m2[has_s] * wt_s,
                                      area_w  = clip$area_w[has_s]  * wt_s,
                                      area_w_centroid = clip$area_w_centroid[has_s] * wt_s)
              }
            }
            cat_area <- dplyr::bind_rows(prim, sec) %>%
              dplyr::group_by(id) %>%
              dplyr::summarise(area_m2 = sum(area_m2),
                               area_w  = sum(area_w),
                               area_w_centroid = sum(area_w_centroid),
                               .groups = "drop") %>%
              dplyr::left_join(nearest, by = "id")

            qc_rows[[paste(tn, rad)]] <- tibble::tibble(
              Ovitrap_ID   = tn, radius_m = rad,
              n_polygons   = nrow(clip),
              area_exact   = sum(clip$area_m2),
              area_grid    = sum(clip$area_grid),
              buffer_area  = buf_area,
              coverage_ratio = sum(clip$area_m2) / buf_area
            )
          }
        }
      }

      full <- cat_map %>%
        dplyr::left_join(cat_area, by = "id") %>%
        dplyr::mutate(area_m2 = tidyr::replace_na(area_m2, 0),
                      area_w  = tidyr::replace_na(area_w, 0),
                      area_w_centroid = tidyr::replace_na(area_w_centroid, 0),
                      # absent class: no distance exists. NOT zero, which would
                      # place the site inside a patch that is not there.
                      d_nearest_m = d_nearest_m,
                      Ovitrap_ID = tn, radius_m = rad)
      long_rows[[paste(tn, rad)]] <- full

      # -- water containers: tanks (sealed/open) + pools --------------------
      if (nrow(tanks_p) > 0) {
        inb <- tanks_p[lengths(sf::st_intersects(tanks_p, buf)) > 0, ]
        n_open <- sum(inb$open); n_seal <- sum(!inb$open)
      } else n_open <- n_seal <- 0L
      n_pool <- if (nrow(pools_p) > 0)
        sum(lengths(sf::st_intersects(pools_p, buf)) > 0) else 0L
      tank_rows[[paste(tn, rad)]] <- tibble::tibble(
        Ovitrap_ID = tn, radius_m = rad,
        tank_open = n_open, tank_sealed = n_seal, tank_total = n_open + n_seal,
        pool_count = n_pool
      )
    }
  }

  long <- dplyr::bind_rows(long_rows)
  tank <- dplyr::bind_rows(tank_rows)
  qc   <- dplyr::bind_rows(qc_rows)

  # ---- 6. DISTANCES (NA when not measured) -------------------------------
  dist_tbl <- tibble::tibble(Ovitrap_ID = trap_names)
  if (nrow(lines_p) > 0) {
    # A reference line belongs to the site whose name it starts with, so the
    # pairing is derived from the sites actually present rather than from a
    # fixed list of name prefixes.
    tp <- paste0("^(",
                 paste(vapply(trap_names,
                              function(z) gsub("([^A-Za-z0-9_ -])", "\\\\\\1", z),
                              character(1)), collapse = "|"),
                 ")[_ -]")
    dl <- lines_p %>%
      dplyr::mutate(length_m = as.numeric(sf::st_length(.))) %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(Ovitrap_ID = stringr::str_extract(Name, tp),
                    Ovitrap_ID = stringr::str_remove(Ovitrap_ID, "[_ -]$"),
                    target = stringr::str_remove(Name, tp)) %>%
      dplyr::filter(!is.na(Ovitrap_ID)) %>%
      dplyr::select(Ovitrap_ID, target, length_m) %>%
      dplyr::distinct(Ovitrap_ID, target, .keep_all = TRUE) %>%
      tidyr::pivot_wider(names_from = target, values_from = length_m,
                         names_prefix = "dist_")
    dist_tbl <- dplyr::left_join(dist_tbl, dl, by = "Ovitrap_ID")
  }

  # ---- 7. WIDE OUTPUT ----------------------------------------------------
  # all three surface metrics are exported:
  #   area_m2          exact area within the buffer
  #   area_w           kernel-weighted, integrated over polygon geometry
  #   area_w_centroid  kernel-weighted at the polygon centroid (comparison only)
  wide_area <- long %>%
    dplyr::select(Ovitrap_ID, radius_m, full_name,
                  area_m2, area_w, area_w_centroid, d_nearest_m) %>%
    tidyr::pivot_wider(
      names_from  = c(full_name, radius_m),
      values_from = c(area_m2, area_w, area_w_centroid, d_nearest_m),
      names_glue  = "{full_name}_{.value}_{radius_m}m"
    )
  wide_tank <- tank %>%
    tidyr::pivot_wider(names_from = radius_m,
                       values_from = c(tank_open, tank_sealed, tank_total, pool_count),
                       names_glue = "{.value}_{radius_m}m")

  wide <- wide_area %>%
    dplyr::left_join(wide_tank, by = "Ovitrap_ID") %>%
    dplyr::left_join(dist_tbl,  by = "Ovitrap_ID")

  meta <- tibble::tibble(
    key = c("kml", "driver", "epsg", "radii_m", "kernel", "lambda_m", "grid_res_m",
            "secondary", "secondary_weight", "n_traps", "run_time", "sf_version"),
    value = c(paste(basename(kml), collapse = "; "), driver, as.character(epsg),
              paste(radii, collapse = ","), kernel, as.character(lambda),
              as.character(grid_res), secondary, as.character(secondary_weight),
              as.character(length(trap_names)),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              as.character(utils::packageVersion("sf")))
  )

  res <- list(wide = wide, long = long, tanks = tank, distances = dist_tbl,
              qc = qc, unmatched_ids = qc_unmatched_ids, meta = meta,
              traps = traps_p, polygons = polys_p, tanks_sf = tanks_p,
              pools_sf = pools_p, lines = lines_p, buffer_kml = bufkml_p,
              categories = cat_map)

  # ---- 8. optional KML buffer cross-check --------------------------------
  if (check_kml_buffer && nrow(bufkml_p) > 0) {
    chk <- purrr::map_dfr(seq_len(nrow(bufkml_p)), function(i) {
      b  <- bufkml_p[i, ]
      nm <- stringr::str_remove(b$Name, "(?i)_?buffer$")
      tp <- traps_p[traps_p$Name == nm, ]
      if (nrow(tp) == 0) return(NULL)
      vtx <- sf::st_cast(sf::st_geometry(b), "POINT")
      d   <- as.numeric(sf::st_distance(vtx, tp))
      tibble::tibble(Ovitrap_ID = nm,
                     kml_area_m2 = as.numeric(sf::st_area(b)),
                     kml_radius_min = min(d), kml_radius_max = max(d),
                     kml_radius_mean = mean(d))
    })
    res$buffer_check <- chk
    if (nrow(chk) > 0) {
      msg("  KML buffer cross-check: mean radius ",
          paste(sprintf("%.2f", chk$kml_radius_mean), collapse = ", "), " m")
    }
  }

  # ---- 9. EXPORT ---------------------------------------------------------
  if (!is.null(out_xlsx)) {
    if (!requireNamespace("writexl", quietly = TRUE))
      stop("writexl is required to export.")
    sheets <- list(wide = wide, long = long, tanks = tank,
                   distances = dist_tbl, qc = qc, meta = meta,
                   categories = cat_map)
    if (nrow(qc_unmatched_ids) > 0) sheets$unmatched_ids <- qc_unmatched_ids
    if (!is.null(res$buffer_check)) sheets$buffer_check <- res$buffer_check
    writexl::write_xlsx(sheets, path = out_xlsx)
    msg("  written: ", out_xlsx)
  }

  invisible(res)
}

