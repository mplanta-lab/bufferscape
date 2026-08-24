#' Read digitised site data from KML or GeoPackage
#'
#' Both formats describe the same thing -- sampling points, their buffers, the
#' land-cover polygons around them, water containers, and lines to off-buffer
#' reference features -- but they describe it very differently. This function
#' hides that difference: it detects the format, reads it, and returns one
#' normalised structure, so nothing downstream needs to know which was used.
#'
#' A file may hold one site or several. In a GeoPackage the sites are separated
#' by the `site_id` column; in a KML they are separated by placemark naming.
#' Either way the caller receives the same thing.
#'
#' @param f Path to a `.kml` or `.gpkg` file.
#' @param driver GDAL driver for flat formats. `"KML"` is forced by default
#'   because the LIBKML driver overwrites `<name>`.
#' @param trap_pattern,tank_pattern,tank_open_pattern,pool_pattern Regular
#'   expressions used only for flat formats such as KML, where features are
#'   identified by their names.
#'
#' @return A list with `traps`, `polys`, `tanks`, `pools`, `lines`,
#'   `buffer_ref`, `unmatched`, `dict` and `format`. `polys$Description` always
#'   carries the class code, `tanks$open` is always a logical, and `lines$Name`
#'   is always `site_poitype`, whatever the source format.
#'
#' @keywords internal
#' @noRd
.bs_read_site_file <- function(f,
                               driver = "KML",
                               trap_pattern = "^[A-Za-z]{2,}[_ -]?\\d+$",
                               tank_pattern = "tank|caixa|reservat",
                               tank_open_pattern = "open|abert|descoberta|sem[_ ]?tampa",
                               pool_pattern = "pool|piscina|swimming") {

  lyr <- try(sf::st_layers(f, do_count = FALSE)$name, silent = TRUE)
  if (inherits(lyr, "try-error"))
    stop("Could not open '", basename(f), "' as a spatial file.", call. = FALSE)

  # The structured schema is recognised by its layers, not by the file
  # extension, so a GeoPackage written by some other tool still falls back to
  # the flat reader rather than failing.
  structured <- all(c("landcover", "focal_points") %in% lyr)

  if (structured) .bs_read_structured(f, lyr)
  else .bs_read_flat(f, driver, trap_pattern, tank_pattern,
                     tank_open_pattern, pool_pattern)
}

#' @keywords internal
#' @noRd
.bs_read_structured <- function(f, lyr) {
  rd <- function(L) {
    if (!L %in% lyr) return(NULL)
    x <- try(suppressWarnings(sf::st_read(f, layer = L, quiet = TRUE)),
             silent = TRUE)
    if (inherits(x, "try-error") || nrow(x) == 0) return(NULL)
    sf::st_zm(x, drop = TRUE, what = "ZM")
  }
  gname <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(x)
    names(x)[names(x) == attr(x, "sf_column")] <- "geometry"
    sf::st_geometry(x) <- "geometry"
    x
  }

  fp <- gname(rd("focal_points"))
  if (is.null(fp))
    stop("'", basename(f), "' has no usable focal_points layer.", call. = FALSE)
  # `name` is the site's own label; fall back to site_id when absent
  fp$Name <- if ("name" %in% names(fp) && !all(is.na(fp$name)))
    as.character(fp$name) else as.character(fp$site_id)
  fp$Description <- NA_character_

  lc <- gname(rd("landcover"))
  if (!is.null(lc)) {
    # class_code already holds the dual notation ("6", "5-7"), so the same
    # parser used for KML descriptions works unchanged.
    code <- if ("class_code" %in% names(lc)) as.character(lc$class_code) else NA_character_
    if (all(is.na(code)) && "class_code_primary" %in% names(lc)) {
      sec <- if ("class_code_secondary" %in% names(lc)) lc$class_code_secondary else NA
      code <- ifelse(is.na(sec), as.character(lc$class_code_primary),
                     paste0(lc$class_code_primary, "-", sec))
    }
    lc$Description <- code
    lc$Name <- if ("source_name" %in% names(lc)) as.character(lc$source_name) else "landcover"
    lc$site_id <- if ("site_id" %in% names(lc)) as.character(lc$site_id) else NA_character_
  }

  wc <- gname(rd("water_containers"))
  tanks <- pools <- NULL
  if (!is.null(wc)) {
    ct <- tolower(as.character(wc$container_type))
    wc$Name <- if ("source_name" %in% names(wc)) as.character(wc$source_name) else ct
    wc$Description <- NA_character_
    pools <- wc[ct %in% c("pool", "piscina", "swimmingpool"), ]
    tanks <- wc[ct %in% c("sealed", "unsealed", "open"), ]
    if (nrow(tanks) > 0)
      tanks$open <- tolower(as.character(tanks$container_type)) %in%
        c("unsealed", "open")
    if (!is.null(pools) && nrow(pools) == 0) pools <- NULL
    if (!is.null(tanks) && nrow(tanks) == 0) tanks <- NULL
  }

  pl <- gname(rd("poi_lines"))
  if (!is.null(pl)) {
    # rebuild the "site_poitype" name the distance step expects
    pl$Name <- paste0(as.character(pl$site_id), "_", as.character(pl$poi_type))
    pl$Description <- NA_character_
  }

  bf <- gname(rd("buffers"))
  if (!is.null(bf)) {
    bf$Name <- paste0(as.character(bf$site_id), "_Buffer")
    bf$Description <- NA_character_
  }

  dict <- NULL
  if ("class_dictionary" %in% lyr) {
    d <- try(suppressWarnings(sf::st_read(f, layer = "class_dictionary",
                                          quiet = TRUE)), silent = TRUE)
    if (!inherits(d, "try-error") && nrow(d) > 0) dict <- as.data.frame(d)
  }

  list(traps = fp, polys = lc, tanks = tanks, pools = pools, lines = pl,
       buffer_ref = bf, unmatched = NULL, dict = dict, format = "structured")
}

#' @keywords internal
#' @noRd
.bs_read_flat <- function(f, driver, trap_pattern, tank_pattern,
                          tank_open_pattern, pool_pattern) {
  layers <- sf::st_layers(f, do_count = FALSE)$name
  feats <- purrr::map_dfr(layers, function(L) {
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
  if (nrow(feats) == 0)
    stop("No features found in '", basename(f), "'.", call. = FALSE)

  feats <- sf::st_zm(feats, drop = TRUE, what = "ZM")
  names(feats)[names(feats) == attr(feats, "sf_column")] <- "geometry"
  sf::st_geometry(feats) <- "geometry"

  gtype   <- as.character(sf::st_geometry_type(feats))
  is_pt   <- gtype == "POINT"
  is_poly <- gtype %in% c("POLYGON", "MULTIPOLYGON")
  is_line <- gtype %in% c("LINESTRING", "MULTILINESTRING")
  keep <- function(v) which(v %in% TRUE)

  traps <- feats[keep(is_pt & stringr::str_detect(feats$Name, trap_pattern)), ]
  if (nrow(traps) == 0)
    stop("No sampling points in '", basename(f),
         "' matched `trap_pattern` (", trap_pattern, ").", call. = FALSE)
  trap_names <- traps$Name

  # A buffer may be named "VP_21_Buffer", but it is just as often digitised
  # under the site's own name. Treating a polygon that carries a site name as
  # reference geometry catches both, and stops a 50 m circle being counted as
  # classified surface.
  is_buf <- stringr::str_detect(feats$Name, "(?i)buffer") |
            (is_poly & feats$Name %in% trap_names)
  buffer_ref <- feats[keep(is_poly &  is_buf), ]
  polys      <- feats[keep(is_poly & !is_buf), ]
  lines      <- feats[keep(is_line), ]

  other_pts <- feats[keep(is_pt & !(feats$Name %in% trap_names)), ]
  is_pool <- stringr::str_detect(other_pts$Name,
                                 stringr::regex(pool_pattern, ignore_case = TRUE))
  is_tank <- !is_pool &
    stringr::str_detect(other_pts$Name,
                        stringr::regex(tank_pattern, ignore_case = TRUE))
  pools <- other_pts[keep(is_pool), ]
  tanks <- other_pts[keep(is_tank), ]
  unmatched <- other_pts[keep(!is_pool & !is_tank), ]
  if (nrow(tanks) > 0)
    tanks$open <- stringr::str_detect(
      tanks$Name, stringr::regex(tank_open_pattern, ignore_case = TRUE))

  nz <- function(x) if (is.null(x) || nrow(x) == 0) NULL else x
  list(traps = traps, polys = nz(polys), tanks = nz(tanks), pools = nz(pools),
       lines = nz(lines), buffer_ref = nz(buffer_ref), unmatched = nz(unmatched),
       dict = NULL, format = "flat")
}

#' Combine reads from several files
#'
#' @keywords internal
#' @noRd
.bs_bind_reads <- function(reads) {
  pick <- function(nm) {
    parts <- lapply(reads, function(z) z[[nm]])
    parts <- parts[!vapply(parts, is.null, logical(1))]
    parts <- parts[vapply(parts, nrow, integer(1)) > 0]
    if (!length(parts)) return(NULL)
    # keep only the columns every part shares, so a structured and a flat file
    # can be read in the same batch
    common <- Reduce(intersect, lapply(parts, names))
    must <- intersect(c("Name", "Description", "open", "site_id"), common)
    common <- unique(c(must, common))
    parts <- lapply(parts, function(p) p[, common])
    out <- do.call(rbind, parts)
    out
  }
  list(traps = pick("traps"), polys = pick("polys"), tanks = pick("tanks"),
       pools = pick("pools"), lines = pick("lines"),
       buffer_ref = pick("buffer_ref"), unmatched = pick("unmatched"),
       dict = { d <- Filter(Negate(is.null), lapply(reads, function(z) z$dict))
                if (length(d)) d[[1]] else NULL },
       format = paste(unique(vapply(reads, function(z) z$format, character(1))),
                      collapse = "+"))
}
