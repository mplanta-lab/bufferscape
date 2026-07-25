#' Quantify the centroid approximation bias, polygon by polygon
#'
#' Recomputes, for every polygon intersecting every buffer, the distance-decay
#' weight two ways -- integrated over the polygon's geometry, and evaluated at
#' its centroid -- and reports the discrepancy alongside shape descriptors.
#'
#' This is the diagnostic behind the claim that centroid weighting is unsafe.
#' The bias is not uniform: it is negligible for compact features and large for
#' elongated ones that pass close to the site, because such a polygon's centroid
#' can sit almost on the sampling point while most of its area does not.
#'
#' @inheritParams buffer_composition
#' @param kml Path, or vector of paths, to KML files.
#'
#' @return A [tibble][tibble::tibble], one row per polygon per site per radius:
#' \describe{
#'   \item{`Ovitrap_ID`, `radius_m`, `source_file`}{identifiers}
#'   \item{`id_primary`, `label_en`}{class}
#'   \item{`area_m2`}{clipped area inside the buffer}
#'   \item{`d_centroid`}{distance from the site to the polygon centroid (m)}
#'   \item{`d_min`, `d_max`}{nearest and farthest distance from the site to the
#'     polygon (m)}
#'   \item{`centroid_inside`}{`FALSE` when the polygon's centroid falls outside
#'     the polygon, as happens for concave features. The centroid weight is then
#'     evaluated at a point that is not part of the feature.}
#'   \item{`compactness`}{\eqn{4 \pi A / P^2}; 1 for a circle, towards 0 as the
#'     outline becomes elongated or convoluted}
#'   \item{`d_mean`}{area-weighted mean distance from the site to the polygon
#'     (m). This is the quantity the centroid is standing in for.}
#'   \item{`centroid_offset`}{`(d_mean - d_centroid) / lambda`; how far the
#'     centroid sits from the polygon's mean distance, in kernel scale lengths.
#'     This is the mechanistic driver of the bias: a polygon whose centroid is
#'     much nearer than its typical point gets over-weighted.}
#'   \item{`reach_ratio`}{`(d_max - d_min) / (d_centroid + 1)`; the span of the
#'     polygon relative to its centroid distance.}
#'   \item{`geometry_class`}{`"compact"` or `"elongated"`, split at
#'     `compact_cut`}
#'   \item{`w_integrated`, `w_centroid`}{mean kernel weight, both ways}
#'   \item{`area_w`, `area_w_centroid`}{weighted areas}
#'   \item{`w_nearest`, `area_w_nearest`, `bias_nearest_pct`}{the same, for a
#'     weight taken at the polygon's nearest point. Included because it is the
#'     obvious alternative to the centroid; it is an upper bound on the mean
#'     weight and overestimates more than the centroid does, on both compact
#'     and elongated polygons.}
#'   \item{`bias_pct`}{\eqn{100 (area\_w\_centroid - area\_w) / area\_w}}
#' }
#'
#' @param compact_cut Compactness below which a polygon is called elongated.
#' @param trap_pattern Regular expression matching the placemark names of
#'   sampling points.
#'
#' @examples
#' kml <- system.file("extdata", "example_site.kml", package = "bufferscape")
#' b <- centroid_bias(kml, radii = 50, grid_res = 5)
#' summarise_centroid_bias(b)
#'
#' @seealso [summarise_centroid_bias()]
#' @export
centroid_bias <- function(kml,
                          categories  = NULL,
                          driver      = "KML",
                          epsg        = 31983,
                          radii       = 50,
                          kernel      = "exponential",
                          lambda      = 45,
                          grid_res    = 1,
                          compact_cut = 0.40,
                          trap_pattern = "^[A-Za-z]{2,}[_ -]?\\d+$",
                          verbose     = TRUE) {

  cat_map <- class_dictionary(categories)
  out <- list()

  for (f in kml) {
    if (isTRUE(verbose)) message("centroid_bias: ", basename(f))
    r <- try(suppressWarnings(
      buffer_composition(f, categories = categories, driver = driver,
                         epsg = epsg, radii = radii, kernel = kernel,
                         lambda = lambda, grid_res = grid_res,
                         trap_pattern = trap_pattern,
                         check_kml_buffer = FALSE, verbose = FALSE)),
      silent = TRUE)
    if (inherits(r, "try-error") || nrow(r$polygons) == 0) next

    for (tn in r$traps$Name) {
      trap_i <- r$traps[r$traps$Name == tn, ][1, ]
      for (rad in radii) {
        buf  <- sf::st_buffer(trap_i, rad)
        near <- r$polygons[lengths(sf::st_intersects(r$polygons, buf)) > 0, ]
        if (nrow(near) == 0) next
        clip <- suppressWarnings(sf::st_intersection(near, buf))
        clip <- clip[!sf::st_is_empty(clip), ]
        if (nrow(clip) == 0) next

        gpts <- sf::st_make_grid(buf, cellsize = grid_res, what = "centers")
        gpts <- gpts[lengths(sf::st_intersects(gpts, buf)) > 0]
        gd   <- as.numeric(sf::st_distance(gpts, trap_i))
        gw   <- decay_kernel(gd, kernel, lambda)

        hits <- sf::st_intersects(clip, gpts)
        w_int <- vapply(seq_along(hits), function(i) {
          h <- hits[[i]]
          if (length(h) > 0) mean(gw[h]) else NA_real_
        }, numeric(1))
        # mean distance over the polygon's area, which is what the centroid is
        # standing in for. Where the two diverge, the centroid weight is wrong.
        d_bar <- vapply(seq_along(hits), function(i) {
          h <- hits[[i]]
          if (length(h) > 0) mean(gd[h]) else NA_real_
        }, numeric(1))

        gm  <- sf::st_geometry(clip)
        ctr <- suppressWarnings(sf::st_centroid(gm))
        d_c <- as.numeric(sf::st_distance(ctr, trap_i))
        d_min <- as.numeric(sf::st_distance(gm, trap_i))
        vtx <- lapply(seq_along(gm), function(i) {
          v <- suppressWarnings(sf::st_cast(gm[i], "POINT"))
          max(as.numeric(sf::st_distance(v, trap_i)))
        })
        d_max <- unlist(vtx)

        # A concave feature -- a road bending around the site, a river meander,
        # an L-shaped block -- can have its centroid fall OUTSIDE itself. The
        # centroid weight is then evaluated at a location that is not part of
        # the feature at all, which is not an approximation but a category
        # error. Flagged rather than silently averaged in.
        inside <- as.logical(lengths(sf::st_intersects(ctr, gm, sparse = TRUE)) > 0)
        inside <- vapply(seq_along(gm), function(i)
          lengths(sf::st_intersects(ctr[i], gm[i])) > 0, logical(1))

        A <- as.numeric(sf::st_area(clip))
        P <- as.numeric(sf::st_length(sf::st_cast(gm, "MULTILINESTRING",
                                                  warn = FALSE)))
        comp <- ifelse(P > 0, 4 * pi * A / P^2, NA_real_)
        w_ct <- decay_kernel(d_c, kernel, lambda)
        # a third estimator, for the three-way comparison: the weight at the
        # polygon's NEAREST point. Intuitively appealing, but it is an upper
        # bound on the true mean weight and so overestimates systematically.
        w_nr <- decay_kernel(d_min, kernel, lambda)

        keep <- !is.na(w_int) & A > 0
        if (!any(keep)) next
        lab <- cat_map$label_en[match(clip$id_primary, cat_map$id)]

        out[[length(out) + 1]] <- tibble::tibble(
          source_file = tools::file_path_sans_ext(basename(f)),
          Ovitrap_ID = tn, radius_m = rad,
          id_primary = clip$id_primary[keep],
          label_en = lab[keep],
          area_m2 = A[keep],
          d_centroid = d_c[keep], d_min = d_min[keep], d_max = d_max[keep],
          centroid_inside = inside[keep],
          compactness = comp[keep],
          d_mean = d_bar[keep],
          # the mechanistic driver: how far the centroid sits from the
          # polygon's mean distance, expressed in kernel scale lengths
          centroid_offset = (d_bar[keep] - d_c[keep]) / lambda,
          reach_ratio = (d_max[keep] - d_min[keep]) / (d_c[keep] + 1),
          w_integrated = w_int[keep], w_centroid = w_ct[keep],
          w_nearest = w_nr[keep],
          area_w = A[keep] * w_int[keep],
          area_w_centroid = A[keep] * w_ct[keep],
          area_w_nearest = A[keep] * w_nr[keep]
        )
      }
    }
  }

  if (!length(out))
    stop("No polygons found in any buffer.", call. = FALSE)

  res <- dplyr::bind_rows(out)
  res$bias_pct <- 100 * (res$area_w_centroid - res$area_w) / res$area_w
  res$bias_nearest_pct <- 100 * (res$area_w_nearest - res$area_w) / res$area_w
  res$geometry_class <- ifelse(res$compactness < compact_cut,
                               "elongated", "compact")
  res
}

#' Summarise centroid bias
#'
#' Collapses [centroid_bias()] output into the table a methods paper needs:
#' the distribution of the discrepancy, split by polygon geometry, and the worst
#' offenders by class.
#'
#' @param x Output of [centroid_bias()].
#' @param by Grouping: `"geometry"` (default), `"class"` or `"site"`.
#' @param area_weighted If `TRUE`, weight each polygon's bias by its area, so
#'   the summary reflects the effect on the covariate a model would actually
#'   see rather than treating a 2 m2 sliver like a 2000 m2 block.
#' @return A [tibble][tibble::tibble].
#' @examples
#' kml <- system.file("extdata", "example_site.kml", package = "bufferscape")
#' b <- centroid_bias(kml, radii = 50, grid_res = 5)
#' summarise_centroid_bias(b, by = "class")
#' @export
summarise_centroid_bias <- function(x, by = c("geometry", "class", "site"),
                                    area_weighted = FALSE) {
  by <- match.arg(by)
  g <- switch(by,
              geometry = "geometry_class",
              class    = "label_en",
              site     = "Ovitrap_ID")
  sp <- split(x, x[[g]])
  wm <- function(v, w) if (area_weighted) stats::weighted.mean(v, w) else mean(v)

  res <- do.call(rbind, lapply(names(sp), function(k) {
    d <- sp[[k]]
    q <- stats::quantile(d$bias_pct, c(0.5, 0.25, 0.75, 0.95, 1),
                         na.rm = TRUE, names = FALSE)
    tibble::tibble(
      group = k,
      n_polygons = nrow(d),
      total_area_m2 = sum(d$area_m2),
      median_bias_pct = q[1],
      q25 = q[2], q75 = q[3], p95 = q[4], max_bias_pct = q[5],
      mean_bias_pct = wm(d$bias_pct, d$area_m2),
      pct_over_10 = 100 * mean(abs(d$bias_pct) > 10, na.rm = TRUE)
    )
  }))
  res[order(-res$median_bias_pct), ]
}
