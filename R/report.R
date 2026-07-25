#' Write a composition workbook
#'
#' Exports the tables from [buffer_composition()] or [batch_composition()] to a
#' multi-sheet `.xlsx` file, with control over which radii, which surface
#' metrics and which sheets are included.
#'
#' Exposed as its own function so that a workbook can be produced from a single
#' `buffer_composition()` call, re-exported with different content without
#' recomputing, or trimmed before sharing.
#'
#' @param x The list returned by [buffer_composition()] or
#'   [batch_composition()].
#' @param path Output `.xlsx` path.
#' @param radii Radii to include. `NULL` (default) includes every radius
#'   present. The largest is treated as primary and its sheet is written first.
#' @param metrics Which per-class surface metrics to export, any of:
#'   \describe{
#'     \item{`"exact"`}{`_area_m2`, the exact area inside the buffer}
#'     \item{`"weighted"`}{`_area_w`, kernel integrated over polygon geometry --
#'       the metric intended for modelling}
#'     \item{`"centroid"`}{`_area_w_centroid`, kernel at the polygon centroid,
#'       for methods comparison}
#'     \item{`"distance"`}{`_d_nearest_m`, distance to the nearest patch of the
#'       class, `NA` when absent}
#'   }
#'   Defaults to all four. Dropping `"centroid"` roughly halves the width of a
#'   wide sheet if the methods comparison is not needed.
#' @param per_radius Write one modelling-ready sheet per radius
#'   (`wide_50m`, `wide_30m`, ...).
#' @param combined Write the single `wide` sheet holding every radius at once.
#' @param long Write the tidy site x class x radius table.
#' @param extras Write the supporting sheets: `tanks`, `distances`, `qc`,
#'   `categories`, `settings`, and any diagnostic sheets present
#'   (`unmatched_ids`, `failed_files`, `duplicate_traps`, `buffer_check`).
#' @param key Write the `metric_key` sheet explaining every column suffix.
#' @param labels Class naming in the wide sheets: `"full_name"` (default,
#'   `id_category_description`, stable and machine-friendly) or `"label_en"`
#'   (readable, but changes if the dictionary's labels are edited).
#' @param digits Round numeric columns to this many decimals. `NULL` leaves
#'   full precision.
#'
#' @return Invisibly, the named list of data frames that was written.
#'
#' @examples
#' \donttest{
#' kml <- system.file("extdata", "example_site.kml", package = "bufferscape")
#' res <- buffer_composition(kml, radii = c(30, 50), grid_res = 5,
#'                           verbose = FALSE)
#' out <- file.path(tempdir(), "composition.xlsx")
#' if (requireNamespace("writexl", quietly = TRUE))
#'   write_composition_report(res, out, metrics = c("exact", "weighted"))
#' }
#' @seealso [buffer_composition()], [batch_composition()]
#' @export
write_composition_report <- function(x, path,
                                     radii = NULL,
                                     metrics = c("exact", "weighted",
                                                 "centroid", "distance"),
                                     per_radius = TRUE,
                                     combined = TRUE,
                                     long = TRUE,
                                     extras = TRUE,
                                     key = TRUE,
                                     labels = c("full_name", "label_en"),
                                     digits = NULL) {

  if (!requireNamespace("writexl", quietly = TRUE))
    stop("writexl is needed to write a workbook: install.packages(\"writexl\")",
         call. = FALSE)
  if (!is.list(x) || is.null(x$long))
    stop("`x` must be the list returned by buffer_composition() or ",
         "batch_composition().", call. = FALSE)

  metrics <- match.arg(metrics, c("exact", "weighted", "centroid", "distance"),
                       several.ok = TRUE)
  labels  <- match.arg(labels)

  cols <- c(exact = "area_m2", weighted = "area_w",
            centroid = "area_w_centroid", distance = "d_nearest_m")[metrics]
  cols <- unname(cols[cols %in% names(x$long)])
  if (!length(cols))
    stop("None of the requested metrics are present in `x$long`.", call. = FALSE)

  L <- x$long
  if (!is.null(radii)) {
    miss <- setdiff(radii, unique(L$radius_m))
    if (length(miss))
      stop("Radii not present in the result: ", paste(miss, collapse = ", "),
           ". Available: ", paste(sort(unique(L$radius_m)), collapse = ", "),
           call. = FALSE)
    L <- L[L$radius_m %in% radii, ]
  }
  rads <- sort(unique(L$radius_m), decreasing = TRUE)

  namecol <- if (labels == "label_en" && "label_en" %in% names(L))
    "label_en" else "full_name"
  L$.nm <- make.names(L[[namecol]], unique = FALSE)

  tk <- x$tanks
  if (!is.null(tk) && !is.null(radii)) tk <- tk[tk$radius_m %in% radii, ]

  widen <- function(d) {
    w <- d[, c("Ovitrap_ID", ".nm", cols)] %>%
      tidyr::pivot_wider(names_from = ".nm", values_from = dplyr::all_of(cols),
                         names_glue = "{.nm}_{.value}")
    if (!is.null(tk)) {
      keep <- intersect(c("Ovitrap_ID", "tank_sealed", "tank_open",
                          "tank_total", "pool_count"), names(tk))
      tw <- unique(tk[tk$radius_m == d$radius_m[1], keep, drop = FALSE])
      w <- dplyr::left_join(w, tw, by = "Ovitrap_ID")
    }
    if (!is.null(x$distances))
      w <- dplyr::left_join(w, x$distances, by = "Ovitrap_ID")
    w
  }

  sheets <- list()
  if (!is.null(x$summary)) {
    s <- x$summary
    if (!is.null(radii)) s <- s[s$radius_m %in% radii, ]
    sheets$summary <- s
  }
  if (isTRUE(per_radius))
    for (rr in rads)
      sheets[[paste0("wide_", rr, "m")]] <- widen(L[L$radius_m == rr, ])

  if (isTRUE(combined) && length(rads) > 0) {
    wc <- L[, c("Ovitrap_ID", "radius_m", ".nm", cols)] %>%
      tidyr::pivot_wider(names_from = c(".nm", "radius_m"),
                         values_from = dplyr::all_of(cols),
                         names_glue = "{.nm}_{.value}_{radius_m}m")
    sheets$wide <- wc
  }
  if (isTRUE(long)) sheets$long <- L[, setdiff(names(L), ".nm")]

  if (isTRUE(key)) {
    kk <- data.frame(
      column_suffix = c("_area_m2", "_area_w", "_area_w_centroid",
                        "_d_nearest_m", "tank_sealed", "tank_open",
                        "pool_count", "dist_*"),
      meaning = c(
        "exact surface area of the class inside the buffer (m2)",
        "distance-decay weighted area, kernel integrated OVER the polygon (m2). Use this for modelling.",
        "distance-decay weighted area, kernel evaluated at the polygon CENTROID (m2). Methods comparison only: biased for elongated features passing close to the site.",
        "distance from the site to the nearest patch of the class (m). NA when the class is absent, never 0.",
        "sealed water tanks inside the buffer (count)",
        "unsealed / open water tanks inside the buffer (count)",
        "swimming pools inside the buffer (count)",
        "straight-line distance from the site to an off-buffer reference feature (m); NA when not measured"),
      stringsAsFactors = FALSE)
    kk <- kk[c(cols %in% "area_m2", cols %in% "area_w",
               cols %in% "area_w_centroid", cols %in% "d_nearest_m",
               TRUE, TRUE, TRUE, TRUE), ]
    sheets$metric_key <- kk
  }

  if (isTRUE(extras)) {
    for (nm in c("tanks", "distances", "qc", "categories", "settings", "meta",
                 "unmatched_ids", "failed_files", "duplicate_traps",
                 "buffer_check")) {
      d <- x[[nm]]
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) next
      if (!is.null(radii) && "radius_m" %in% names(d))
        d <- d[d$radius_m %in% radii, ]
      sheets[[nm]] <- d
    }
  }

  if (!is.null(digits))
    sheets <- lapply(sheets, function(d) {
      num <- vapply(d, is.numeric, logical(1))
      d[num] <- lapply(d[num], round, digits = digits)
      d
    })

  writexl::write_xlsx(sheets, path = path)
  message("workbook written: ", path, "  (", length(sheets), " sheets)")
  invisible(sheets)
}
