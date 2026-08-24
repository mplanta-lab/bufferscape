# =============================================================================
# batch_composition() : run every KML in a folder, write one workbook + all maps
# =============================================================================
#' Process every KML in a folder
#'
#' Runs [buffer_composition()] over each KML in a directory, writes one workbook
#' containing every table, one map per sampling site, and the composition and
#' container charts.
#'
#' @param dir Folder holding the `.kml` and/or `.gpkg` files.
#' @param kml_dir Deprecated name for `dir`, still accepted.
#' @param out_dir Output folder; defaults to `Results/` inside `dir`.
#' @param pattern Regular expression selecting files. The default takes both
#'   `.kml` and `.gpkg`, so a folder may mix the two.
#' @param categories A category dictionary; see [class_dictionary()].
#' @param make_maps,make_charts Produce figures.
#' @param palette Passed to [map_composition()]; `"aerial"` (default),
#'   `"colorblind"`, `"viridis"`, `"greyscale"`, or a palette table.
#' @param metrics Which surface metrics the workbook should carry; see
#'   [write_composition_report()].
#' @param label_ids Print the class id on this many of the largest polygons in
#'   each map.
#' @param basemap,map_zoom Passed to the map functions.
#' @param radii,kernel,lambda,grid_res,epsg,secondary,sec_weight Passed to
#'   [buffer_composition()].
#'
#' @details
#' A KML may hold one, two or three sampling points. Buffer polygons are named
#' per point (`VP_9_Buffer`), while land-cover polygons and water containers are
#' a shared pool for the whole file; each point takes its own elements
#' **geometrically**, against its own buffer, so per-site outputs never
#' contaminate one another.
#'
#' One malformed file does not stop the batch: it is logged and skipped. Sites
#' whose name appears in more than one file are reported in a
#' `duplicate_traps` sheet, and their figures are given distinct filenames.
#'
#' @return Invisibly, a list with `wide`, `long`, `tanks`, `distances`, `qc`,
#'   `summary`, `out_dir` and `failed`.
#'
#' @examples
#' \donttest{
#' dir.create(d <- tempfile())
#' file.copy(system.file("extdata", "example_site.kml", package = "bufferscape"), d)
#' out <- batch_composition(d, radii = 50, grid_res = 5,
#'                      make_maps = FALSE, make_charts = FALSE)
#' out$summary
#' }
#' @export
batch_composition <- function(dir = NULL,
                          out_dir     = file.path(dir, "Results"),
                          pattern    = "\\.(kml|gpkg)$",
                          make_maps   = TRUE,
                          make_charts = TRUE,
                          basemap     = "none",
                          map_zoom    = NA,
                          radii       = c(20, 30, 40, 50),
                          kernel      = "exponential",
                          lambda      = 45,
                          grid_res    = 1,
                          epsg        = 31983,
                          secondary   = "weighted",
                          sec_weight  = 0.3,
                          categories  = NULL,
                          palette     = "aerial",
                          metrics     = c("exact", "weighted", "centroid",
                                          "distance"),
                          label_ids   = 0,
                          kml_dir     = NULL) {
  if (!is.null(kml_dir)) {
    if (is.null(dir)) dir <- kml_dir
    warning("`kml_dir` is deprecated; the argument is now `dir`.", call. = FALSE)
  }
  if (is.null(dir)) stop("`dir` is required.", call. = FALSE)

  if (!dir.exists(dir))
    stop("Folder does not exist:\n  ", dir,
         "\nCheck the path in the CONFIG block (forward slashes, even on Windows).")

  files <- list.files(dir, pattern = pattern, full.names = TRUE,
                      ignore.case = TRUE)
  # never re-read our own outputs
  files <- files[!grepl("/Results/", files, fixed = TRUE)]
  if (length(files) == 0)
    stop("No .kml or .gpkg files found in:\n  ", dir)

  # ---- check the basemap BEFORE drawing 50 figures -----------------------
  if (isTRUE(make_maps) && !identical(basemap, "none") &&
      !.bs_basemap_ready(basemap)) {
    need <- if (identical(basemap, "esri")) '"maptiles", "terra"' else '"terra"'
    message("\n", strrep("!", 74), "\n")
    message("  BASEMAP is '", basemap, "' but the packages it needs are missing.\n")
    message("  Maps will be drawn WITHOUT satellite imagery.\n\n")
    message("  For imagery, stop now and run:\n")
    message("      install.packages(c(", need, "))\n\n")
    message("  then Source this script again.\n")
    message(strrep("!", 74), "\n\n")
    basemap <- "none"
  }

  map_dir   <- file.path(out_dir, "maps")
  chart_dir <- file.path(out_dir, "charts")
  dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(make_charts))
    dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(out_dir, "run_log.txt")
  logcon <- file(log_path, open = "wt")
  on.exit(close(logcon), add = TRUE)
  logmsg <- function(...) {
    line <- paste0(...)
    message(line); writeLines(line, logcon); flush(logcon)
  }

  logmsg("batch_composition  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  logmsg("folder : ", dir)
  logmsg("files  : ", length(files))
  logmsg("kernel : ", kernel, ", lambda = ", lambda, " m, radii = ",
         paste(radii, collapse = ","), " m, grid = ", grid_res, " m")
  logmsg("secondary handling: ", secondary, " (weight ", sec_weight, ")")
  logmsg("basemap: ", basemap)
  logmsg(strrep("-", 74))

  W <- L <- TK <- DI <- QC <- UM <- BC <- list()
  ok <- 0L; failed <- character(0); t_start <- Sys.time()

  for (i in seq_along(files)) {
    f <- files[i]
    nm <- tools::file_path_sans_ext(basename(f))
    message(sprintf("[%d/%d] %-28s ", i, length(files), nm))

    r <- try(suppressWarnings(
      buffer_composition(kml = f, radii = radii, kernel = kernel, lambda = lambda,
                        grid_res = grid_res, epsg = epsg, secondary = secondary,
                        secondary_weight = sec_weight, categories = categories,
                        verbose = FALSE)
    ), silent = TRUE)

    if (inherits(r, "try-error")) {
      message("FAILED\n")
      logmsg("[FAIL] ", nm, " : ", trimws(as.character(r)))
      failed <- c(failed, nm); next
    }

    W[[nm]]  <- dplyr::mutate(r$wide,      source_file = nm)
    L[[nm]]  <- dplyr::mutate(r$long,      source_file = nm)
    TK[[nm]] <- dplyr::mutate(r$tanks,     source_file = nm)
    DI[[nm]] <- dplyr::mutate(r$distances, source_file = nm)
    QC[[nm]] <- dplyr::mutate(r$qc,        source_file = nm)
    if (nrow(r$unmatched_ids) > 0)
      UM[[nm]] <- dplyr::mutate(r$unmatched_ids, file = nm)
    if (!is.null(r$buffer_check) && nrow(r$buffer_check) > 0)
      BC[[nm]] <- r$buffer_check

    cov <- if (nrow(r$qc)) sprintf("%.1f%% classified", 100 * r$qc$coverage_ratio[1]) else "no polygons"
    message(sprintf("%3d poly | %s | %2d tanks (%d unsealed)",
                if (nrow(r$qc)) r$qc$n_polygons[1] else 0L, cov,
                r$tanks$tank_total[1], r$tanks$tank_open[1]))
    logmsg("[ok]   ", nm, " : traps ", paste(r$traps$Name, collapse = ","),
           " | polygons ", if (nrow(r$qc)) r$qc$n_polygons[1] else 0L,
           " | coverage ", cov,
           " | tanks ", r$tanks$tank_total[1], " (", r$tanks$tank_open[1], " unsealed)")

    if (isTRUE(make_maps)) {
      for (tn in r$traps$Name) {
        stem <- tn
        if (file.exists(file.path(map_dir, paste0(stem, ".png")))) {
          stem <- paste0(tn, "__", nm)          # same trap name in two files
          logmsg("       NOTE ", tn, " already drawn from another file; ",
                 "this one saved as ", stem, "*.png")
        }
        # one figure per trap: the land-cover map.
        # map_basemap() and map_combined() remain available to call
        # directly if an imagery or combined figure is ever wanted.
        fp <- file.path(map_dir, paste0(stem, ".png"))
        mr <- try(suppressWarnings(map_composition(r, tn, file = fp,
                                  palette = palette, label_ids = label_ids)),
                  silent = TRUE)
        if (inherits(mr, "try-error"))
          logmsg("       map FAILED for ", tn, " : ", trimws(as.character(mr)))
        if (isTRUE(make_charts)) {
          cf <- file.path(chart_dir, paste0(stem, "_categories.png"))
          cr <- try(suppressWarnings(
            plot_site_composition(r, tn, file = cf)), silent = TRUE)
          if (inherits(cr, "try-error"))
            logmsg("       category chart FAILED for ", tn, " : ",
                   trimws(as.character(cr)))
        }
      }
    }
    ok <- ok + 1L
  }

  if (ok == 0L) stop("Every file failed. See ", log_path)

  wide <- dplyr::bind_rows(W)
  long <- dplyr::bind_rows(L)
  tank <- dplyr::bind_rows(TK)
  dist <- dplyr::bind_rows(DI)
  qc   <- dplyr::bind_rows(QC)

  # one-line-per-trap overview
  top <- long %>%
    dplyr::group_by(source_file, Ovitrap_ID, radius_m) %>%
    dplyr::slice_max(area_m2, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(source_file, Ovitrap_ID, radius_m,
                  dominant_category = full_name, dominant_area_m2 = area_m2)
  key <- c("source_file", "Ovitrap_ID", "radius_m")
  summary_tbl <- qc %>%
    dplyr::left_join(tank, by = key) %>%
    dplyr::left_join(top,  by = key) %>%
    dplyr::mutate(pct_classified = round(100 * coverage_ratio, 1)) %>%
    dplyr::select(Ovitrap_ID, source_file, radius_m, n_polygons, area_exact,
                  pct_classified, tank_sealed, tank_open, tank_total, pool_count,
                  dominant_category, dominant_area_m2) %>%
    dplyr::arrange(pct_classified)   # incomplete digitising surfaces first

  # ---- one modelling-ready sheet per radius ------------------------------
  # The largest radius is the primary analysis; the smaller ones are there for
  # scale-of-effect / sensitivity work. Each sheet carries all three surface
  # metrics per category (exact, kernel-weighted, centroid-weighted) plus the
  # container counts and the reference-feature distances.
  rad_sheets <- list()
  for (rr in sort(unique(long$radius_m), decreasing = TRUE)) {
    lw <- long[long$radius_m == rr, ]
    wa <- lw %>%
      dplyr::select(Ovitrap_ID, full_name, area_m2, area_w, area_w_centroid,
                    d_nearest_m) %>%
      tidyr::pivot_wider(names_from = full_name,
                         values_from = c(area_m2, area_w, area_w_centroid,
                                         d_nearest_m),
                         names_glue = "{full_name}_{.value}")
    tw <- tank[tank$radius_m == rr,
               c("Ovitrap_ID", "tank_sealed", "tank_open", "tank_total",
                 intersect("pool_count", names(tank)))]
    rad_sheets[[paste0("wide_", rr, "m")]] <- wa %>%
      dplyr::left_join(tw,   by = "Ovitrap_ID") %>%
      dplyr::left_join(dist, by = "Ovitrap_ID")
  }

  # short guide to what the columns mean
  metric_key <- tibble::tibble(
    column_suffix = c("_area_m2", "_area_w", "_area_w_centroid",
                      "tank_sealed", "tank_open", "pool_count", "dist_*"),
    meaning = c(
      "exact surface area of the category inside the buffer (m2)",
      "distance-decay weighted area: kernel integrated OVER the polygon (m2). Use this one.",
      "distance-decay weighted area evaluated at the polygon CENTROID (m2). For methods comparison only - biased for elongated features.",
      "sealed water tanks inside the buffer (count)",
      "unsealed / open water tanks inside the buffer (count)",
      "swimming pools inside the buffer (count)",
      "straight-line distance from the trap to an off-buffer reference feature (m); NA when not measured"),
    detail = c(
      "", 
      sprintf("kernel %s, lambda = %g m", kernel, lambda),
      sprintf("kernel %s, lambda = %g m", kernel, lambda),
      "", "", "", ""))

  sheets <- c(list(summary = summary_tbl), rad_sheets,
              list(wide = wide, long = long,
                 tanks = tank, distances = dist, qc = qc,
                 metric_key = metric_key,
                 categories = class_dictionary(categories),
                 settings = tibble::tibble(
                   key = c("run_time", "dir", "n_files", "n_ok", "n_failed",
                           "epsg", "radii_m", "kernel", "lambda_m", "grid_res_m",
                           "secondary", "secondary_weight", "basemap", "sf_version"),
                   value = c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), dir,
                             as.character(length(files)), as.character(ok),
                             as.character(length(failed)), as.character(epsg),
                             paste(radii, collapse = ","), kernel,
                             as.character(lambda), as.character(grid_res),
                             secondary, as.character(sec_weight), basemap,
                             as.character(utils::packageVersion("sf"))))))
  if (length(UM)) sheets$unmatched_ids <- dplyr::bind_rows(UM)
  if (length(BC)) sheets$buffer_check  <- dplyr::bind_rows(BC)
  if (length(failed))
    sheets$failed_files <- tibble::tibble(file = failed)

  dup <- summary_tbl %>%
    dplyr::count(Ovitrap_ID, radius_m) %>%
    dplyr::filter(n > 1)
  if (nrow(dup) > 0) {
    d_ids <- unique(dup$Ovitrap_ID)
    logmsg("[WARN] trap ID(s) appear in more than one file: ",
           paste(d_ids, collapse = ", "))
    sheets_dup <- summary_tbl[summary_tbl$Ovitrap_ID %in% d_ids,
                              c("Ovitrap_ID", "source_file", "n_polygons",
                                "pct_classified", "tank_total")]
  } else sheets_dup <- NULL
  if (!is.null(sheets_dup)) sheets$duplicate_traps <- sheets_dup

  xlsx <- file.path(out_dir, "Ovitrap_Results.xlsx")
  writexl::write_xlsx(sheets, path = xlsx)

  # ---- one container chart per community ---------------------------------
  if (isTRUE(make_charts)) {
    prim <- max(tank$radius_m)
    comms <- sort(unique(sub("_.*$", "", tank$Ovitrap_ID)))
    for (cm in comms) {
      cf <- file.path(chart_dir, paste0("containers_", cm, ".png"))
      nt <- length(unique(tank$Ovitrap_ID[sub("_.*$", "", tank$Ovitrap_ID) == cm]))
      cr <- try(suppressWarnings(plot_group_points(
        tank, cm, radius = prim, file = cf,
        width = max(8, min(20, 1.6 + 0.42 * nt)))), silent = TRUE)
      if (inherits(cr, "try-error"))
        logmsg("[WARN] container chart failed for ", cm, " : ",
               trimws(as.character(cr)))
      else logmsg("chart   : ", cf, "  (", nt, " traps)")
    }
  }

  el <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")))
  logmsg(strrep("-", 74))
  logmsg("done in ", el, " s  |  ", ok, " ok, ", length(failed), " failed")
  logmsg("workbook : ", xlsx)
  if (isTRUE(make_maps)) logmsg("maps     : ", map_dir)
  if (isTRUE(make_charts)) logmsg("charts   : ", chart_dir)
  if (length(failed)) logmsg("FAILED   : ", paste(failed, collapse = ", "))

  message("\n", strrep("=", 74), "\n")
  message("  traps processed : ", nrow(summary_tbl), "\n")
  message("  workbook        : ", xlsx, "\n")
  if (isTRUE(make_maps)) message("  maps            : ", map_dir, "\n")
  if (isTRUE(make_charts)) message("  charts          : ", chart_dir, "\n")
  message("  log             : ", log_path, "\n")
  if (length(failed))
    message("  FAILED          : ", paste(failed, collapse = ", "), "\n")
  if (nrow(dup) > 0) {
    message("\n  DUPLICATE TRAP IDs across files - check for repeated KMLs:\n")
    for (i in seq_len(nrow(dup)))
      message(sprintf("    %-12s appears %d times\n", dup$Ovitrap_ID[i], dup$n[i]))
  }
  low <- summary_tbl[summary_tbl$pct_classified < 80 &
                     summary_tbl$radius_m == max(summary_tbl$radius_m), ]
  if (nrow(low) > 0) {
    message("\n  CHECK DIGITISING - under 80% of the buffer classified:\n")
    for (i in seq_len(min(nrow(low), 12)))
      message(sprintf("    %-12s %5.1f%%\n", low$Ovitrap_ID[i], low$pct_classified[i]))
    if (nrow(low) > 12) message("    ... and ", nrow(low) - 12, " more\n")
  }
  message(strrep("=", 74), "\n")

  invisible(list(wide = wide, long = long, tanks = tank, distances = dist,
                 qc = qc, summary = summary_tbl, out_dir = out_dir,
                 failed = failed))
}


