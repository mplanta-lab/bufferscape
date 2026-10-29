kml <- function() system.file("extdata", "example_site.kml", package = "bufferscape")

skip_no_writexl <- function()
  skip_if_not_installed("writexl")

test_that("a workbook is written with the expected sheets", {
  skip_no_writexl(); skip_if_not(nzchar(kml()))
  res <- suppressWarnings(buffer_composition(kml(), radii = c(30, 50),
                                             grid_res = 5, verbose = FALSE))
  f <- tempfile(fileext = ".xlsx")
  sh <- write_composition_report(res, f)
  expect_true(file.exists(f))
  expect_true(all(c("wide_50m", "wide_30m", "wide", "long", "metric_key")
                  %in% names(sh)))
  # largest radius first
  expect_equal(which(names(sh) == "wide_50m") < which(names(sh) == "wide_30m"),
               TRUE)
})

test_that("metric selection controls the columns written", {
  skip_no_writexl(); skip_if_not(nzchar(kml()))
  res <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                             verbose = FALSE))
  full <- write_composition_report(res, tempfile(fileext = ".xlsx"))
  slim <- write_composition_report(res, tempfile(fileext = ".xlsx"),
                                   metrics = c("exact", "weighted"))
  nf <- names(full$wide_50m); ns <- names(slim$wide_50m)
  expect_gt(sum(grepl("_area_w_centroid$", nf)), 0)
  expect_equal(sum(grepl("_area_w_centroid$", ns)), 0)
  expect_equal(sum(grepl("_d_nearest_m$", ns)), 0)
  expect_gt(sum(grepl("_area_w$", ns)), 0)
  expect_lt(ncol(slim$wide_50m), ncol(full$wide_50m))
})

test_that("sheet switches and radius subsetting work", {
  skip_no_writexl(); skip_if_not(nzchar(kml()))
  res <- suppressWarnings(buffer_composition(kml(), radii = c(30, 50),
                                             grid_res = 5, verbose = FALSE))
  sh <- write_composition_report(res, tempfile(fileext = ".xlsx"),
                                 radii = 50, combined = FALSE, long = FALSE,
                                 extras = FALSE, key = FALSE)
  expect_true("wide_50m" %in% names(sh))
  expect_false("wide_30m" %in% names(sh))
  expect_false("wide" %in% names(sh))
  expect_false("long" %in% names(sh))
  expect_false("metric_key" %in% names(sh))
})

test_that("rounding and bad input are handled", {
  skip_no_writexl(); skip_if_not(nzchar(kml()))
  res <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                             verbose = FALSE))
  sh <- write_composition_report(res, tempfile(fileext = ".xlsx"), digits = 1)
  a <- sh$long$area_m2[sh$long$area_m2 > 0][1]
  expect_equal(a, round(a, 1))
  expect_error(write_composition_report(list(), tempfile()), "must be the list")
  expect_error(write_composition_report(res, tempfile(), radii = 999),
               "not present")
})

test_that("a basemap request does not crash the batch runner", {
  skip_if_not(nzchar(kml()))
  # regression: batch_composition() called a helper that had been lost in a
  # refactor, so any run with make_maps = TRUE and a basemap failed with
  # "could not find function basemap_ready".
  expect_true(bufferscape:::.bs_basemap_ready("none"))
  expect_type(bufferscape:::.bs_basemap_ready("esri"), "logical")

  d <- file.path(tempdir(), "bs_basemap_test")
  dir.create(d, showWarnings = FALSE)
  file.copy(kml(), d, overwrite = TRUE)
  expect_no_error(
    suppressWarnings(suppressMessages(
      batch_composition(d, radii = 50, grid_res = 5, basemap = "esri",
                        make_maps = TRUE, make_charts = FALSE)))
  )
  unlink(d, recursive = TRUE)
})

test_that("the batch log reports every site at the primary radius", {
  skip_no_writexl(); skip_if_not(nzchar(kml()))
  # a second sampling point in the same file, about 40 m from the first
  x  <- readLines(kml(), warn = FALSE)
  p1 <- paste0("<Placemark><name>SITE_1</name><Point><coordinates>",
               "-43.50000000,-23.00000000,0</coordinates></Point></Placemark>")
  p2 <- sub("-43.50000000,-23.00000000", "-43.49970000,-22.99980000",
            sub("SITE_1", "SITE_2", p1, fixed = TRUE), fixed = TRUE)
  two <- sub(p1, paste0(p1, p2), x, fixed = TRUE)
  skip_if(identical(two, x), "example file layout changed")

  d <- file.path(tempdir(), "bs_log_test")
  unlink(d, recursive = TRUE); dir.create(d)
  writeLines(two, file.path(d, "SITE_1eSITE_2.kml"))
  suppressWarnings(suppressMessages(
    batch_composition(d, radii = c(20, 50), grid_res = 5,
                      make_maps = FALSE, make_charts = FALSE)))
  log <- readLines(file.path(d, "Results", "run_log.txt"))
  ok  <- grep("^\\[ok\\]", log, value = TRUE)
  expect_length(ok, 1)
  # both sites, at 50 m rather than the first site at 20 m
  expect_match(ok, "@ 50 m")
  expect_match(ok, "SITE_1 [0-9.]+% classified")
  expect_match(ok, "SITE_2 [0-9.]+% classified")
  # the closing line counts files, not the workbook path
  expect_true(any(grepl("1 ok, 0 failed", log, fixed = TRUE)))
  unlink(d, recursive = TRUE)
})
