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
