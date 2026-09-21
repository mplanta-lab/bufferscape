sq <- function(cx, cy, w) sf::st_polygon(list(rbind(
  c(cx - w/2, cy - w/2), c(cx + w/2, cy - w/2), c(cx + w/2, cy + w/2),
  c(cx - w/2, cy + w/2), c(cx - w/2, cy - w/2))))
mk <- function(geoms, desc) sf::st_sf(Name = "p", Description = desc,
                                      geometry = sf::st_sfc(geoms, crs = 31983))

test_that("genuine overlap is never treated as duplication", {
  skip_if_not_installed("sf")
  # a tree crown over a roof is two real surfaces at the same place; this is
  # the case the duplicate check must not touch
  x <- mk(list(sq(0, 0, 20), sq(3, 3, 8)), c("7", "3"))
  expect_equal(nrow(bufferscape:::.bs_drop_duplicate_polygons(x)), 2L)
})

test_that("identical geometry is collapsed to one polygon", {
  skip_if_not_installed("sf")
  x <- mk(list(sq(0, 0, 20), sq(0, 0, 20)), c("7", "7"))
  expect_warning(r <- bufferscape:::.bs_drop_duplicate_polygons(x),
                 "digitised twice")
  expect_equal(nrow(r), 1L)
})

test_that("when only one copy carries a class code, that copy survives", {
  skip_if_not_installed("sf")
  x <- mk(list(sq(0, 0, 20), sq(0, 0, 20)), c("", "7"))
  expect_warning(r <- bufferscape:::.bs_drop_duplicate_polygons(x))
  expect_equal(nrow(r), 1L)
  expect_equal(trimws(r$Description), "7")
})

test_that("conflicting class codes on the same geometry are named", {
  skip_if_not_installed("sf")
  x <- mk(list(sq(0, 0, 20), sq(0, 0, 20)), c("7", "6"))
  expect_warning(bufferscape:::.bs_drop_duplicate_polygons(x), "conflicting")
})

test_that("a single polygon or an empty layer is returned untouched", {
  skip_if_not_installed("sf")
  x <- mk(list(sq(0, 0, 20)), "7")
  expect_equal(nrow(bufferscape:::.bs_drop_duplicate_polygons(x)), 1L)
  expect_null(bufferscape:::.bs_drop_duplicate_polygons(NULL))
})

test_that("coverage far above 100% is flagged", {
  skip_if_not(nzchar(system.file("extdata", "example_site.kml",
                                 package = "bufferscape")))
  r <- suppressWarnings(buffer_composition(
    system.file("extdata", "example_site.kml", package = "bufferscape"),
    radii = 50, grid_res = 5, verbose = FALSE))
  expect_true("coverage_flag" %in% names(r$qc))
  # overlap is legitimate, so the flag must only fire well above 100%
  expect_equal(nzchar(r$qc$coverage_flag), r$qc$coverage_ratio > 1.20)
})
