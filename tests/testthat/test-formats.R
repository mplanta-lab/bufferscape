kml  <- function() system.file("extdata", "example_site.kml",  package = "bufferscape")
gpkg <- function() system.file("extdata", "example_site.gpkg", package = "bufferscape")

test_that("both formats are read without the user choosing", {
  skip_if_not(nzchar(kml())); skip_if_not(nzchar(gpkg()))
  a <- suppressWarnings(buffer_composition(kml(),  radii = 50, grid_res = 5,
                                           verbose = FALSE))
  b <- suppressWarnings(buffer_composition(gpkg(), radii = 50, grid_res = 5,
                                           verbose = FALSE))
  expect_equal(nrow(a$traps), 1L)
  expect_equal(nrow(b$traps), 1L)
  expect_equal(a$traps$Name, b$traps$Name)
})

test_that("the same site gives the same numbers in either format", {
  skip_if_not(nzchar(kml())); skip_if_not(nzchar(gpkg()))
  a <- suppressWarnings(buffer_composition(kml(),  radii = 50, grid_res = 5,
                                           verbose = FALSE))
  b <- suppressWarnings(buffer_composition(gpkg(), radii = 50, grid_res = 5,
                                           verbose = FALSE))
  # this is the contract: format must not change a single computed value
  expect_equal(sum(a$long$area_m2), sum(b$long$area_m2), tolerance = 1e-6)
  expect_equal(sum(a$long$area_w),  sum(b$long$area_w),  tolerance = 1e-6)
  expect_equal(a$tanks$tank_sealed, b$tanks$tank_sealed)
  expect_equal(a$tanks$tank_open,   b$tanks$tank_open)
  expect_equal(a$tanks$pool_count,  b$tanks$pool_count)
  expect_equal(sum(a$long$area_m2 > 0), sum(b$long$area_m2 > 0))
  da <- sort(round(unlist(a$distances[, -1]), 4))
  db <- sort(round(unlist(b$distances[, -1]), 4))
  expect_equal(unname(da), unname(db))
})

test_that("format detection is by content, not by extension", {
  skip_if_not(nzchar(gpkg()))
  f <- file.path(tempdir(), "renamed_without_extension")
  file.copy(gpkg(), f, overwrite = TRUE)
  r <- suppressWarnings(buffer_composition(f, radii = 50, grid_res = 5,
                                           verbose = FALSE))
  expect_equal(nrow(r$traps), 1L)
  unlink(f)
})

test_that("a mixed folder is processed in one batch", {
  skip_if_not(nzchar(kml())); skip_if_not(nzchar(gpkg()))
  skip_if_not_installed("writexl")
  d <- file.path(tempdir(), "bs_mixed"); dir.create(d, showWarnings = FALSE)
  file.copy(c(kml(), gpkg()), d, overwrite = TRUE)
  out <- suppressWarnings(suppressMessages(
    batch_composition(d, radii = 50, grid_res = 5,
                      make_maps = FALSE, make_charts = FALSE)))
  # same site id in both files, so the duplicate detector should also fire
  expect_true(nrow(out$summary) >= 1)
  expect_true(all(c("tank_sealed", "tank_open", "pool_count") %in%
                    names(out$summary)))
  unlink(d, recursive = TRUE)
})

test_that("a buffer digitised under the site's own name is not counted as cover", {
  skip_if_not(nzchar(kml()))
  # regression: buffers are sometimes named "NH_6" rather than "NH_6_Buffer".
  # Such a polygon must be held as reference geometry, not added to the
  # classified surface.
  r <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                           verbose = FALSE))
  expect_false(any(grepl("(?i)buffer", r$polygons$Name, perl = TRUE)))
  expect_true(!is.null(r$buffer_check) && nrow(r$buffer_check) > 0)
  expect_equal(r$buffer_check$kml_radius_mean[1], 50, tolerance = 0.02)
})

test_that("the deprecated argument names still work", {
  skip_if_not(nzchar(kml()))
  expect_warning(
    r <- buffer_composition(kml = kml(), radii = 50, grid_res = 5,
                            verbose = FALSE),
    "deprecated")
  expect_equal(nrow(r$traps), 1L)
  expect_error(buffer_composition(radii = 50), "required")
})

test_that("the built-in dictionary resolves without attaching the package", {
  # regression: lazy-loaded data is not on the namespace search path, so a bare
  # reference worked only after library(bufferscape).
  expect_equal(nrow(bufferscape::class_dictionary()), 28L)
})
