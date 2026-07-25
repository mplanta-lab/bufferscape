kml <- function() system.file("extdata", "example_site.kml", package = "bufferscape")

test_that("a real KML is read and summarised", {
  skip_if_not(nzchar(kml()))
  r <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                          verbose = FALSE))
  expect_named(r$tanks, c("Ovitrap_ID", "radius_m", "tank_open", "tank_sealed",
                          "tank_total", "pool_count"))
  expect_equal(nrow(r$traps), 1L)
  expect_true(nrow(r$polygons) > 0)
  expect_true(all(r$long$area_m2 >= 0))
})

test_that("the buffer polygon in the file is excluded from land cover", {
  skip_if_not(nzchar(kml()))
  r <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                          verbose = FALSE))
  # a *_Buffer polygon must never be read as a classified surface
  expect_false(any(grepl("(?i)buffer", r$polygons$Name)))
  # and the cross-check should recover a 50 m radius
  expect_true(!is.null(r$buffer_check))
  expect_equal(r$buffer_check$kml_radius_mean[1], 50, tolerance = 0.01)
})

test_that("weighted area is bounded by exact area and shrinks with distance", {
  skip_if_not(nzchar(kml()))
  r <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                          verbose = FALSE))
  d <- r$long[r$long$area_m2 > 0, ]
  expect_true(all(d$area_w <= d$area_m2 + 1e-6))
  expect_true(all(d$area_w > 0))
  # kernel = "none" must return the exact areas
  r0 <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                           kernel = "none", verbose = FALSE))
  d0 <- r0$long[r0$long$area_m2 > 0, ]
  expect_equal(d0$area_w, d0$area_m2, tolerance = 1e-6)
})

test_that("the decay ratio falls as the buffer widens", {
  skip_if_not(nzchar(kml()))
  r <- suppressWarnings(buffer_composition(kml(), radii = c(20, 50), grid_res = 5,
                                          verbose = FALSE))
  ratio <- vapply(c(20, 50), function(rr) {
    d <- r$long[r$long$radius_m == rr & r$long$area_m2 > 0, ]
    sum(d$area_w) / sum(d$area_m2)
  }, numeric(1))
  # a wider buffer puts proportionally more surface far from the trap
  expect_true(ratio[2] < ratio[1])
})

test_that("smaller radii are nested inside larger ones", {
  skip_if_not(nzchar(kml()))
  r <- suppressWarnings(buffer_composition(kml(), radii = c(20, 30, 50),
                                          grid_res = 5, verbose = FALSE))
  tot <- vapply(c(20, 30, 50), function(rr)
    sum(r$long$area_m2[r$long$radius_m == rr]), numeric(1))
  expect_true(all(diff(tot) > 0))
  tk <- r$tanks[order(r$tanks$radius_m), ]
  expect_true(all(diff(tk$tank_total) >= 0))
})

test_that("secondary codes are split, kept or duplicated as asked", {
  skip_if_not(nzchar(kml()))
  a <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                          secondary = "primary", verbose = FALSE))
  b <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                          secondary = "both", verbose = FALSE))
  # "both" assigns the full area to each of the two codes, so the total rises
  expect_gt(sum(b$long$area_m2), sum(a$long$area_m2))
  expect_error(buffer_composition(kml(), secondary_weight = 2, verbose = FALSE),
               "between 0 and 1")
})

test_that("a geographic CRS is refused", {
  skip_if_not(nzchar(kml()))
  expect_error(
    suppressWarnings(buffer_composition(kml(), epsg = 4326, verbose = FALSE)),
    "projected CRS")
})

test_that("distances that were not measured stay NA", {
  skip_if_not(nzchar(kml()))
  r <- suppressWarnings(buffer_composition(kml(), radii = 50, grid_res = 5,
                                          verbose = FALSE))
  num <- r$distances[, setdiff(names(r$distances), "Ovitrap_ID"), drop = FALSE]
  if (ncol(num) > 0) expect_false(any(unlist(num) == 0, na.rm = TRUE))
})

test_that("a custom dictionary flows through to the outputs", {
  skip_if_not(nzchar(kml()))
  own <- data.frame(id = c(6L, 7L), category = c("roof", "roof"),
                    description = c("slab", "fibro"),
                    label_en = c("Slab", "Fibrocement"))
  expect_warning(
    r <- buffer_composition(kml(), radii = 50, grid_res = 5,
                           categories = own, verbose = FALSE),
    "absent from the dictionary")
  expect_equal(nrow(r$categories), 2L)
  expect_true(all(r$long$full_name %in% c("6_roof_slab", "7_roof_fibro")))
  expect_true(nrow(r$unmatched_ids) > 0)   # codes not in the small dictionary
})
