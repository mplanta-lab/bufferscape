kml <- function() system.file("extdata", "example_site.kml", package = "bufferscape")

test_that("centroid_bias returns one row per polygon with shape descriptors", {
  skip_if_not(nzchar(kml()))
  b <- suppressWarnings(centroid_bias(kml(), radii = 50, grid_res = 5,
                                      verbose = FALSE))
  expect_true(nrow(b) > 0)
  expect_true(all(c("area_m2", "d_centroid", "d_mean", "compactness",
                    "centroid_offset", "bias_pct", "geometry_class")
                  %in% names(b)))
  expect_true(all(b$compactness > 0 & b$compactness <= 1.001, na.rm = TRUE))
  expect_true(all(b$d_max >= b$d_centroid - 1e-6))
  # d_min <= d_centroid holds only when the centroid lies inside the polygon;
  # for a concave feature the centroid can be nearer the site than the feature.
  ins <- b[b$centroid_inside, ]
  expect_true(all(ins$d_min <= ins$d_centroid + 1e-6))
})

test_that("centroids outside concave polygons are flagged", {
  skip_if_not(nzchar(kml()))
  b <- suppressWarnings(centroid_bias(kml(), radii = 50, grid_res = 5,
                                      verbose = FALSE))
  expect_type(b$centroid_inside, "logical")
  out <- b[!b$centroid_inside, ]
  # any such polygon must be concave, i.e. far from circular
  if (nrow(out) > 0) expect_true(all(out$compactness < 0.5))
})

test_that("bias is signed by the centroid offset", {
  skip_if_not(nzchar(kml()))
  b <- suppressWarnings(centroid_bias(kml(), radii = 50, grid_res = 5,
                                      verbose = FALSE))
  # a centroid nearer than the polygon's mean distance over-weights it
  pos <- b[b$centroid_offset > 0.02, ]
  neg <- b[b$centroid_offset < -0.02, ]
  if (nrow(pos) > 3) expect_gt(median(pos$bias_pct), 0)
  if (nrow(neg) > 3) expect_lt(median(neg$bias_pct), 0)
  # and the two agree closely when the offset is negligible
  small <- b[abs(b$centroid_offset) < 0.005, ]
  if (nrow(small) > 3) expect_lt(median(abs(small$bias_pct)), 2)
})

test_that("no kernel means no bias", {
  skip_if_not(nzchar(kml()))
  b <- suppressWarnings(centroid_bias(kml(), radii = 50, grid_res = 5,
                                      kernel = "none", verbose = FALSE))
  expect_equal(b$bias_pct, rep(0, nrow(b)), tolerance = 1e-8)
})

test_that("the summary groups and orders as documented", {
  skip_if_not(nzchar(kml()))
  b <- suppressWarnings(centroid_bias(kml(), radii = 50, grid_res = 5,
                                      verbose = FALSE))
  s <- summarise_centroid_bias(b)
  expect_true(all(c("group", "n_polygons", "median_bias_pct",
                    "max_bias_pct", "pct_over_10") %in% names(s)))
  expect_equal(sum(s$n_polygons), nrow(b))
  expect_true(is.data.frame(summarise_centroid_bias(b, by = "class")))
  expect_true(is.data.frame(summarise_centroid_bias(b, by = "site")))
  expect_error(summarise_centroid_bias(b, by = "nonsense"))
})

test_that("the nearest-point weight is an upper bound and overestimates", {
  skip_if_not(nzchar(kml()))
  b <- suppressWarnings(centroid_bias(kml(), radii = 50, grid_res = 5,
                                      verbose = FALSE))
  # w(d_min) >= mean weight over the polygon, always
  expect_true(all(b$w_nearest >= b$w_integrated - 1e-9))
  expect_true(all(b$bias_nearest_pct >= -1e-6))
  # and it is worse than the centroid on typical polygons
  expect_gt(median(b$bias_nearest_pct), median(abs(b$bias_pct)))
})
