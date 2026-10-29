ex_res <- function() {
  f <- system.file("extdata", "example_site.kml", package = "bufferscape")
  suppressWarnings(buffer_composition(f, radii = 50, grid_res = 5,
                                      verbose = FALSE))
}

# Tail and tip of the one arrow drawn on a map, whichever end carries the head.
arrow_of <- function(g) {
  L <- tryCatch(g$layers, error = function(e) NULL)
  if (is.null(L)) skip("ggplot layers cannot be inspected in this ggplot2 version")
  seg <- Filter(function(l) inherits(l$geom, "GeomSegment") &&
                  !is.null(l$geom_params$arrow), L)
  expect_length(seg, 1)
  d <- seg[[1]]$data
  e <- seg[[1]]$geom_params$arrow$ends         # 1 = first, 2 = last, 3 = both
  a <- c(d$x[1], d$y[1]); b <- c(d$xend[1], d$yend[1])
  if (e == 1L) list(tail = b, tip = a, ends = e) else list(tail = a, tip = b, ends = e)
}

test_that("the north arrow points north on every map", {
  skip_if_not_installed("ggplot2")
  res  <- ex_res()
  site <- res$traps$Name[1]
  maps <- list(composition = map_composition(res, site),
               basemap     = map_basemap(res, site, basemap = "none"),
               combined    = map_combined(res, site, basemap = "none"))
  for (nm in names(maps)) {
    a <- arrow_of(maps[[nm]])
    expect_false(a$ends == 3L, info = nm)      # a single head
    v <- a$tip - a$tail
    # 1.3.0 drew the head on the southern end of the shaft
    expect_gt(v[2], 0, label = paste(nm, ": arrowhead north of the tail"))
    # in the example's UTM zone true north is within a degree of grid north
    expect_lt(abs(atan2(v[1], v[2]) * 180 / pi), 1.5,
              label = paste(nm, ": deviation from grid north"))
  }
})

test_that("the arrow follows true north where the grid does not", {
  skip_if_not_installed("sf")
  # 60 N on the western edge of UTM zone 31N: the meridian converges by about
  # 2.6 degrees, true north lying clockwise of grid north
  pt  <- sf::st_transform(sf::st_sfc(sf::st_point(c(0, 60)), crs = 4326), 32631)
  u   <- bufferscape:::.bs_north_dir(pt)
  ang <- atan2(u[1], u[2]) * 180 / pi
  expect_equal(ang, atan(tan(3 * pi / 180) * sin(60 * pi / 180)) * 180 / pi,
               tolerance = 0.01)
  # Web Mercator has no convergence
  wm <- sf::st_transform(sf::st_sfc(sf::st_point(c(-43.24, -22.86)), crs = 4326),
                         3857)
  expect_equal(bufferscape:::.bs_north_dir(wm), c(0, 1), tolerance = 1e-6)
  # no CRS: fall back to grid north instead of failing
  expect_equal(bufferscape:::.bs_north_dir(sf::st_sfc(sf::st_point(c(1, 2)))),
               c(0, 1))
})

test_that("hatching keeps every piece of a line crossing a concave polygon", {
  skip_if_not_installed("sf")
  U <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(30, 0), c(30, 30), c(20, 30), c(20, 10), c(10, 10),
    c(10, 30), c(0, 30), c(0, 0)))))
  # 1.3.0 warned "keeping first linestring only" and dropped the second arm
  expect_silent(h <- bufferscape:::.bs_hatch_df(U, spacing = 2, angle = 0))
  rows <- split(h$grp, round(h$y, 6))
  yy   <- as.numeric(names(rows))
  np   <- vapply(rows, function(g) length(unique(g)), integer(1))
  expect_true(all(np[yy > 10.01] == 2L))       # both arms of the U
  expect_true(all(np[yy < 9.99] == 1L))        # the base, one piece
})
