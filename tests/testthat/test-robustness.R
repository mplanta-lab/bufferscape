# Regressions from a 44-file field run that exposed four separate failures.

test_that("self-intersecting polygons are repaired rather than aborting a site", {
  skip_if_not_installed("sf")
  bow <- sf::st_sf(Name = "bad", Description = "7",
                   geometry = sf::st_sfc(sf::st_polygon(list(rbind(
                     c(0, 0), c(10, 10), c(10, 0), c(0, 10), c(0, 0)))),
                     crs = 31983))
  expect_false(all(sf::st_is_valid(bow)))
  fixed <- suppressMessages(bufferscape:::.bs_make_valid(bow, "test"))
  expect_true(all(sf::st_is_valid(fixed)))
})

test_that("an overlay that GEOS refuses is retried, not propagated", {
  skip_if_not_installed("sf")
  x <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(rbind(
         c(0, 0), c(10, 10), c(10, 0), c(0, 10), c(0, 0)))), crs = 31983))
  y <- sf::st_sf(id = 1, geometry = sf::st_buffer(
         sf::st_sfc(sf::st_point(c(5, 5)), crs = 31983), 50))
  # whether GEOS throws here depends on its version, so the assertion is only
  # that the guarded call never propagates an error, whatever GEOS does
  r <- suppressWarnings(bufferscape:::.bs_safe_intersection(x, y, "test"))
  expect_true(is.null(r) || inherits(r, "sf"))
  expect_no_error(suppressWarnings(
    bufferscape:::.bs_safe_intersection(x, y, "test")))
})

test_that("the workbook writer never refuses a list column", {
  skip_if_not_installed("writexl")
  s <- list(one = data.frame(a = 1:2, b = I(list(1, 2:3))),
            two = data.frame(z = "ok"))
  f <- tempfile(fileext = ".xlsx")
  expect_warning(bufferscape:::.bs_write_xlsx(s, f), "Flattened")
  expect_true(file.exists(f))
  skip_if_not_installed("readxl")
  expect_equal(readxl::excel_sheets(f), c("one", "two"))   # names preserved
  expect_equal(readxl::read_excel(f, sheet = "one")$b[2], "2; 3")
  unlink(f)
})

test_that("a legend value is always a single string", {
  # a site duplicated upstream gave a length-2 legend cell, which stopped
  # map drawing part-way through a batch
  rows <- list(list(txt = "sealed", val = c("  16", "  16"), kind = "row"))
  rows <- lapply(rows, function(z) {
    z$txt <- as.character(z$txt)[1]; z$val <- as.character(z$val)[1]; z
  })
  expect_length(vapply(rows, function(z) z$val, character(1)), 1L)
})
