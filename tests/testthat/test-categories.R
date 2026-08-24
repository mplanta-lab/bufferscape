test_that("the built-in dictionary is valid and has 28 classes", {
  d <- class_dictionary()
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 28L)
  expect_true(all(c("id", "category", "description", "label_en",
                    "fill", "pattern", "full_name") %in% names(d)))
  expect_false(anyDuplicated(d$id) > 0)
})

test_that("a user dictionary is accepted and completed", {
  own <- data.frame(id = 1:3,
                    category = c("water", "built", "veg"),
                    description = c("pond", "roof", "canopy"))
  d <- class_dictionary(own)
  expect_equal(nrow(d), 3L)
  expect_equal(d$label_en, c("water pond", "built roof", "veg canopy"))
  expect_true(all(d$pattern == "none"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", d$fill)))   # generated
  expect_equal(d$full_name, c("1_water_pond", "2_built_roof", "3_veg_canopy"))
})

test_that("supplied fills and patterns are preserved", {
  own <- data.frame(id = 1:2, category = c("a", "b"),
                    description = c("x", "y"),
                    fill = c("#123456", "#ABCDEF"),
                    pattern = c("dots", "none"))
  d <- class_dictionary(own)
  expect_equal(d$fill, c("#123456", "#ABCDEF"))
  expect_equal(d$pattern, c("dots", "none"))
})

test_that("invalid dictionaries are rejected with a useful message", {
  expect_error(validate_dictionary(data.frame(id = 1)), "missing required column")
  expect_error(validate_dictionary(data.frame(id = c(1, 1), category = "a",
                                              description = "b")), "unique")
  expect_error(validate_dictionary(data.frame(id = c(1, 2), category = "a",
                                              description = "b",
                                              pattern = c("swirl", "none"))),
               "Unknown pattern")
  expect_error(validate_dictionary(data.frame(id = 1, category = "a",
                                              description = "b",
                                              fill = "not-a-colour")), "hex")
  expect_error(validate_dictionary(data.frame()), "no rows|missing required")
  expect_error(class_dictionary(42), "must be NULL")
  expect_error(class_dictionary("no/such/file.xlsx"), "not found")
})

test_that("a custom dictionary drives the palette", {
  own <- data.frame(id = 1:2, category = c("a", "b"), description = c("x", "y"),
                    fill = c("#111111", "#222222"))
  p <- class_palette(own)
  expect_equal(p$fill, c("#111111", "#222222"))
  expect_equal(nrow(p), 2L)
})

test_that("viridis returns the requested number of colours", {
  expect_length(viridis_colours(1), 1L)
  expect_length(viridis_colours(29), 29L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", viridis_colours(5))))
  expect_error(viridis_colours(0), "positive")
})
