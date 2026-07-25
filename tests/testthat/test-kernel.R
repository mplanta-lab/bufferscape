test_that("kernels are bounded on [0,1] and equal 1 at the origin", {
  d <- c(0, 1, 10, 45, 100, 1000)
  for (k in c("exponential", "gaussian")) {
    w <- decay_kernel(d, k, lambda = 45)
    expect_equal(w[1], 1)
    expect_true(all(w >= 0 & w <= 1))
    expect_true(all(diff(w) <= 0))          # monotone decreasing
  }
  expect_equal(decay_kernel(d, "none"), rep(1, length(d)))
})

test_that("the exponential kernel matches its closed form", {
  expect_equal(decay_kernel(45, "exponential", 45), exp(-1))
  expect_equal(decay_kernel(90, "exponential", 45), exp(-2))
})

test_that("bad arguments are rejected", {
  expect_error(decay_kernel(1, "cauchy"), "Unknown kernel")
  expect_error(decay_kernel(1, "exponential", lambda = 0), "positive")
  expect_error(decay_kernel(1, "exponential", lambda = -5), "positive")
})

test_that("hyphenated category codes survive parsing", {
  p <- parse_class_codes(c("7", " 6 ", "8-6", "5 / 7", "", NA))
  expect_equal(p$id_primary,   c(7L, 6L, 8L, 5L, NA, NA))
  expect_equal(p$id_secondary, c(NA, NA, 6L, 7L, NA, NA))
  expect_equal(p$n_codes,      c(1L, 1L, 2L, 2L, 0L, 0L))
})
