test_that("aerial scheme returns the dictionary's own colours", {
  d <- class_dictionary()
  p <- class_palette(d, "aerial")
  expect_equal(p$fill, d$fill)
  expect_equal(p$pattern, d$pattern)
})

test_that("the built-in palette has no indistinguishable pairs", {
  d <- class_dictionary()
  expect_equal(sum(duplicated(d$fill)), 0L)
  lab <- function(h) {
    m <- grDevices::col2rgb(h) / 255
    lin <- ifelse(m <= 0.04045, m / 12.92, ((m + 0.055) / 1.055)^2.4)
    M <- matrix(c(.4124, .3576, .1805, .2126, .7152, .0722,
                  .0193, .1192, .9505), 3, byrow = TRUE)
    xyz <- as.vector(M %*% lin) / c(.95047, 1, 1.08883)
    f <- ifelse(xyz > 0.008856, xyz^(1 / 3), 7.787 * xyz + 16 / 116)
    c(116 * f[2] - 16, 500 * (f[1] - f[2]), 200 * (f[2] - f[3]))
  }
  L <- t(vapply(d$fill, lab, numeric(3)))
  n <- nrow(d); worst <- Inf
  for (i in seq_len(n - 1)) for (j in (i + 1):n)
    worst <- min(worst, sqrt(sum((L[i, ] - L[j, ])^2)))
  # regression guard: ids 6 and 10 were once the same colour
  expect_gt(worst, 5)
})

test_that("every scheme covers every class exactly once", {
  d <- class_dictionary()
  for (s in c("aerial", "colorblind", "viridis", "greyscale")) {
    p <- class_palette(d, s)
    expect_equal(nrow(p), nrow(d), info = s)
    expect_equal(p$id, d$id, info = s)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", p$fill)), info = s)
    expect_true(all(p$pattern %in%
      c("none", "dots", "dots2", "diag", "diag2", "cross",
        "horiz", "vert", "grid")), info = s)
  }
})

test_that("members of a group are separated by texture or lightness", {
  d <- class_dictionary()
  p <- class_palette(d, "colorblind")
  for (g in unique(d$category)) {
    ids <- d$id[d$category == g]
    if (length(ids) < 2) next
    sub <- p[p$id %in% ids, ]
    # no two members may share BOTH a texture and a colour
    key <- paste(sub$pattern, sub$fill)
    expect_equal(length(unique(key)), nrow(sub), info = g)
  }
})

test_that("every texture in the vocabulary renders distinctly", {
  # a pattern name must never fall through to a different mark
  skip_if_not_installed("sf")
  sq <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(20, 0), c(20, 20),
                                             c(0, 20), c(0, 0)))))
  marks <- list()
  for (k in c("dots", "dots2", "diag", "diag2", "horiz", "vert",
              "cross", "grid")) {
    tx <- bufferscape:::.bs_texture(sq, k, spacing = 2)
    got <- if (!is.null(tx$dots)) tx$dots[order(tx$dots$x, tx$dots$y), ]
           else tx$lines[order(tx$lines$x, tx$lines$y), c("x", "y")]
    marks[[k]] <- round(as.matrix(got), 3)
  }
  # each texture must differ from every other
  for (a in seq_along(marks)) for (b in seq_along(marks)) {
    if (b <= a) next
    same <- identical(dim(marks[[a]]), dim(marks[[b]])) &&
            isTRUE(all.equal(marks[[a]], marks[[b]]))
    expect_false(same, info = paste(names(marks)[a], "vs", names(marks)[b]))
  }
  expect_error(bufferscape:::.bs_texture(sq, "swirl"), "Unknown pattern")
})

test_that("the colorblind scheme leaves no pair ambiguous under deuteranopia", {
  d <- class_dictionary()
  p <- class_palette(d, "colorblind")
  deut <- function(h) {
    m <- grDevices::col2rgb(h) / 255
    lin <- ifelse(m <= 0.04045, m / 12.92, ((m + 0.055) / 1.055)^2.4)
    L <- 17.8824 * lin[1] + 43.5161 * lin[2] + 4.11935 * lin[3]
    S <- 0.0299566 * lin[1] + 0.184309 * lin[2] + 1.46709 * lin[3]
    Mf <- matrix(c(17.8824, 43.5161, 4.11935, 3.45565, 27.1554, 3.86714,
                   0.0299566, 0.184309, 1.46709), 3, byrow = TRUE)
    v <- as.vector(solve(Mf) %*% c(L, 0.494207 * L + 1.24827 * S, S))
    v <- pmin(pmax(v, 0), 1)
    o <- ifelse(v <= 0.0031308, v * 12.92, 1.055 * v^(1 / 2.4) - 0.055)
    grDevices::rgb(o[1], o[2], o[3])
  }
  cc <- vapply(p$fill, deut, character(1))
  n <- nrow(p); ambiguous <- 0
  for (i in seq_len(n - 1)) for (j in (i + 1):n) {
    same_col <- isTRUE(all.equal(
      as.vector(grDevices::col2rgb(cc[i])),
      as.vector(grDevices::col2rgb(cc[j])), tolerance = 0.06))
    if (same_col && p$pattern[i] == p$pattern[j]) ambiguous <- ambiguous + 1
  }
  expect_equal(ambiguous, 0)
})

test_that("resolve_palette accepts names, tables and vectors", {
  expect_equal(nrow(resolve_palette("colorblind")), 28L)
  own <- data.frame(id = 1:3, fill = c("#111111", "#222222", "#333333"))
  r <- resolve_palette(own)
  expect_equal(r$fill[1:3], own$fill)          # supplied classes honoured
  expect_equal(nrow(r), 28L)                   # the rest filled in
  v <- resolve_palette(c("7" = "#FF00FF"))
  expect_equal(v$fill[v$id == 7], "#FF00FF")
  expect_error(resolve_palette(42), "must be a scheme name")
  expect_error(resolve_palette(data.frame(a = 1)), "id.*fill")
})

test_that("a custom dictionary gets a working colorblind scheme", {
  own <- data.frame(id = 1:5,
                    category = c("water", "water", "built", "built", "veg"),
                    description = c("pond", "channel", "roof", "wall", "tree"))
  p <- class_palette(own, "colorblind")
  expect_equal(nrow(p), 5L)
  # two-member groups must differ by texture
  expect_false(p$pattern[1] == p$pattern[2])
  expect_false(p$pattern[3] == p$pattern[4])
})

test_that("more groups than hues warns rather than failing", {
  many <- data.frame(id = 1:15, category = paste0("g", 1:15),
                     description = "x")
  expect_warning(p <- class_palette(many, "colorblind"), "colour-vision-safe")
  expect_equal(nrow(p), 15L)
  expect_equal(length(unique(p$fill)), 15L)
})
