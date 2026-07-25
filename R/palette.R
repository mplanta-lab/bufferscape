#' Colour and texture schemes for a dictionary
#'
#' Turns a class dictionary into the `fill` and `pattern` used by the map and
#' chart functions. Separating this from the drawing code means a scheme can be
#' inspected, tested and swapped without touching a figure.
#'
#' @param categories A dictionary, as returned by [class_dictionary()].
#' @param scheme One of:
#'   \describe{
#'     \item{`"aerial"`}{(default) the dictionary's own `fill` and `pattern`.
#'       For [mare_categories] these echo the appearance in aerial imagery:
#'       greens for vegetation, terracotta for clay tile, near-white for
#'       reflective metal, greys for concrete, rust for corroded metal.}
#'     \item{`"colorblind"`}{one colour-vision-safe hue per coarse group, with
#'       members of a group separated by a lightness step **and** a texture. See
#'       Details.}
#'     \item{`"viridis"`}{the viridis ramp ordered by `id`, no textures. Useful
#'       when the classes are ordinal rather than nominal.}
#'     \item{`"greyscale"`}{a lightness ramp plus textures, for print or
#'       photocopying.}
#'   }
#' @param hues Optional character vector of hex colours to use as the group
#'   hues under `scheme = "colorblind"`, overriding the built-in bank.
#'
#' @details
#' # Why "colorblind" works by group
#' A palette of 29 nominal colours cannot be made safe for colour-vision
#' deficiency; the perceptual space is not large enough, and any such palette
#' will contain pairs that converge under deuteranopia. Measured on
#' [mare_categories], even a palette built entirely from colour-vision-safe
#' primaries leaves a minimum pairwise distance of roughly 4 under simulated
#' deuteranopia, below a comfortable discrimination threshold.
#'
#' The `"colorblind"` scheme therefore stops asking colour to do the whole job.
#' Colour distinguishes the **coarse group** only -- 12 of them for
#' [mare_categories], where the minimum distance under simulated deuteranopia
#' rises to about 7.6 -- and within a group, members are separated by a
#' lightness step and by a distinct texture. No class depends on hue as its only
#' cue, which is the property that makes a figure accessible, and it also
#' survives greyscale printing.
#'
#' If a dictionary has more groups than available hues, the bank is cycled with
#' a lightness offset and a warning is issued: at that point the grouping is
#' probably too fine to carry by colour at all.
#'
#' @return A `data.frame` with columns `id`, `fill` and `pattern`.
#'
#' @examples
#' # the appearance-matched default
#' head(class_palette())
#'
#' # colour-vision-safe alternative
#' head(class_palette(scheme = "colorblind"))
#'
#' # with a custom dictionary
#' own <- data.frame(id = 1:4,
#'                   category    = c("water", "water", "built", "veg"),
#'                   description = c("pond", "channel", "roof", "canopy"))
#' class_palette(own, scheme = "colorblind")
#'
#' @seealso [class_dictionary()], [map_composition()]
#' @export
class_palette <- function(categories = class_dictionary(),
                          scheme = c("aerial", "colorblind", "viridis",
                                     "greyscale"),
                          hues = NULL) {
  scheme <- match.arg(scheme)
  d <- validate_dictionary(categories)
  n <- nrow(d)

  if (scheme == "aerial")
    return(d[, c("id", "fill", "pattern")])

  if (scheme == "viridis")
    return(data.frame(id = d$id, fill = viridis_colours(n),
                      pattern = "none", stringsAsFactors = FALSE))

  # textures used to separate members within a single group
  # nine textures plus "none" = ten distinct marks, which covers the largest
  # group in the built-in dictionary. Members are additionally separated by a
  # lightness step, so a repeat would still be distinguishable.
  tex <- c("none", "diag", "cross", "dots", "horiz", "vert", "grid",
           "diag2", "dots2")

  if (scheme == "greyscale") {
    # spread lightness across the dictionary, then let texture separate
    # neighbours that end up close
    g <- grDevices::grey(seq(0.90, 0.25, length.out = n))
    p <- character(n)
    for (k in seq_len(n)) p[k] <- tex[((k - 1) %% length(tex)) + 1]
    return(data.frame(id = d$id, fill = g, pattern = p,
                      stringsAsFactors = FALSE))
  }

  # ---- colorblind -------------------------------------------------------
  # Okabe-Ito, plus four additions from the Tol qualitative sets, all checked
  # for separability under simulated deuteranopia and protanopia.
  bank <- if (!is.null(hues)) hues else
    c("#009E73", "#D55E00", "#0072B2", "#CC79A7", "#E69F00",
      "#56B4E9", "#F0E442", "#999999", "#332288", "#661100",
      "#AA4499", "#000000")

  grp  <- unique(d$category)
  ng   <- length(grp)
  if (ng > length(bank))
    warning("Dictionary has ", ng, " groups but only ", length(bank),
            " colour-vision-safe hues are available; hues are cycled with a ",
            "lightness offset. Consider a coarser `category` grouping.",
            call. = FALSE)

  ghex <- character(ng)
  for (k in seq_len(ng)) {
    base <- bank[((k - 1) %% length(bank)) + 1]
    lap  <- (k - 1) %/% length(bank)
    ghex[k] <- if (lap == 0) base else .bs_lighten(base, 0.30 * lap)
  }
  names(ghex) <- grp

  fill <- character(n); pat <- character(n)
  for (k in seq_len(n)) {
    g   <- d$category[k]
    h   <- ghex[[g]]
    mem <- sort(d$id[d$category == g])
    rk  <- which(mem == d$id[k]); nm <- length(mem)
    # lightness spread within the group, centred so the middle member keeps
    # the group hue recognisable
    f <- if (nm == 1) 0 else (rk - 1) / (nm - 1) * 0.55 - 0.12
    fill[k] <- if (f >= 0) .bs_lighten(h, f) else .bs_darken(h, -f)
    pat[k]  <- if (nm == 1) "none" else tex[((rk - 1) %% length(tex)) + 1]
  }
  data.frame(id = d$id, fill = fill, pattern = pat, stringsAsFactors = FALSE)
}

#' @keywords internal
#' @noRd
.bs_lighten <- function(hex, f = 0.35) {
  m <- grDevices::col2rgb(hex)
  grDevices::rgb(t(round(m + (255 - m) * f)), maxColorValue = 255)
}

#' @keywords internal
#' @noRd
.bs_darken <- function(hex, f = 0.55) {
  m <- grDevices::col2rgb(hex)
  grDevices::rgb(t(round(m * (1 - f))), maxColorValue = 255)
}

#' Resolve a palette argument
#'
#' Accepts a scheme name, a data frame, or a named vector of colours, and
#' returns a canonical palette table. Used internally by the figure functions so
#' they all take `palette` the same way.
#'
#' @param palette A scheme name, a `data.frame` with `id`/`fill`
#'   (and optionally `pattern`), or a vector of colours named by class `id`.
#' @param categories The dictionary in use.
#' @return A `data.frame` with `id`, `fill`, `pattern`.
#' @examples
#' resolve_palette("colorblind")
#' resolve_palette(c("1" = "#FF0000", "2" = "#00FF00"))
#' @export
resolve_palette <- function(palette = "aerial",
                            categories = class_dictionary()) {
  d <- validate_dictionary(categories)

  if (is.character(palette) && length(palette) == 1L &&
      palette %in% c("aerial", "colorblind", "viridis", "greyscale"))
    return(class_palette(d, scheme = palette))

  if (is.data.frame(palette)) {
    if (!all(c("id", "fill") %in% names(palette)))
      stop("A palette data.frame needs at least `id` and `fill` columns.",
           call. = FALSE)
    p <- data.frame(id = as.integer(palette$id),
                    fill = as.character(palette$fill),
                    pattern = if ("pattern" %in% names(palette))
                      as.character(palette$pattern) else "none",
                    stringsAsFactors = FALSE)
  } else if (is.character(palette) && !is.null(names(palette))) {
    p <- data.frame(id = as.integer(names(palette)),
                    fill = unname(palette), pattern = "none",
                    stringsAsFactors = FALSE)
  } else {
    stop("`palette` must be a scheme name (\"aerial\", \"colorblind\", ",
         "\"viridis\", \"greyscale\"), a data.frame with id/fill, or a ",
         "vector of colours named by class id.", call. = FALSE)
  }

  p$pattern[is.na(p$pattern) | !nzchar(p$pattern)] <- "none"
  # fill any class the user did not supply, so a partial palette is allowed
  miss <- setdiff(d$id, p$id)
  if (length(miss)) {
    base <- class_palette(d, scheme = "aerial")
    p <- rbind(p, base[base$id %in% miss, ])
  }
  p[match(d$id, p$id), ]
}
