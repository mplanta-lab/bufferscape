# =============================================================================
# CHARTS
# =============================================================================

# ---------------------------------------------------------------------------
# plot_site_composition() : how the buffer is composed, one trap
# Horizontal bars, one per land-cover category present, ordered by area.
# Categories absent from the buffer are omitted, so the panel always fits.
# ---------------------------------------------------------------------------
#' Land-cover composition chart for one sampling site
#'
#' Horizontal bars, one per category present in the buffer, ordered by area.
#' Categories absent from the buffer are omitted, so the panel always fits
#' whatever the size of the dictionary.
#'
#' @param res Output of [buffer_composition()].
#' @param trap Name of the sampling point.
#' @param radius Buffer radius; defaults to the largest computed.
#' @param palette `"viridis"` (default) orders hue by magnitude. Any other
#'   value is passed to [resolve_palette()] -- `"aerial"` makes the bars match
#'   the colours used by [map_composition()], `"colorblind"` matches the
#'   colour-vision-safe scheme.
#' @param file Optional output path.
#' @param width,height,dpi Passed to [ggplot2::ggsave()].
#' @return A \pkg{ggplot} object.
#' @examples
#' \donttest{
#' kml <- system.file("extdata", "example_site.kml", package = "bufferscape")
#' res <- buffer_composition(kml, radii = 50, grid_res = 5, verbose = FALSE)
#' if (requireNamespace("ggplot2", quietly = TRUE))
#'   plot_site_composition(res, "SITE_1")
#' }
#' @export
plot_site_composition <- function(res, trap,
                                         radius  = max(res$long$radius_m),
                                         palette = "viridis",
                                         file = NULL, width = 9, height = 6,
                                         dpi = 300) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")


  d <- res$long[res$long$Ovitrap_ID == trap & res$long$radius_m == radius, ]
  d <- d[d$area_m2 > 0, ]
  if (nrow(d) == 0) {
    warning("No classified area for ", trap, " at ", radius, " m.", call. = FALSE)
    return(invisible(NULL))
  }
  ba <- pi * radius^2
  d$label <- if ("label_en" %in% names(d)) d$label_en else d$full_name
  d <- d[order(d$area_m2), ]
  d$label <- factor(d$label, levels = d$label)
  d$pct   <- 100 * d$area_m2 / ba

  if (is.character(palette) && length(palette) == 1L && palette == "viridis") {
    cols <- viridis_colours(nrow(d))          # dark = smallest, bright = largest
  } else {
    # any scheme name, palette table or named colour vector, so the chart can be
    # made to match the map exactly
    pal  <- resolve_palette(palette, res$categories)
    cols <- vapply(d$id, function(i) {
      r <- pal[pal$id == i, ]; if (nrow(r)) r$fill[1] else "#CCCCCC" }, character(1))
  }

  tkc <- res$tanks[res$tanks$Ovitrap_ID == trap & res$tanks$radius_m == radius, ]
  npool <- if ("pool_count" %in% names(tkc)) as.integer(tkc$pool_count[1]) else 0L

  g <- ggplot2::ggplot(d, ggplot2::aes(x = label, y = area_m2)) +
    ggplot2::geom_col(fill = cols, width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f m2  (%.1f%%)", area_m2, pct)),
                       hjust = -0.08, size = 3.0, colour = "grey25") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.26))) +
    ggplot2::labs(
      title = sprintf("Ovitrap %s \u2014 land-cover composition (%g m buffer)", trap, radius),
      subtitle = sprintf("%d of 29 categories present  |  %.0f m2 classified (%.1f%% of buffer)  |  %d water containers",
                         nrow(d), sum(d$area_m2), 100 * sum(d$area_m2) / ba,
                         tkc$tank_total[1] + npool),
      x = NULL, y = "surface area within buffer (m2)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
      axis.text.y  = ggplot2::element_text(size = 9),
      plot.title    = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35"),
      plot.margin   = ggplot2::margin(10, 26, 8, 8))

  if (!is.null(file)) {
    ok <- try(ggplot2::ggsave(file, g, width = width, height = height,
                              dpi = dpi, bg = "white"), silent = TRUE)
    if (inherits(ok, "try-error"))
      warning("category chart failed for ", trap, ": ",
              trimws(as.character(ok)), call. = FALSE)
  }
  g
}

# ---------------------------------------------------------------------------
# plot_group_points() : water containers per trap, one community
# One stacked column per trap. Column height = total containers; the segments
# are the three container classes, ordered by how available the water is to an
# ovipositing female (sealed -> unsealed -> open pool), on a viridis ramp so
# the abundance and the nature of the habitat read together at a glance.
# ---------------------------------------------------------------------------
#' Water containers per site across a community
#'
#' One stacked column per sampling site: sealed tanks, unsealed tanks and
#' swimming pools, so that the abundance and the nature of the larval habitat
#' read together at a glance.
#'
#' @param tanks The `tanks` table from [buffer_composition()] or
#'   [batch_composition()].
#' @param community Value of the site-name prefix to plot, for example `"VP"`.
#' @param radius Buffer radius; defaults to the largest present.
#' @param file Optional output path.
#' @param width,height,dpi Passed to [ggplot2::ggsave()].
#'
#' @details
#' Segments are ordered by how available the water is to an ovipositing female
#' -- sealed, then unsealed, then open pool -- on a viridis ramp. Sealed tanks
#' usually dominate by an order of magnitude, so a single unsealed tank would
#' otherwise be a one-pixel sliver: white separators keep every segment legible
#' and the oviposition-available count is called out above each column.
#'
#' @return A \pkg{ggplot} object.
#' @export
plot_group_points <- function(tanks, community,
                                              radius = max(tanks$radius_m),
                                              file = NULL, width = 11, height = 6,
                                              dpi = 300) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")

  d <- tanks[tanks$radius_m == radius, ]
  d$community <- sub("_.*$", "", d$Ovitrap_ID)
  d <- d[d$community == community, ]
  if (nrow(d) == 0) return(invisible(NULL))

  if (!"pool_count" %in% names(d)) d$pool_count <- 0L
  d$pool_count[is.na(d$pool_count)] <- 0L

  # order traps by their numeric suffix
  num <- suppressWarnings(as.integer(sub("^[A-Za-z]+_", "", d$Ovitrap_ID)))
  d   <- d[order(num, d$Ovitrap_ID), ]
  lev <- unique(d$Ovitrap_ID)

  lv <- c("sealed tank", "unsealed tank", "swimming pool")
  L <- rbind(
    data.frame(Ovitrap_ID = d$Ovitrap_ID, type = lv[1], n = d$tank_sealed),
    data.frame(Ovitrap_ID = d$Ovitrap_ID, type = lv[2], n = d$tank_open),
    data.frame(Ovitrap_ID = d$Ovitrap_ID, type = lv[3], n = d$pool_count))
  L$Ovitrap_ID <- factor(L$Ovitrap_ID, levels = lev)
  L$type <- factor(L$type, levels = rev(lv))     # sealed at the base

  vc <- viridis_colours(3)                       # dark -> bright with exposure
  cols <- stats::setNames(c(vc[1], vc[2], vc[3]), lv)

  tot <- stats::aggregate(n ~ Ovitrap_ID, data = L, FUN = sum)

  # sealed tanks usually dominate, so a single unsealed tank would otherwise be
  # a 1-pixel sliver. A white separator keeps every segment legible, and the
  # oviposition-available count is called out above each column.
  risk <- data.frame(Ovitrap_ID = d$Ovitrap_ID,
                     n = d$tank_sealed + d$tank_open + d$pool_count,
                     r = d$tank_open + d$pool_count)
  risk$Ovitrap_ID <- factor(risk$Ovitrap_ID, levels = lev)
  risk <- risk[risk$r > 0, ]

  g <- ggplot2::ggplot(L, ggplot2::aes(x = Ovitrap_ID, y = n, fill = type)) +
    ggplot2::geom_col(width = 0.76, colour = "white", linewidth = 0.35) +
    ggplot2::geom_text(data = tot, ggplot2::aes(x = Ovitrap_ID, y = n, label = n),
                       inherit.aes = FALSE, vjust = -0.55, size = 3.0,
                       colour = "grey25") +
    ggplot2::scale_fill_manual(values = cols, breaks = lv, name = NULL) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) +
    {if (nrow(risk) > 0)
      ggplot2::geom_text(data = risk,
                         ggplot2::aes(x = Ovitrap_ID, y = n,
                                      label = paste0("+", r)),
                         inherit.aes = FALSE, vjust = -2.0, size = 2.9,
                         fontface = 2, colour = "#B8860B")
     else NULL} +
    ggplot2::labs(
      title = sprintf("%s \u2014 water containers per ovitrap (%g m buffer)",
                      community, radius),
      subtitle = sprintf("%d traps  |  %d containers total  |  %d unsealed tanks, %d pools \u2014 the oviposition-available fraction",
                         nrow(d), sum(L$n), sum(d$tank_open), sum(d$pool_count)),
      x = NULL, y = "number of containers within buffer") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position    = "top",
      legend.text        = ggplot2::element_text(size = 9),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
      axis.text.x   = ggplot2::element_text(angle = 45, hjust = 1, size = 8.5),
      plot.title    = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35"),
      plot.margin   = ggplot2::margin(10, 10, 8, 8))

  if (!is.null(file)) {
    ok <- try(ggplot2::ggsave(file, g, width = width, height = height,
                              dpi = dpi, bg = "white"), silent = TRUE)
    if (inherits(ok, "try-error"))
      warning("container chart failed for ", community, ": ",
              trimws(as.character(ok)), call. = FALSE)
  }
  g
}


