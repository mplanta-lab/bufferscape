# =============================================================================
# MAPPING
# -----------------------------------------------------------------------------
# Three figures per trap:
#   <trap>_satellite.png    imagery + buffer + trap only
#   <trap>_categories.png   white ground, polygons coloured by category
#   <trap>.png              combined (imagery + polygons + legend)
#
# Deliberately built WITHOUT geom_sf, coord_sf, patchwork, tidyterra or
# ggpattern. Each of those has proved version-fragile here:
#   * patchwork < 1.3 with ggplot2 >= 3.5 throws Ops.data.frame in guide code
#   * coord_sf renegotiates CRS at render time and can silently drop layers
#   * zero-row sf layers (a trap with no unsealed tanks) can blank a panel
# Geometry is converted to plain x/y data frames and drawn with geom_polygon /
# geom_path / geom_point under coord_fixed(). Hatching and stippling are built
# as real clipped geometry rather than via a pattern package.
# =============================================================================



# darker outline of the same hue

# ---- pattern geometry (clipped hatching / stippling) -----------------------
#' @keywords internal
#' @noRd
.bs_hatch_df <- function(g, spacing = 2.0, angle = 45) {
  if (is.null(g) || length(g) == 0) return(NULL)
  g <- try(sf::st_union(sf::st_geometry(g)), silent = TRUE)
  if (inherits(g, "try-error") || length(g) == 0) return(NULL)
  bb <- sf::st_bbox(g)
  if (any(!is.finite(as.numeric(bb)))) return(NULL)
  cx <- mean(as.numeric(bb[c("xmin", "xmax")]))
  cy <- mean(as.numeric(bb[c("ymin", "ymax")]))
  d  <- sqrt(diff(as.numeric(bb[c("xmin", "xmax")]))^2 +
             diff(as.numeric(bb[c("ymin", "ymax")]))^2)
  if (!is.finite(d) || d <= 0) return(NULL)
  th <- angle * pi / 180
  dir <- c(cos(th), sin(th)); per <- c(-sin(th), cos(th))
  offs <- seq(-d / 2, d / 2, by = spacing)
  if (length(offs) > 400) return(NULL)
  ls <- lapply(offs, function(o) {
    p0 <- c(cx, cy) + o * per - (d / 2) * dir
    p1 <- c(cx, cy) + o * per + (d / 2) * dir
    sf::st_linestring(rbind(p0, p1))
  })
  sl  <- sf::st_sfc(ls, crs = sf::st_crs(g))
  cut <- try(suppressWarnings(sf::st_intersection(sl, g)), silent = TRUE)
  if (inherits(cut, "try-error") || length(cut) == 0) return(NULL)
  # A hatch line that grazes a vertex intersects the polygon in a POINT, and
  # st_cast() errors on a mixed collection. Dropping the degenerate pieces first
  # keeps the whole texture from being lost.
  gt  <- as.character(sf::st_geometry_type(cut))
  keep <- gt %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION")
  if (!any(keep)) return(NULL)
  cut <- cut[keep]
  if (any(gt[keep] == "GEOMETRYCOLLECTION")) {
    ex <- try(sf::st_collection_extract(cut, "LINESTRING"), silent = TRUE)
    if (!inherits(ex, "try-error")) cut <- ex
  }
  cut <- try(sf::st_cast(cut, "LINESTRING", warn = FALSE), silent = TRUE)
  if (inherits(cut, "try-error") || length(cut) == 0) return(NULL)
  cm <- try(sf::st_coordinates(cut), silent = TRUE)
  if (inherits(cm, "try-error") || is.null(cm) || nrow(cm) == 0) return(NULL)
  data.frame(x = cm[, 1], y = cm[, 2],
             grp = paste0("h", cm[, ncol(cm)]), stringsAsFactors = FALSE)
}

#' Build the geometry for one texture
#'
#' Every texture the palette vocabulary allows is drawn here, so a pattern name
#' can never silently fall through to a different one.
#'
#' @keywords internal
#' @noRd
.bs_texture <- function(geom, kind, spacing = 1.8) {
  lines <- function(angles) {
    out <- lapply(angles, function(a)
      .bs_hatch_df(geom, spacing = spacing, angle = a))
    out <- out[!vapply(out, is.null, logical(1))]
    if (!length(out)) return(NULL)
    for (k in seq_along(out)) out[[k]]$grp <- paste0(k, "_", out[[k]]$grp)
    do.call(rbind, out)
  }
  switch(kind,
    none  = list(dots = NULL, lines = NULL),
    dots  = list(dots = .bs_dots_df(geom, spacing = 2.2), lines = NULL),
    dots2 = list(dots = .bs_dots_df(geom, spacing = 1.5), lines = NULL),
    diag  = list(dots = NULL, lines = lines(45)),
    diag2 = list(dots = NULL, lines = lines(135)),
    horiz = list(dots = NULL, lines = lines(0)),
    vert  = list(dots = NULL, lines = lines(90)),
    cross = list(dots = NULL, lines = lines(c(45, 135))),
    grid  = list(dots = NULL, lines = lines(c(0, 90))),
    stop("Unknown pattern: ", kind, call. = FALSE))
}

#' @keywords internal
#' @noRd
.bs_dots_df <- function(g, spacing = 2.0) {
  if (is.null(g) || length(g) == 0) return(NULL)
  g <- try(sf::st_union(sf::st_geometry(g)), silent = TRUE)
  if (inherits(g, "try-error") || length(g) == 0) return(NULL)
  bb <- sf::st_bbox(g)
  if (any(!is.finite(as.numeric(bb)))) return(NULL)
  xs <- seq(as.numeric(bb["xmin"]), as.numeric(bb["xmax"]), by = spacing)
  ys <- seq(as.numeric(bb["ymin"]), as.numeric(bb["ymax"]), by = spacing)
  if (!length(xs) || !length(ys) || length(xs) * length(ys) > 8e4) return(NULL)
  gr  <- expand.grid(x = xs, y = ys)
  pts <- sf::st_as_sf(gr, coords = c("x", "y"), crs = sf::st_crs(g))
  ins <- lengths(sf::st_intersects(pts, g)) > 0
  if (!any(ins)) return(NULL)
  gr[ins, , drop = FALSE]
}

# ---- ovitrap marker: black cup + wooden paddle -----------------------------
# A stylised ovitrap (black plastic cup with a eucatex paddle) rather than a
# generic dot, so the marker reads as the sampling device itself. The trap
# coordinate is the base of the cup, so the glyph sits on the location.
#' @keywords internal
#' @noRd
.bs_trap_glyph <- function(x0, y0, s) {
  th <- seq(0, 2 * pi, length.out = 48)
  list(
    pad = data.frame(x = x0 + s * c(-0.17, 0.17, 0.17, -0.17),
                     y = y0 + s * c( 1.55, 1.55, 3.55,  3.55)),
    cup = data.frame(x = x0 + s * c(-0.68, 0.68, 0.90, -0.90),
                     y = y0 + s * c( 0.10, 0.10, 2.05,  2.05)),
    rim = data.frame(x = x0 + s * 0.90 * cos(th),
                     y = y0 + s * 2.05 + s * 0.26 * sin(th))
  )
}

# ---- sf geometry -> plain data.frame ---------------------------------------
#' @keywords internal
#' @noRd
.bs_poly_df <- function(x) {
  if (is.null(x) || length(sf::st_geometry(x)) == 0) return(NULL)
  g <- sf::st_geometry(x)
  out <- vector("list", length(g))
  for (i in seq_along(g)) {
    cm <- try(sf::st_coordinates(g[i]), silent = TRUE)
    if (inherits(cm, "try-error") || is.null(cm) || nrow(cm) == 0) next
    cm <- as.data.frame(cm)
    idc <- intersect(c("L1", "L2", "L3"), names(cm))
    key <- if (length(idc)) do.call(paste, c(cm[idc], sep = "_")) else "1"
    out[[i]] <- data.frame(x = cm$X, y = cm$Y, id = i,
                           grp = paste(i, key, sep = ":"),
                           stringsAsFactors = FALSE)
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

#' @keywords internal
#' @noRd
.bs_pt_df <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  cm <- try(sf::st_coordinates(x), silent = TRUE)
  if (inherits(cm, "try-error") || is.null(cm) || nrow(cm) == 0) return(NULL)
  data.frame(x = cm[, 1], y = cm[, 2])
}

# ---- basemap -> plain raster data.frame ------------------------------------
# maptiles::get_tiles() reprojects the tiles to the CRS of the object it is
# given. `view` is already in the drawing CRS, so passing it directly returns
# the tiles in that same CRS: no resampling, and the raster lands in the same
# coordinate space as the vectors. (Passing a lon/lat view here returns degrees
# and the raster is drawn far outside the panel - it silently disappears.)
#' @keywords internal
#' @noRd
.bs_basemap_df <- function(view, draw_crs, basemap, zoom) {
  out <- list(df = NULL, zoom = NA)
  bb  <- sf::st_bbox(view)
  to_df <- function(r) {
    npx <- terra::ncell(r)
    if (npx > 1.6e6) r <- terra::aggregate(r, fact = ceiling(sqrt(npx / 1.6e6)))
    d <- terra::as.data.frame(r, xy = TRUE, na.rm = FALSE)
    if (ncol(d) < 3) return(NULL)
    names(d)[1:2] <- c("x", "y")
    b <- d[, 3:min(5, ncol(d)), drop = FALSE]
    b[] <- lapply(b, function(v) { v[is.na(v)] <- 0; pmin(pmax(v, 0), 255) })
    while (ncol(b) < 3) b[[ncol(b) + 1]] <- b[[1]]
    d$hex <- grDevices::rgb(b[[1]], b[[2]], b[[3]], maxColorValue = 255)
    d[, c("x", "y", "hex")]
  }
  fix_crs <- function(r) {
    tc <- try(terra::crs(r, describe = TRUE)$code, silent = TRUE)
    want <- draw_crs$epsg
    if (!inherits(tc, "try-error") && !is.null(tc) && !is.na(tc) &&
        !is.na(want) && as.character(tc) != as.character(want))
      r <- terra::project(r, paste0("EPSG:", want))
    r
  }

  if (identical(basemap, "esri")) {
    if (!requireNamespace("maptiles", quietly = TRUE) ||
        !requireNamespace("terra", quietly = TRUE)) {
      warning('basemap = "esri" needs maptiles + terra:\n',
              '  install.packages(c("maptiles", "terra"))', call. = FALSE)
      return(out)
    }
    for (z in if (is.na(zoom)) 20:17 else zoom) {
      tl <- try(suppressWarnings(maptiles::get_tiles(
        view, provider = "Esri.WorldImagery", crop = TRUE, zoom = z)),
        silent = TRUE)
      if (inherits(tl, "try-error") || is.null(tl)) next
      d <- try(to_df(fix_crs(tl)), silent = TRUE)
      if (!inherits(d, "try-error") && !is.null(d)) {
        out$df <- d; out$zoom <- z; break
      }
    }
    if (is.null(out$df))
      warning("Could not fetch Esri tiles - drawing without imagery.", call. = FALSE)
  } else if (!identical(basemap, "none")) {
    if (!file.exists(basemap)) {
      warning("basemap file not found: ", basemap, call. = FALSE)
    } else if (requireNamespace("terra", quietly = TRUE)) {
      d <- try({
        r0 <- fix_crs(terra::rast(basemap))
        to_df(terra::crop(r0, terra::ext(as.numeric(bb[c("xmin","xmax","ymin","ymax")]))))
      }, silent = TRUE)
      if (inherits(d, "try-error") || is.null(d))
        warning("Could not read ", basemap, " - drawing without imagery.", call. = FALSE)
      else out$df <- d
    }
  }
  out
}

# ---- shared per-trap geometry ----------------------------------------------
#' @keywords internal
#' @noRd
.bs_prep <- function(res, trap, radius, basemap) {
  trap_sf <- res$traps[res$traps$Name == trap, ]
  if (nrow(trap_sf) == 0) stop("Trap '", trap, "' not found.")
  esri     <- identical(basemap, "esri")
  draw_crs <- if (esri) sf::st_crs(3857) else sf::st_crs(trap_sf)
  rp <- function(x) if (!is.null(x) && nrow(x) > 0) sf::st_transform(x, draw_crs) else x

  trap_d <- rp(trap_sf)
  buf    <- sf::st_buffer(trap_d, radius)
  view   <- sf::st_buffer(trap_d, radius * 1.28)
  bb     <- sf::st_bbox(view)

  buf_in <- function(layer) {
    if (is.null(layer) || nrow(layer) == 0) return(NULL)
    b <- sf::st_transform(buf, sf::st_crs(layer))
    sel <- layer[lengths(sf::st_intersects(layer, b)) > 0, ]
    if (nrow(sel) == 0) NULL else rp(sel)
  }

  list(trap_sf = trap_sf, trap_d = trap_d, buf = buf, view = view,
       draw_crs = draw_crs, esri = esri,
       xmin = as.numeric(bb["xmin"]), xmax = as.numeric(bb["xmax"]),
       ymin = as.numeric(bb["ymin"]), ymax = as.numeric(bb["ymax"]),
       lay = buf_in(res$polygons), tk = buf_in(res$tanks_sf),
       pl = buf_in(res$pools_sf))
}

#' @keywords internal
#' @noRd
.bs_scalebar_len <- function(p, radius) {
  # a "nice" round length near a fifth of the buffer diameter, so the bar stays
  # sensible whether the buffer is 20 m or 5 km
  target <- 0.4 * radius
  nice <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500,
            1000, 2000, 2500, 5000, 10000)
  m <- nice[which.min(abs(nice - target))]
  lab <- if (m >= 1000) paste0(m / 1000, " km") else paste0(m, " m")
  # in Web Mercator a ground metre is inflated by 1/cos(latitude)
  k <- if (p$esri) {
    lat <- as.numeric(sf::st_coordinates(sf::st_transform(p$trap_d, 4326))[1, 2])
    1 / cos(lat * pi / 180)
  } else 1
  list(len = m * k, label = lab)
}

# =============================================================================
# 1. SATELLITE MAP : imagery + buffer + trap
# =============================================================================
#' Satellite basemap figure for one sampling site
#'
#' Imagery with the buffer and the sampling point, and nothing else.
#'
#' @inheritParams map_composition
#' @param basemap `"esri"` for Esri World Imagery, a path to a georeferenced
#'   raster of your own, or `"none"`.
#' @param zoom Tile zoom level; `NA` picks the finest the provider serves.
#'
#' @details
#' Tiles are requested in the CRS the figure is drawn in, so the raster is never
#' resampled and cannot drift relative to the polygons. Requires
#' \pkg{maptiles} and \pkg{terra}; without them the panel renders white and a
#' warning is issued rather than failing.
#'
#' Google Earth imagery is not offered: its terms permit alteration only inside
#' Google software, and it cannot be relicensed for an open-access figure.
#'
#' @return A \pkg{ggplot} object.
#' @export
map_basemap <- function(res, trap,
                                  radius  = max(res$long$radius_m),
                                  basemap = "esri", zoom = NA,
                                  file = NULL, width = 8, height = 8, dpi = 300) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required.")
  p  <- .bs_prep(res, trap, radius, basemap)
  bm <- .bs_basemap_df(p$view, p$draw_crs, basemap, zoom)
  img <- !is.null(bm$df)
  W <- p$xmax - p$xmin; H <- p$ymax - p$ymin
  ink <- if (img) "white" else "grey15"

  buf_df  <- .bs_poly_df(p$buf)
  trap_df <- .bs_pt_df(p$trap_d)
  sb <- .bs_scalebar_len(p, radius); sl <- sb$len
  sx <- p$xmax - 0.30 * W; sy <- p$ymin + 0.06 * H

  g <- ggplot2::ggplot()
  if (img)
    g <- g + ggplot2::geom_raster(data = bm$df,
                                  ggplot2::aes(x = x, y = y, fill = hex)) +
      ggplot2::scale_fill_identity()
  else
    g <- g + ggplot2::annotate("rect", xmin = p$xmin, xmax = p$xmax,
                               ymin = p$ymin, ymax = p$ymax, fill = "#FFFFFF")
  if (!is.null(buf_df))
    g <- g + ggplot2::geom_path(data = buf_df,
                                ggplot2::aes(x = x, y = y, group = grp),
                                colour = ink, linewidth = 1.0, linetype = "22")
  if (!is.null(trap_df))
    g <- g + ggplot2::geom_point(data = trap_df, ggplot2::aes(x = x, y = y),
                                 shape = 21, size = 5.4, fill = "#00C88C",
                                 colour = ink, stroke = 1.5) +
      ggplot2::annotate("text", x = trap_df$x[1], y = trap_df$y[1] + 0.045 * H,
                        label = trap, colour = ink, fontface = 2, size = 4.4)
  g <- g +
    ggplot2::annotate("rect", xmin = sx, xmax = sx + sl,
                      ymin = sy, ymax = sy + 0.008 * H, fill = ink) +
    ggplot2::annotate("text", x = sx + sl / 2, y = sy + 0.033 * H,
                      label = sb$label, colour = ink, size = 3.1) +
    ggplot2::annotate("text", x = p$xmin + 0.05 * W, y = p$ymax - 0.06 * H,
                      label = "N", colour = ink, fontface = 2, size = 4.5) +
    ggplot2::annotate("segment", x = p$xmin + 0.05 * W, xend = p$xmin + 0.05 * W,
                      y = p$ymax - 0.105 * H, yend = p$ymax - 0.075 * H,
                      colour = ink, linewidth = 0.8,
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.16, "cm"),
                                             ends = "first")) +
    ggplot2::coord_fixed(1, xlim = c(p$xmin, p$xmax), ylim = c(p$ymin, p$ymax),
                         expand = FALSE) +
    ggplot2::labs(
      title = sprintf("Ovitrap %s \u2014 %g m buffer", trap, radius),
      caption = if (img)
        sprintf("Imagery: Esri World Imagery (zoom %s). Analysis in EPSG:%s.",
                bm$zoom, res$meta$value[res$meta$key == "epsg"])
      else 'No imagery \u2014 install.packages(c("maptiles","terra")), set BASEMAP <- "esri"') +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(face = "bold", size = 13),
                   plot.caption = ggplot2::element_text(size = 7.5, colour = "grey50",
                                                        hjust = 1),
                   plot.margin = ggplot2::margin(8, 8, 6, 8))

  if (!is.null(file)) {
    ok <- try(ggplot2::ggsave(file, g, width = width, height = height,
                              dpi = dpi, limitsize = FALSE), silent = TRUE)
    if (inherits(ok, "try-error"))
      warning("satellite map failed for ", trap, ": ",
              trimws(as.character(ok)), call. = FALSE)
    else message("map written: ", file)
  }
  g
}

# =============================================================================
# 2. CATEGORY MAP : white ground, polygons coloured + patterned by category
# =============================================================================
#' Land-cover map for one sampling site
#'
#' A white-ground figure: polygons coloured by category, water containers,
#' the buffer, and a legend panel giving the surface composition, the container
#' counts and the distances to reference features.
#'
#' @param res Output of [buffer_composition()].
#' @param trap Name of the sampling point to draw.
#' @param radius Buffer radius; defaults to the largest computed.
#' @param palette Colours and textures. A scheme name -- `"aerial"` (default),
#'   `"colorblind"`, `"viridis"` or `"greyscale"` -- or a `data.frame` with
#'   `id`/`fill`/`pattern`, or a vector of colours named by class id. A partial
#'   palette is allowed: unlisted classes fall back to the dictionary's own
#'   colour. See [class_palette()].
#' @param top_n Number of classes listed individually in the legend; the rest
#'   are pooled as "other categories".
#' @param fill_alpha Fill transparency. Semi-transparent fills let overlapping
#'   polygons show through, which is the point.
#' @param patterns Draw the textures defined by the palette. `FALSE` gives flat
#'   fills.
#' @param show_unclassified Draw and quantify the part of the buffer covered by
#'   no polygon, hatched in grey.
#' @param label_ids Print the class id on this many of the largest polygons.
#'   `0` (default) prints none. Useful with large dictionaries, where colour
#'   alone cannot separate every class.
#' @param legend Which legend sections to show: any of `"composition"`,
#'   `"containers"`, `"key"`. `FALSE` suppresses the legend entirely and the map
#'   fills the panel.
#' @param title,subtitle,caption Text overrides. `NULL` uses the automatic text;
#'   `""` suppresses that line.
#' @param file Optional output path.
#' @param width,height,dpi Passed to [ggplot2::ggsave()].
#'
#' @details
#' Polygons are clipped to a square frame set outside the buffer, so features
#' overshoot the circle as they do in the imagery but never run across the
#' legend. Overlapping polygons are drawn largest-first, so small features stay
#' visible. Patterns separate classes that share a base material: dots for
#' debris, crosshatch for degraded mixed slab, diagonals for active
#' construction, and grey hatching for unclassified ground.
#'
#' # Customising beyond the arguments
#' The return value is an ordinary \pkg{ggplot} object, so anything not exposed
#' as an argument can be added afterwards -- `+ ggplot2::labs()`,
#' `+ ggplot2::theme()`, further layers. One exception is worth knowing: the
#' legend is drawn as text **inside the panel**, not as a ggplot guide, so
#' `theme(legend.*)` has no effect on it. That is why `legend`, `top_n` and
#' `title` are explicit arguments.
#'
#' Drawing deliberately avoids `geom_sf()`, `coord_sf()` and composition
#' packages. Geometry is converted to plain coordinates and drawn with
#' [ggplot2::geom_polygon()] under [ggplot2::coord_fixed()], which is the most
#' version-stable combination and cannot lose layers to CRS renegotiation at
#' render time.
#'
#' @return A \pkg{ggplot} object, invisibly written to `file` if given.
#' @examples
#' \donttest{
#' kml <- system.file("extdata", "example_site.kml", package = "bufferscape")
#' res <- buffer_composition(kml, radii = 50, grid_res = 5, verbose = FALSE)
#' if (requireNamespace("ggplot2", quietly = TRUE)) map_composition(res, "SITE_1")
#' }
#' @export
map_composition <- function(res, trap,
                            radius = max(res$long$radius_m),
                            palette = "aerial",
                            top_n = 10, fill_alpha = 0.55,
                            patterns = TRUE,
                            show_unclassified = TRUE,
                            label_ids = 0,
                            legend = c("composition", "containers", "key"),
                            title = NULL, subtitle = NULL, caption = NULL,
                            file = NULL, width = 13, height = 8, dpi = 300) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required.")
  p <- .bs_prep(res, trap, radius, "none")   # never imagery here
  W <- p$xmax - p$xmin; H <- p$ymax - p$ymin
  pal <- resolve_palette(palette, res$categories)
  if (!isTRUE(patterns)) pal$pattern <- "none"
  cat_map <- res$categories
  legend <- if (isFALSE(legend)) character(0)
            else if (isTRUE(legend)) c("composition", "containers", "key")
            else match.arg(legend, c("composition", "containers", "key"),
                           several.ok = TRUE)

  lay <- p$lay
  # Clip to a square frame around the buffer. Polygons stay uncut anywhere near
  # the circle - they run well past it, as in Google Earth - but a very large
  # feature (a whole forest block, an expressway) is cut at the frame instead of
  # sprawling across the legend. The frame is the bbox of the 1.28 x view, so
  # the cut line sits ~14 m outside the buffer at its nearest point.
  if (!is.null(lay) && nrow(lay) > 0) {
    box <- sf::st_as_sfc(sf::st_bbox(p$view))
    cl  <- try(suppressWarnings(sf::st_intersection(lay, box)), silent = TRUE)
    if (!inherits(cl, "try-error") && nrow(cl) > 0) {
      gt <- as.character(sf::st_geometry_type(cl))
      if (any(gt == "GEOMETRYCOLLECTION")) {
        ex <- try(sf::st_collection_extract(cl, "POLYGON"), silent = TRUE)
        if (!inherits(ex, "try-error")) cl <- ex
      }
      cl <- cl[!sf::st_is_empty(cl), ]
      if (nrow(cl) > 0) lay <- cl
    }
  }
  # smallest on top: draw largest first so small features stay visible
  if (!is.null(lay) && nrow(lay) > 0) {
    lay$.area <- as.numeric(sf::st_area(lay))
    lay <- lay[order(-lay$.area), ]
  }

  # ---- unclassified residual inside the buffer ---------------------------
  na_hatch <- NULL
  if (isTRUE(show_unclassified)) {
    resid <- try({
      if (!is.null(lay) && nrow(lay) > 0) {
        u <- sf::st_union(sf::st_geometry(lay))
        sf::st_difference(sf::st_geometry(p$buf), u)
      } else sf::st_geometry(p$buf)
    }, silent = TRUE)
    if (!inherits(resid, "try-error") && length(resid) > 0 &&
        as.numeric(sum(sf::st_area(resid))) > 1)
      na_hatch <- .bs_hatch_df(resid, spacing = 2.4, angle = 45)
  }

  buf_df  <- .bs_poly_df(p$buf)
  trap_df <- .bs_pt_df(p$trap_d)
  seal_df <- if (!is.null(p$tk)) .bs_pt_df(p$tk[!p$tk$open, , drop = FALSE]) else NULL
  open_df <- if (!is.null(p$tk)) .bs_pt_df(p$tk[ p$tk$open, , drop = FALSE]) else NULL
  pool_df <- .bs_pt_df(p$pl)

  g <- ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = p$xmin, xmax = p$xmax,
                      ymin = p$ymin, ymax = p$ymax, fill = "#FFFFFF")

  # unclassified hatch sits under the polygons
  if (!is.null(na_hatch))
    g <- g + ggplot2::geom_path(data = na_hatch,
                                ggplot2::aes(x = x, y = y, group = grp),
                                colour = "#BBBBBB", linewidth = 0.28)

  # ---- polygons, one geom per category so fills stay literal --------------
  if (!is.null(lay) && nrow(lay) > 0) {
    for (i in seq_len(nrow(lay))) {
      cid <- lay$id_primary[i]
      row <- pal[pal$id == cid, ]
      fl  <- if (nrow(row)) row$fill[1] else "#CCCCCC"
      pt  <- if (nrow(row)) row$pattern[1] else "none"
      oc  <- .bs_darken(fl)
      df  <- .bs_poly_df(lay[i, ])
      if (is.null(df)) next
      g <- g + ggplot2::geom_polygon(data = df,
                                     ggplot2::aes(x = x, y = y, group = grp),
                                     fill = fl, alpha = fill_alpha,
                                     colour = oc, linewidth = 0.34)
      if (pt != "none") {
        tx <- .bs_texture(sf::st_geometry(lay[i, ]), pt)
        if (!is.null(tx$dots))
          g <- g + ggplot2::geom_point(data = tx$dots,
                                       ggplot2::aes(x = x, y = y),
                                       colour = oc, size = 0.42, stroke = 0)
        if (!is.null(tx$lines))
          g <- g + ggplot2::geom_path(data = tx$lines,
                                      ggplot2::aes(x = x, y = y, group = grp),
                                      colour = oc, linewidth = 0.26)
      }
    }
  }

  # Optional numeric labels on the largest polygons. With many classes, colour
  # alone is a weak cue; a printed id makes the largest features unambiguous
  # regardless of palette or colour vision.
  if (label_ids > 0 && !is.null(lay) && nrow(lay) > 0) {
    nn  <- min(label_ids, nrow(lay))
    big <- lay[seq_len(nn), ]
    ct  <- suppressWarnings(sf::st_coordinates(
      sf::st_centroid(sf::st_geometry(big))))
    if (!is.null(ct) && nrow(ct) > 0)
      g <- g + ggplot2::geom_label(
        data = data.frame(x = ct[, 1], y = ct[, 2],
                          lab = as.character(big$id_primary)),
        ggplot2::aes(x = x, y = y, label = lab),
        size = 2.6, fontface = "bold", colour = "grey10",
        fill = "white", alpha = 0.85, label.size = 0.15,
        label.padding = ggplot2::unit(0.10, "lines"))
  }

  if (!is.null(buf_df))
    g <- g + ggplot2::geom_path(data = buf_df,
                                ggplot2::aes(x = x, y = y, group = grp),
                                colour = "grey25", linewidth = 0.9, linetype = "22")
  if (!is.null(seal_df))
    g <- g + ggplot2::geom_point(data = seal_df, ggplot2::aes(x = x, y = y),
                                 shape = 22, size = 2.1, fill = "#2E86C1",
                                 colour = "#0B3C5D", stroke = 0.8)
  if (!is.null(open_df))
    g <- g + ggplot2::geom_point(data = open_df, ggplot2::aes(x = x, y = y),
                                 shape = 24, size = 3.9, fill = "#FFC400",
                                 colour = "#4A2600", stroke = 0.9)
  if (!is.null(pool_df))
    g <- g + ggplot2::geom_point(data = pool_df, ggplot2::aes(x = x, y = y),
                                 shape = 23, size = 3.6, fill = "#17BEBB",
                                 colour = "#0B3C5D", stroke = 0.9)
  if (!is.null(trap_df)) {
    gl <- .bs_trap_glyph(trap_df$x[1], trap_df$y[1], radius * 0.040)
    g <- g +
      ggplot2::geom_polygon(data = gl$pad, ggplot2::aes(x = x, y = y),
                            fill = "#C8A165", colour = "#5E4423", linewidth = 0.45) +
      ggplot2::geom_polygon(data = gl$cup, ggplot2::aes(x = x, y = y),
                            fill = "#141414", colour = "white", linewidth = 0.55) +
      ggplot2::geom_polygon(data = gl$rim, ggplot2::aes(x = x, y = y),
                            fill = "#3A3A3A", colour = "white", linewidth = 0.45)
  }

  # ---- scale bar + north arrow -------------------------------------------
  sb <- .bs_scalebar_len(p, radius)
  sx <- p$xmax - 0.34 * W; sy <- p$ymin + 0.05 * H
  g <- g +
    ggplot2::annotate("rect", xmin = sx, xmax = sx + sb$len,
                      ymin = sy, ymax = sy + 0.007 * H, fill = "grey15") +
    ggplot2::annotate("text", x = sx + sb$len / 2, y = sy + 0.031 * H,
                      label = sb$label, colour = "grey15", size = 3) +
    ggplot2::annotate("text", x = p$xmin + 0.045 * W, y = p$ymax - 0.055 * H,
                      label = "N", colour = "grey15", fontface = 2, size = 4.3) +
    ggplot2::annotate("segment", x = p$xmin + 0.045 * W, xend = p$xmin + 0.045 * W,
                      y = p$ymax - 0.100 * H, yend = p$ymax - 0.072 * H,
                      colour = "grey15", linewidth = 0.7,
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"),
                                             ends = "first"))

  # ---- legend -------------------------------------------------------------
  ba   <- as.numeric(sf::st_area(p$buf))
  allc <- res$long[res$long$Ovitrap_ID == trap & res$long$radius_m == radius, ]
  pos  <- allc[allc$area_m2 > 0, ]
  pos  <- pos[order(-pos$area_m2), ]
  summ <- utils::head(pos, top_n)
  tkc  <- res$tanks[res$tanks$Ovitrap_ID == trap & res$tanks$radius_m == radius, ]
  npool <- if ("pool_count" %in% names(tkc)) as.integer(tkc$pool_count[1]) else 0L
  lab   <- if ("label_en" %in% names(summ)) summ$label_en else summ$full_name

  rows <- list()
  addR <- function(txt, val = "", kind = "row", id = NA)
    rows[[length(rows) + 1]] <<- list(txt = txt, val = val, kind = kind, id = id)

  if ("composition" %in% legend) {
  addR("SURFACE COMPOSITION", "", "head")
  if (nrow(summ) == 0) addR("no classified polygons")
  for (i in seq_len(nrow(summ)))
    addR(substr(lab[i], 1, 32),
         sprintf("%6.0f m2 %5.1f%%", summ$area_m2[i], 100 * summ$area_m2[i] / ba),
         "cat", summ$id[i])
  oth <- sum(allc$area_m2) - sum(summ$area_m2)
  if (oth > 0.5)
    addR("other categories", sprintf("%6.0f m2 %5.1f%%", oth, 100 * oth / ba))
  addR("TOTAL CLASSIFIED",
       sprintf("%6.0f m2 %5.1f%%", sum(allc$area_m2), 100 * sum(allc$area_m2) / ba),
       "bold")
  if (isTRUE(show_unclassified)) {
    ncl <- max(0, ba - min(sum(allc$area_m2), ba))
    addR("unclassified (hatched)", sprintf("%6.0f m2 %5.1f%%", ncl, 100 * ncl / ba), "na")
  }
  }
  if ("containers" %in% legend) {
  addR("", "", "blank")
  addR("POINT FEATURES", "", "head")
  addR("sealed tank", sprintf("%6d", tkc$tank_sealed[1]), "seal")
  addR("unsealed tank (oviposition risk)", sprintf("%6d", tkc$tank_open[1]), "open")
  addR("swimming pool", sprintf("%6d", npool), "pool")
  addR("TOTAL", sprintf("%6d", tkc$tank_total[1] + npool), "bold")
  }
  if ("key" %in% legend) {
    # The texture key is generated from the palette actually in use, so it stays
    # correct for a custom dictionary or a different scheme rather than
    # describing whatever the built-in schema happens to do.
    pat_now <- pal[pal$id %in% pos$id & pal$pattern != "none", , drop = FALSE]
    nm <- c(diag = "diagonal", diag2 = "reverse diagonal", cross = "crosshatch",
            dots = "dots", dots2 = "dense dots", horiz = "horizontal",
            vert = "vertical", grid = "grid")
    if (nrow(pat_now) > 0 || isTRUE(show_unclassified)) {
      addR("", "", "blank")
      addR("TEXTURE KEY", "", "head")
      if (nrow(pat_now) > 0) {
        pl <- cat_map$label_en[match(pat_now$id, cat_map$id)]
        ord <- order(pl)
        for (k in ord)
          addR(sprintf("%-11s %s", nm[[pat_now$pattern[k]]],
                       substr(pl[k], 1, 20)), "")
      }
      if (isTRUE(show_unclassified)) addR("grey hatch  not classified", "")
    }
  }

  n <- length(rows)
  if (n == 0) {
    # legend suppressed: the map fills the panel and there is no legend column
    vx <- p$xmax
    leg <- NULL
  } else {
  gap  <- 0.06 * W; lw <- 0.80 * W
  lx   <- p$xmax + gap; vx <- lx + lw
  step <- H / (max(n, 16) + 2)
  ly   <- p$ymax - step * (seq_len(n) - 0.2)

  leg <- data.frame(
    x = lx, y = ly,
    txt = vapply(rows, function(z) z$txt, character(1)),
    val = vapply(rows, function(z) z$val, character(1)),
    kind = vapply(rows, function(z) z$kind, character(1)),
    id  = vapply(rows, function(z) as.numeric(z$id), numeric(1)),
    stringsAsFactors = FALSE)
  leg$face <- ifelse(leg$kind %in% c("head", "bold"), 2, 1)
  leg$col  <- ifelse(leg$kind == "head", "#111111", "#333333")

  g <- g +
    ggplot2::geom_text(data = leg, ggplot2::aes(x = x, y = y, label = txt),
                       hjust = 0, size = 3.0, family = "mono",
                       fontface = leg$face, colour = leg$col) +
    ggplot2::geom_text(data = leg, ggplot2::aes(x = vx, y = y, label = val),
                       hjust = 1, size = 3.0, family = "mono",
                       fontface = leg$face, colour = "#333333")

  hd <- leg[leg$kind == "head", ]
  if (nrow(hd) > 0)
    g <- g + ggplot2::annotate("segment", x = lx, xend = vx,
                               y = hd$y + step * 0.42, yend = hd$y + step * 0.42,
                               colour = "#CCCCCC", linewidth = 0.3)

  # colour swatches for the category rows
  sw <- leg[leg$kind == "cat", ]
  if (nrow(sw) > 0) {
    swx <- lx - 0.032 * W; hw <- 0.011 * W; hh2 <- step * 0.30
    for (k in seq_len(nrow(sw))) {
      row <- pal[pal$id == sw$id[k], ]
      fl  <- if (nrow(row)) row$fill[1] else "#CCCCCC"
      g <- g + ggplot2::annotate("rect",
                                 xmin = swx - hw, xmax = swx + hw,
                                 ymin = sw$y[k] - hh2, ymax = sw$y[k] + hh2,
                                 fill = fl, alpha = fill_alpha,
                                 colour = .bs_darken(fl), linewidth = 0.3)
    }
  }
  # marker glyphs
  gl <- leg[leg$kind %in% c("seal", "open", "pool", "na"), ]
  if (nrow(gl) > 0) {
    gx <- lx - 0.032 * W
    for (k in seq_len(nrow(gl))) {
      kd <- gl$kind[k]
      if (kd == "na") {
        g <- g + ggplot2::annotate("segment",
                                   x = gx - 0.011 * W, xend = gx + 0.011 * W,
                                   y = gl$y[k] - step * 0.22,
                                   yend = gl$y[k] + step * 0.22,
                                   colour = "#BBBBBB", linewidth = 0.4)
      } else {
        sp <- switch(kd, seal = 22, open = 24, pool = 23)
        fl <- switch(kd, seal = "#2E86C1", open = "#FFC400", pool = "#17BEBB")
        oc <- switch(kd, seal = "#0B3C5D", open = "#4A2600", pool = "#0B3C5D")
        sz <- switch(kd, seal = 2.4, open = 3.4, pool = 3.2)
        g <- g + ggplot2::annotate("point", x = gx, y = gl$y[k], shape = sp,
                                   fill = fl, colour = oc, size = sz, stroke = 0.8)
      }
    }
  }

  }

  g <- g +
    ggplot2::coord_fixed(1, xlim = c(p$xmin, vx + 0.02 * W),
                         ylim = c(p$ymin, p$ymax), expand = FALSE) +
    ggplot2::labs(
      title = if (is.null(title))
        sprintf("%s \u2014 land cover within %g m", trap, radius) else title,
      subtitle = if (is.null(subtitle))
        sprintf("%d classified polygons  |  %.1f%% of buffer classified  |  %d point features  |  overlapping polygons drawn largest-first",
                nrow(pos), 100 * sum(allc$area_m2) / ba,
                tkc$tank_total[1] + npool) else subtitle,
      caption = caption) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(face = "bold", size = 14),
                   plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35"),
                   plot.margin = ggplot2::margin(9, 9, 7, 9),
                   plot.background = ggplot2::element_rect(fill = "white",
                                                           colour = NA))

  if (!is.null(file)) {
    ok <- try(ggplot2::ggsave(file, g, width = width, height = height,
                              dpi = dpi, limitsize = FALSE, bg = "white"),
              silent = TRUE)
    if (inherits(ok, "try-error"))
      warning("category map failed for ", trap, ": ",
              trimws(as.character(ok)), call. = FALSE)
    else message("map written: ", file)
  }
  g
}


# =============================================================================
# map_combined() : publication figure for one trap
# -----------------------------------------------------------------------------
# Deliberately built WITHOUT geom_sf, coord_sf, patchwork or tidyterra.
# Every one of those has proved version-fragile in this project:
#   * patchwork < 1.3 with ggplot2 >= 3.5 throws Ops.data.frame in guide code
#   * coord_sf renegotiates CRS at render time and can silently drop layers
#   * zero-row sf layers (e.g. a trap with no unsealed tanks) can blank a panel
# Instead: geometry is converted to plain x/y data frames and drawn with
# geom_polygon / geom_path / geom_point under coord_fixed(). This is the most
# version-stable combination in ggplot2 and cannot lose layers to CRS logic.
# The legend is drawn as text inside the same panel, so there is no composition
# step and no ggplot guide at all.
#
# basemap:
#   "esri"            Esri World Imagery   (needs maptiles only)
#   <path to raster>  your own georeferenced image (needs terra only)
#   "none"            no imagery
# =============================================================================

# ---- sf geometry -> plain data.frame ---------------------------------------


#' Combined imagery and land-cover figure for one sampling site
#'
#' Imagery, polygons and legend in a single panel.
#'
#' @inheritParams map_composition
#' @param basemap,zoom See [map_basemap()].
#' @param poly_col Outline and fill colour used for every polygon.
#' @return A \pkg{ggplot} object.
#' @export
map_combined <- function(res, trap,
                        radius   = max(res$long$radius_m),
                        basemap  = "none",
                        zoom     = NA,
                        top_n    = 9,
                        poly_col = "#E02020",
                        file = NULL, width = 13, height = 7.6, dpi = 300) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for map_combined().")

  trap_sf <- res$traps[res$traps$Name == trap, ]
  if (nrow(trap_sf) == 0) stop("Trap '", trap, "' not found.")

  # ---- CRS to draw in -----------------------------------------------------
  # With Esri tiles we draw in their native Web Mercator so the raster is never
  # resampled and cannot slide under the polygons; the vectors move instead.
  esri     <- identical(basemap, "esri")
  draw_crs <- if (esri) sf::st_crs(3857) else sf::st_crs(trap_sf)
  rp <- function(x) if (!is.null(x) && nrow(x) > 0) sf::st_transform(x, draw_crs) else x

  trap_d <- rp(trap_sf)
  buf    <- sf::st_buffer(trap_d, radius)
  view   <- sf::st_buffer(trap_d, radius * 1.28)
  bbv    <- sf::st_bbox(view)
  xmin <- as.numeric(bbv["xmin"]); xmax <- as.numeric(bbv["xmax"])
  ymin <- as.numeric(bbv["ymin"]); ymax <- as.numeric(bbv["ymax"])
  W <- xmax - xmin; H <- ymax - ymin

  # ---- select THIS trap's elements (shared pools are split geometrically) --
  buf_in <- function(layer) {
    if (is.null(layer) || nrow(layer) == 0) return(NULL)
    b <- sf::st_transform(buf, sf::st_crs(layer))
    sel <- layer[lengths(sf::st_intersects(layer, b)) > 0, ]
    if (nrow(sel) == 0) NULL else rp(sel)
  }
  lay <- buf_in(res$polygons)     # polygons intersecting the buffer, drawn whole
  tk  <- buf_in(res$tanks_sf)     # tanks inside the buffer
  pl  <- buf_in(res$pools_sf)     # pools inside the buffer

  poly_df  <- .bs_poly_df(lay)
  buf_df   <- .bs_poly_df(buf)
  trap_df  <- .bs_pt_df(trap_d)
  seal_df  <- if (!is.null(tk)) .bs_pt_df(tk[!tk$open, , drop = FALSE]) else NULL
  open_df  <- if (!is.null(tk)) .bs_pt_df(tk[ tk$open, , drop = FALSE]) else NULL
  pool_df  <- .bs_pt_df(pl)

  # ---- basemap as a plain raster data.frame (no tidyterra, no coord_sf) ---
  bm_df <- NULL; zoom_used <- NA
  if (esri) {
    if (requireNamespace("maptiles", quietly = TRUE) &&
        requireNamespace("terra", quietly = TRUE)) {
      for (z in if (is.na(zoom)) 20:17 else zoom) {
        tl <- try(suppressWarnings(maptiles::get_tiles(
          view, provider = "Esri.WorldImagery",
          crop = TRUE, zoom = z)), silent = TRUE)
        if (!inherits(tl, "try-error") && !is.null(tl)) {
          rr <- try({
            # cap size so the data.frame stays manageable
            npx <- terra::ncell(tl)
            if (npx > 1.6e6) tl <- terra::aggregate(tl, fact = ceiling(sqrt(npx / 1.6e6)))
            d <- terra::as.data.frame(tl, xy = TRUE, na.rm = FALSE)
            names(d)[1:2] <- c("x", "y")
            b <- d[, 3:min(5, ncol(d)), drop = FALSE]
            b[] <- lapply(b, function(v) { v[is.na(v)] <- 0; pmin(pmax(v, 0), 255) })
            while (ncol(b) < 3) b[[ncol(b) + 1]] <- b[[1]]
            d$hex <- grDevices::rgb(b[[1]], b[[2]], b[[3]], maxColorValue = 255)
            d[, c("x", "y", "hex")]
          }, silent = TRUE)
          if (!inherits(rr, "try-error")) { bm_df <- rr; zoom_used <- z; break }
        }
      }
      if (is.null(bm_df))
        warning("Could not fetch Esri tiles for ", trap,
                " - drawing without imagery.", call. = FALSE)
    } else {
      warning("basemap = 'esri' needs maptiles + terra:\n",
              '  install.packages(c("maptiles", "terra"))\n',
              "Drawing without imagery.", call. = FALSE)
    }
  } else if (!identical(basemap, "none")) {
    if (!file.exists(basemap)) {
      warning("basemap file not found: ", basemap, call. = FALSE)
    } else if (requireNamespace("terra", quietly = TRUE)) {
      rr <- try({
        r0 <- terra::rast(basemap)
        if (!is.na(terra::crs(r0)) && nzchar(terra::crs(r0)))
          r0 <- terra::project(r0, paste0("EPSG:", draw_crs$epsg))
        r0 <- terra::crop(r0, terra::ext(xmin, xmax, ymin, ymax))
        npx <- terra::ncell(r0)
        if (npx > 1.6e6) r0 <- terra::aggregate(r0, fact = ceiling(sqrt(npx / 1.6e6)))
        d <- terra::as.data.frame(r0, xy = TRUE, na.rm = FALSE)
        names(d)[1:2] <- c("x", "y")
        b <- d[, 3:min(5, ncol(d)), drop = FALSE]
        b[] <- lapply(b, function(v) { v[is.na(v)] <- 0; pmin(pmax(v, 0), 255) })
        while (ncol(b) < 3) b[[ncol(b) + 1]] <- b[[1]]
        d$hex <- grDevices::rgb(b[[1]], b[[2]], b[[3]], maxColorValue = 255)
        d[, c("x", "y", "hex")]
      }, silent = TRUE)
      if (inherits(rr, "try-error"))
        warning("Could not read ", basemap, " - drawing without imagery.", call. = FALSE)
      else bm_df <- rr
    }
  }

  img <- !is.null(bm_df)

  # ---- palette flips with the background ---------------------------------
  panel_bg <- "#FBFBF9"
  ink      <- if (img) "white"   else "grey15"
  buf_col  <- if (img) "white"   else "grey25"
  seal_out <- if (img) "#DFF3FF" else "#0B3C5D"
  trap_out <- if (img) "white"   else "grey10"
  p_alpha  <- if (img) 0.10      else 0.20
  p_lw     <- if (img) 0.42      else 0.34

  # ---- legend content -----------------------------------------------------
  ba   <- as.numeric(sf::st_area(sf::st_buffer(rp(trap_sf), radius)))
  allc <- res$long[res$long$Ovitrap_ID == trap & res$long$radius_m == radius, ]
  pos  <- allc[allc$area_m2 > 0, ]
  pos  <- pos[order(-pos$area_m2), ]
  summ <- utils::head(pos, top_n)
  tkc  <- res$tanks[res$tanks$Ovitrap_ID == trap & res$tanks$radius_m == radius, ]
  di   <- res$distances[res$distances$Ovitrap_ID == trap, , drop = FALSE]
  lab  <- if ("label_en" %in% names(summ)) summ$label_en else summ$full_name
  npool <- if ("pool_count" %in% names(tkc)) as.integer(tkc$pool_count[1]) else 0L

  L <- list()
  addL <- function(txt, val = "", kind = "row")
    L[[length(L) + 1]] <<- list(txt = txt, val = val, kind = kind)

  addL("SURFACE COMPOSITION", "", "head")
  if (nrow(summ) == 0) addL("no classified polygons")
  for (i in seq_len(nrow(summ)))
    addL(substr(lab[i], 1, 33),
         sprintf("%6.0f m2  %5.1f%%", summ$area_m2[i], 100 * summ$area_m2[i] / ba))
  oth <- sum(allc$area_m2) - sum(summ$area_m2)
  if (oth > 0.5) addL("other categories",
                      sprintf("%6.0f m2  %5.1f%%", oth, 100 * oth / ba))
  addL("TOTAL CLASSIFIED",
       sprintf("%6.0f m2  %5.1f%%", sum(allc$area_m2), 100 * sum(allc$area_m2) / ba),
       "bold")
  addL("", "", "blank")
  addL("WATER CONTAINERS", "", "head")
  addL("sealed tank", sprintf("%6d", tkc$tank_sealed[1]), "seal")
  addL("unsealed tank (oviposition risk)", sprintf("%6d", tkc$tank_open[1]), "open")
  addL("swimming pool", sprintf("%6d", npool), "pool")
  addL("TOTAL", sprintf("%6d", tkc$tank_total[1] + npool), "bold")
  dc <- setdiff(names(di), "Ovitrap_ID")
  if (nrow(di) > 0 && length(dc)) {
    keep <- dc[!is.na(unlist(di[1, dc, drop = TRUE]))]
    if (length(keep)) {
      addL("", "", "blank")
      addL("LINEAR DISTANCE TO REFERENCE FEATURES", "", "head")
      for (k in keep)
        addL(sub("^dist_", "", k), sprintf("%6.0f m", as.numeric(di[1, k])))
    }
  }

  n  <- length(L)
  gap  <- 0.06 * W                       # gutter between map and legend
  lw   <- 0.74 * W                       # legend column width
  lx   <- xmax + gap                     # legend text left edge
  vx   <- lx + lw                        # value right edge
  step <- H / (max(n, 14) + 2)
  ly   <- ymax - step * (seq_len(n) - 0.2)

  leg <- data.frame(
    x = lx, y = ly,
    txt = vapply(L, function(z) z$txt, character(1)),
    val = vapply(L, function(z) z$val, character(1)),
    kind = vapply(L, function(z) z$kind, character(1)),
    stringsAsFactors = FALSE)
  leg$face <- ifelse(leg$kind %in% c("head", "bold"), 2, 1)
  leg$col  <- ifelse(leg$kind == "head", "#111111", "#333333")

  # ---- scale bar ----------------------------------------------------------
  sb <- .bs_scalebar_len(list(esri = esri, trap_d = trap_d), radius)
  sl <- sb$len
  sx <- xmax - 0.30 * W; sy <- ymin + 0.05 * H

  # ---- build the single panel --------------------------------------------
  g <- ggplot2::ggplot()

  if (img)
    g <- g + ggplot2::geom_raster(data = bm_df,
                                  ggplot2::aes(x = x, y = y, fill = hex)) +
      ggplot2::scale_fill_identity()
  else
    g <- g + ggplot2::annotate("rect", xmin = xmin, xmax = xmax,
                               ymin = ymin, ymax = ymax, fill = panel_bg)

  if (!is.null(poly_df))
    g <- g + ggplot2::geom_polygon(data = poly_df,
                                   ggplot2::aes(x = x, y = y, group = grp),
                                   fill = poly_col, alpha = p_alpha,
                                   colour = poly_col, linewidth = p_lw)
  if (!is.null(buf_df))
    g <- g + ggplot2::geom_path(data = buf_df,
                                ggplot2::aes(x = x, y = y, group = grp),
                                colour = buf_col, linewidth = 0.9, linetype = "22")
  if (!is.null(seal_df))
    g <- g + ggplot2::geom_point(data = seal_df, ggplot2::aes(x = x, y = y),
                                 shape = 22, size = 2.1, fill = "#2E86C1",
                                 colour = seal_out, stroke = 0.8)
  if (!is.null(open_df))
    g <- g + ggplot2::geom_point(data = open_df, ggplot2::aes(x = x, y = y),
                                 shape = 24, size = 3.9, fill = "#FFC400",
                                 colour = "#4A2600", stroke = 0.9)
  if (!is.null(pool_df))
    g <- g + ggplot2::geom_point(data = pool_df, ggplot2::aes(x = x, y = y),
                                 shape = 23, size = 3.6, fill = "#17BEBB",
                                 colour = "#0B3C5D", stroke = 0.9)
  if (!is.null(trap_df)) {
    g <- g + ggplot2::geom_point(data = trap_df, ggplot2::aes(x = x, y = y),
                                 shape = 21, size = 5.2, fill = "#00C88C",
                                 colour = trap_out, stroke = 1.4) +
      ggplot2::annotate("text", x = trap_df$x[1], y = trap_df$y[1] + 0.040 * H,
                        label = trap, colour = ink, fontface = 2, size = 4.1)
  }

  g <- g +
    # scale bar + north arrow
    ggplot2::annotate("rect", xmin = sx, xmax = sx + sl,
                      ymin = sy, ymax = sy + 0.007 * H, fill = ink) +
    ggplot2::annotate("text", x = sx + sl / 2, y = sy + 0.031 * H,
                      label = sb$label, colour = ink, size = 3) +
    ggplot2::annotate("text", x = xmin + 0.045 * W, y = ymax - 0.055 * H,
                      label = "N", colour = ink, fontface = 2, size = 4.3) +
    ggplot2::annotate("segment", x = xmin + 0.045 * W, xend = xmin + 0.045 * W,
                      y = ymax - 0.100 * H, yend = ymax - 0.072 * H,
                      colour = ink, linewidth = 0.7,
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"),
                                             ends = "first")) +
    # legend text, inside the same panel - no composition, no guides
    ggplot2::geom_text(data = leg,
                       ggplot2::aes(x = x, y = y, label = txt),
                       hjust = 0, size = 3.05, family = "mono",
                       fontface = leg$face, colour = leg$col) +
    ggplot2::geom_text(data = leg,
                       ggplot2::aes(x = vx, y = y, label = val),
                       hjust = 1, size = 3.05, family = "mono",
                       fontface = leg$face, colour = "#333333")

  # rules under section headers
  hd <- leg[leg$kind == "head", ]
  if (nrow(hd) > 0)
    g <- g + ggplot2::annotate("segment", x = lx, xend = vx,
                               y = hd$y + step * 0.42, yend = hd$y + step * 0.42,
                               colour = "#CCCCCC", linewidth = 0.3)
  # legend glyphs, matching the map exactly
  gl <- leg[leg$kind %in% c("seal", "open", "pool"), ]
  if (nrow(gl) > 0) {
    gx <- lx - 0.030 * W
    for (k in seq_len(nrow(gl))) {
      sp <- switch(gl$kind[k], seal = 22, open = 24, pool = 23)
      fl <- switch(gl$kind[k], seal = "#2E86C1", open = "#FFC400", pool = "#17BEBB")
      oc <- switch(gl$kind[k], seal = "#0B3C5D", open = "#4A2600", pool = "#0B3C5D")
      sz <- switch(gl$kind[k], seal = 2.4, open = 3.4, pool = 3.2)
      g <- g + ggplot2::annotate("point", x = gx, y = gl$y[k], shape = sp,
                                 fill = fl, colour = oc, size = sz, stroke = 0.8)
    }
  }

  g <- g +
    ggplot2::coord_fixed(ratio = 1, xlim = c(xmin, vx + 0.02 * W),
                         ylim = c(ymin, ymax), expand = FALSE) +
    ggplot2::labs(
      title = sprintf("Ovitrap %s \u2014 urban morphology within %g m", trap, radius),
      subtitle = sprintf("%d classified polygons  |  %.1f%% of buffer classified  |  %d water containers  |  kernel %s, lambda = %s m",
                         nrow(pos), 100 * sum(allc$area_m2) / ba,
                         tkc$tank_total[1] + npool,
                         res$meta$value[res$meta$key == "kernel"],
                         res$meta$value[res$meta$key == "lambda_m"]),
      caption = if (img)
        sprintf("Imagery: Esri World Imagery (zoom %s). Analysis in EPSG:%s.",
                zoom_used, res$meta$value[res$meta$key == "epsg"])
      else 'No imagery \u2014 install.packages(c("maptiles","terra")) then set BASEMAP <- "esri"') +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35"),
      plot.caption  = ggplot2::element_text(size = 7.5, colour = "grey50", hjust = 1),
      plot.margin   = ggplot2::margin(9, 9, 7, 9))

  if (!is.null(file)) {
    ok <- try(ggplot2::ggsave(file, g, width = width, height = height,
                              dpi = dpi, limitsize = FALSE), silent = TRUE)
    if (inherits(ok, "try-error"))
      warning("Could not save map for ", trap, ": ",
              trimws(as.character(ok)), call. = FALSE)
    else message("map written: ", file)
  }
  g
}


