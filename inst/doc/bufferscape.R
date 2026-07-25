## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(bufferscape)

## -----------------------------------------------------------------------------
kml <- system.file("extdata", "example_site.kml", package = "bufferscape")
res <- buffer_composition(kml, radii = 50, grid_res = 5, verbose = FALSE)

## -----------------------------------------------------------------------------
d <- res$long[res$long$area_m2 > 0, c("label_en", "area_m2", "area_w")]
head(d[order(-d$area_m2), ], 6)

## -----------------------------------------------------------------------------
res$tanks

## -----------------------------------------------------------------------------
w <- res$long[res$long$area_m2 > 0, ]
w$bias_pct <- 100 * (w$area_w_centroid - w$area_w) / w$area_w
head(w[order(-abs(w$bias_pct)), c("label_en", "area_m2", "bias_pct")], 5)

## -----------------------------------------------------------------------------
decay_kernel(c(0, 25, 50, 100), kernel = "exponential", lambda = 45)

## ----eval = FALSE-------------------------------------------------------------
#  fits <- lapply(c(15, 30, 45, 60, 100), function(L) {
#    r <- buffer_composition(kml, radii = 50, lambda = L, verbose = FALSE)
#    # ... build the model matrix from r$wide and fit ...
#  })

## ----eval = FALSE-------------------------------------------------------------
#  # fine-scale: a few tens of metres
#  buffer_composition(kml, radii = c(20, 30, 40, 50))
#  
#  # neighbourhood scale, e.g. walkability or food environment
#  buffer_composition(kml, radii = c(100, 250, 500))
#  
#  # land-use regression around an air-quality monitor
#  buffer_composition(kml, radii = c(50, 100, 300, 1000), lambda = 300)

## -----------------------------------------------------------------------------
own <- data.frame(
  id          = 1:4,
  category    = c("water", "water", "built", "vegetation"),
  description = c("pond", "channel", "roof", "canopy"),
  label_en    = c("Standing water", "Drainage channel", "Roof", "Tree canopy"),
  fill        = c("#2C7FB8", "#41B6C4", "#BDBDBD", "#31A354"),
  pattern     = c("none", "none", "none", "dots")
)
class_dictionary(own)

## ----error = TRUE-------------------------------------------------------------
validate_dictionary(data.frame(id = c(1, 1), category = "a", description = "b"))

## ----eval = FALSE-------------------------------------------------------------
#  map_composition(res, "SITE_1", palette = "aerial")      # appearance-matched
#  map_composition(res, "SITE_1", palette = "colorblind")  # colour-vision-safe
#  map_composition(res, "SITE_1", palette = "greyscale")   # print

## -----------------------------------------------------------------------------
head(class_palette(scheme = "colorblind"), 4)

## ----eval = FALSE-------------------------------------------------------------
#  map_composition(res, "SITE_1", legend = FALSE) +
#    ggplot2::labs(title = "My title")

## ----eval = FALSE-------------------------------------------------------------
#  out <- batch_composition("path/to/kml/folder", radii = c(20, 30, 40, 50))
#  out$summary

## ----eval = FALSE-------------------------------------------------------------
#  write_composition_report(res, "composition.xlsx",
#                           radii   = c(30, 50),
#                           metrics = c("exact", "weighted"),
#                           digits  = 2)

