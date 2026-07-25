# bufferscape

<!-- badges: start -->
[![R-CMD-check](https://github.com/mplanta-lab/bufferscape/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mplanta-lab/bufferscape/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Distance-weighted landscape composition in buffers around point locations,
computed directly from vector polygons.

Given point locations and land-cover polygons, `bufferscape` returns for every
point and every class the exact area inside a buffer and a distance-decay
weighted *effective* area. Polygons may overlap. Point features are counted
separately, and distances to off-buffer reference features are measured.

It is built for **fine-scale** work, where the relevant neighbourhood is tens
to hundreds of metres and rasterising would destroy the features that matter --
a 3 m alley, a 2 m water tank, the edge between a roof and a canopy. Typical
designs:

| field | points | classes that matter |
|---|---|---|
| air-quality exposure, land-use regression | monitors, home addresses | road surface, industry, tree cover |
| environmental epidemiology | addresses in a cohort | greenspace, water, built surface |
| food environment | schools, homes | outlet types within walking distance |
| vector surveillance | ovitraps, light traps, tick drags | roofing, vegetation, standing water |
| WASH | water points, households | sanitation infrastructure, drainage |
| landscape ecology | camera traps, nest sites, quadrats | habitat classes, edge, canopy |

## Installation

```r
# install.packages("remotes")
remotes::install_github("mplanta-lab/bufferscape")
```

## Usage

```r
library(bufferscape)

kml <- system.file("extdata", "example_site.kml", package = "bufferscape")
res <- buffer_composition(kml, radii = 50)

head(res$long[res$long$area_m2 > 0, c("label_en", "area_m2", "area_w")])
```

Any radius works; scale bars on the figures adapt.

```r
res <- buffer_composition(kml, radii = c(100, 250, 500), lambda = 300)
```

A whole folder at once, writing a workbook, maps and charts:

```r
out <- batch_composition("path/to/kml", radii = c(20, 30, 40, 50))
```

## Why integrate the kernel over the polygon

Weighting a polygon by the distance to its centroid is cheap and, for compact
features, harmless. For an elongated feature passing close to the point it is
not: the centroid can sit almost on the point while most of the polygon lies
far away, so the entire area is weighted as if adjacent.

Measured on real data, the centroid approximation overstates the weighted area
of a road passing beside the sampling point by up to **45%**, while compact
roofs stay under 1%. Roads, drainage channels, alleys, rivers and field margins
are exactly the geometry that breaks it, and usually the features of interest.
`bufferscape` integrates the kernel over each polygon and reports the centroid
version alongside, so the bias can be quantified rather than assumed away.

## Palettes

Four schemes, or your own colours:

```r
map_composition(res, "SITE_1", palette = "aerial")      # appearance-matched
map_composition(res, "SITE_1", palette = "colorblind")  # colour-vision-safe
map_composition(res, "SITE_1", palette = "greyscale")   # print
map_composition(res, "SITE_1", palette = c("7" = "#FF00FF"))
```

A palette of 29 nominal colours cannot be made safe for colour-vision
deficiency; the space is not large enough. The `"colorblind"` scheme therefore
uses colour for the coarse **group** only and separates members within a group
by lightness and texture, so no class depends on hue alone. Counting texture as
a cue, it leaves **0 of 406 class pairs** ambiguous under simulated
deuteranopia, against 3 for the appearance-matched palette and 11 for viridis.

Maps and charts take the same `palette` argument, so a figure pair can be made
to match.

## Workbooks

```r
write_composition_report(res, "out.xlsx",
                         radii   = c(30, 50),
                         metrics = c("exact", "weighted"),
                         digits  = 2)
```

`metrics` matters: carrying all four metrics for 29 classes is 126 columns.
Dropping the centroid comparison when you are not doing the methods analysis
roughly halves that.

## Methods diagnostic

```r
bias <- centroid_bias(kml, radii = 50)
summarise_centroid_bias(bias)          # by polygon geometry
```

## Your own classification

```r
own <- data.frame(
  id          = 1:3,
  category    = c("water", "built", "vegetation"),
  description = c("pond", "roof", "canopy"),
  fill        = c("#2C7FB8", "#BDBDBD", "#31A354")
)
res <- buffer_composition(kml, categories = own)
```

Only `id`, `category` and `description` are required. `validate_dictionary()`
checks a dictionary before a long run. The 29-class schema used in the worked
example ships as `mare_categories`.

## Citation

```r
citation("bufferscape")
```

## License

MIT
