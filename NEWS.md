# bufferscape 0.1.0

First release.

## Core

* `buffer_composition()` computes per-class surface area within buffers around
  point locations, directly from vector polygons. It returns the exact area, a
  distance-decay weighted area in which the kernel is **integrated over polygon
  geometry** (`area_w`), the centroid-weighted equivalent for comparison
  (`area_w_centroid`), and the distance to the nearest patch of each class
  (`d_nearest_m`).
* Overlapping polygons are supported; class areas are not constrained to sum to
  the buffer area.
* `decay_kernel()` provides bounded exponential and Gaussian kernels.
* `batch_composition()` processes a folder, writing a workbook and figures. A
  malformed file is logged and skipped rather than stopping the run.
* A KML may contain several sampling points; polygons and point features are
  assigned to each site geometrically, against that site's own buffer.

## Classification

* The dictionary is user-supplied. `class_dictionary()` accepts a `data.frame`
  or an `.xlsx`/`.csv` path; `validate_dictionary()` checks one before a long
  run. The 29-class Maré schema ships as `mare_categories`.
* `parse_class_codes()` keeps hyphenated codes such as `"8-6"`, which naive
  coercion would silently drop.

## Methods diagnostics

* `centroid_bias()` and `summarise_centroid_bias()` quantify, polygon by
  polygon, how far centroid weighting departs from the exact integral, with
  shape descriptors and a `centroid_inside` flag for concave features whose
  centroid falls outside the polygon.

## Figures

* `map_composition()` draws the land-cover map; `map_basemap()` and
  `map_combined()` add satellite imagery.
* `plot_site_composition()` and `plot_group_points()` give per-site composition
  bars and per-group point-feature bars.
* `class_palette()` provides four schemes: `"aerial"` (appearance-matched),
  `"colorblind"` (one colour-vision-safe hue per group, members separated by
  lightness and texture), `"viridis"` and `"greyscale"`. `resolve_palette()`
  also accepts a palette table or a named colour vector.

## Export

* `write_composition_report()` writes the workbook, with control over radii,
  which surface metrics to include, which sheets, label style and rounding.
