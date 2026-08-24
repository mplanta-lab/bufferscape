# bufferscape 1.2.0

## The nomenclature now matches the published Table 1

`mare_categories` previously held 29 classes in 12 groups. The article and
protocol define **28 classes in nine thematic groups**, with class 29 (PVC
roofing) removed once the nomenclature stabilised and class 24 revised. The
dataset is now identical to Table 1: same identifiers, same English labels, same
grouping. Identifiers stayed contiguous, so no surviving class was renumbered.

**This changes output column names.** Columns are built as
`id_category_description`, and `category` now carries the nine thematic groups
(`roofing`, `vegetation`, `circulation_parking`, ...) rather than the earlier
twelve Portuguese labels. Analyses that referred to the old column names must be
updated. Areas, weights and counts are unchanged.

Any data still coded with class 29 will be reported as an unrecognised code
rather than silently dropped, which is the intended behaviour.

# bufferscape 1.1.0

## GeoPackage support

* `buffer_composition()` and `batch_composition()` now read **GeoPackage** as
  well as KML. The format is detected from the file's contents rather than its
  extension, and a folder may mix the two. A file may contain one site or
  several in either format; nothing needs to be configured.
* A GeoPackage is expected to carry `focal_points` and `landcover` layers, and
  optionally `buffers`, `water_containers`, `poi_lines` and `class_dictionary`.
  Anything else falls back to the flat reader used for KML.
* The same site read from KML and from GeoPackage returns identical areas,
  weights, container counts and reference distances; this is enforced by a test.

## Fixes

* A buffer polygon digitised under the site's own name (`NH_6` rather than
  `NH_6_Buffer`) was treated as an unclassified land-cover polygon and dropped
  with a warning. It is now recognised as reference geometry, so the buffer
  cross-check works on these files.
* `bufferscape::class_dictionary()` failed with "object 'mare_categories' not
  found" unless the package had been attached with `library()`. Lazy-loaded data
  is not on the namespace search path; the lookup is now explicit.
* The per-radius sheets written by `batch_composition()` were missing the
  `_d_nearest_m` proximity columns.

## Renamed arguments

* `buffer_composition(kml = )` is now `path = `, and
  `batch_composition(kml_dir = )` is now `dir = `, because neither is
  KML-specific any more. The old names still work and warn.

# bufferscape 1.0.3

* Fixed a bug in `batch_composition()`: requesting a basemap with
  `make_maps = TRUE` failed with "could not find function basemap_ready". The
  helper that checks whether the imagery packages are installed had been lost in
  a refactor while its call site remained.
* Declared the non-standard-evaluation column names used by \pkg{dplyr} and
  \pkg{ggplot2}, so `R CMD check` no longer reports undefined globals.

# bufferscape 1.0.2

* Progress output from `batch_composition()` now goes through `message()` rather
  than `cat()`, so a run can be silenced with `suppressMessages()`. CRAN policy
  asks that packages not write to the console in a way the user cannot suppress.
* The archive DOI is recorded in `README.md`, `CITATION.cff` and
  `inst/CITATION`, so `citation("bufferscape")` returns it.

# bufferscape 1.0.1

* Version strings in `DESCRIPTION`, `CITATION.cff` and `inst/CITATION` now match
  the release tag; 1.0.0 was tagged while the package still declared 0.1.0.
* Added `CITATION.cff`, so the repository exposes citation metadata to GitHub
  and to archives such as Zenodo.

# bufferscape 1.0.0

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
