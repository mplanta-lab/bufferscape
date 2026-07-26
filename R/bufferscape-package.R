#' @keywords internal
#' @aliases bufferscape-package
"_PACKAGE"

#' @importFrom dplyr %>%
#' @importFrom stats aggregate setNames
#' @importFrom utils head read.csv read.delim packageVersion
#' @importFrom grDevices col2rgb colorRampPalette rgb
NULL

# quiet R CMD check on tidy-eval column names
utils::globalVariables(c(
  "Ovitrap_ID", "radius_m", "full_name", "area_m2", "area_w", "area_w_centroid",
  "id", "id_primary", "id_secondary", "label_en", "category", "description",
  "Name", "Description", "length_m", "target", "tank_open", "tank_sealed",
  "tank_total", "pool_count", "coverage_ratio", "n_polygons", "area_exact",
  "buffer_area", "n", "type", "grp", "hex", "x", "y", "vx", "pct", "bold",
  "txt", "val", "t", "source_file", "dominant_area_m2", "kind", "open",
  "category_col", "desc_col", "id_str", "clean_desc", "mare_categories",
  ".", "d", "d_nearest_m", "dominant_category", "label", "name",
  "pct_classified", "r"
))
