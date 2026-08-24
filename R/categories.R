#' Land-cover category dictionary
#'
#' A dictionary maps the numeric codes written in the `<description>` field of
#' each digitised polygon to a named land-cover category. Everything downstream
#' -- the area columns, the map palette, the legend labels -- is driven by this
#' table, so the workflow is not tied to any one classification scheme.
#'
#' The dictionary shipped with the package, [mare_categories], is the 29-class
#' schema developed for the Complexo da Maré ovitrap study. Supply your own to
#' work in a different setting.
#'
#' @param x One of:
#'   * `NULL` (default) -- the built-in [mare_categories] schema.
#'   * a `data.frame` -- validated and used as is.
#'   * a path to an `.xlsx` or `.csv` file -- read, then validated.
#'
#' @details
#' A dictionary must contain:
#'
#' \describe{
#'   \item{`id`}{integer, unique, positive. The code written in the KML
#'     `<description>` field.}
#'   \item{`category`}{character. A coarse grouping (for example `"cobertura"`,
#'     `"vegetacao"`). Used for aggregation and for column names.}
#'   \item{`description`}{character. The specific class within the grouping.}
#' }
#'
#' and may optionally contain:
#'
#' \describe{
#'   \item{`label_en`}{character. The label printed on maps, charts and
#'     legends. Defaults to `category description`.}
#'   \item{`fill`}{character. A hex colour for the map. If absent, a colour is
#'     generated. Supplying your own is strongly preferred for land cover,
#'     because a generated ramp assigns arbitrary hues that carry no meaning.}
#'   \item{`pattern`}{character, one of `"none"`, `"dots"`, `"dots2"`,
#'     `"diag"`, `"diag2"`, `"cross"`, `"horiz"`, `"vert"`, `"grid"`.
#'     Overprints a texture so that classes sharing a base colour stay
#'     distinguishable. Defaults to `"none"`.}
#' }
#'
#' A `full_name` column (`id_category_description`) is added automatically and
#' is what the output column names are built from.
#'
#' @return A `data.frame` with columns `id`, `category`, `description`,
#'   `label_en`, `fill`, `pattern` and `full_name`.
#'
#' @examples
#' # the built-in 29-class schema
#' head(class_dictionary())
#'
#' # a minimal custom dictionary
#' own <- data.frame(
#'   id          = 1:3,
#'   category    = c("water", "built", "vegetation"),
#'   description = c("pond", "roof", "canopy"),
#'   fill        = c("#2C7FB8", "#BDBDBD", "#31A354")
#' )
#' class_dictionary(own)
#'
#' @seealso [mare_categories] for the built-in schema.
#' @export
class_dictionary <- function(x = NULL) {
  if (is.null(x)) return(validate_dictionary(.bs_mare()))
  if (is.data.frame(x)) return(validate_dictionary(x))

  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) stop("Category file not found: ", x, call. = FALSE)
    ext <- tolower(tools::file_ext(x))
    tab <- switch(
      ext,
      xlsx = ,
      xls  = {
        if (!requireNamespace("readxl", quietly = TRUE))
          stop("readxl is needed to read '", basename(x), "'. ",
               "Install it, or pass a data.frame.", call. = FALSE)
        as.data.frame(readxl::read_excel(x))
      },
      csv  = utils::read.csv(x, stringsAsFactors = FALSE),
      tsv  = utils::read.delim(x, stringsAsFactors = FALSE),
      stop("Unsupported category file type: .", ext,
           ". Use .xlsx, .csv or .tsv, or pass a data.frame.", call. = FALSE)
    )
    return(validate_dictionary(tab))
  }

  stop("`categories` must be NULL, a data.frame, or a path to a ",
       ".xlsx/.csv/.tsv file.", call. = FALSE)
}

#' Validate and complete a category dictionary
#'
#' Checks the required columns, fills in the optional ones, and returns a
#' dictionary in canonical form. Exported so a custom dictionary can be checked
#' before a long batch run rather than failing partway through.
#'
#' @param x A `data.frame`.
#' @return The completed dictionary.
#' @examples
#' validate_dictionary(
#'   data.frame(id = 1:2, category = c("a", "b"), description = c("x", "y"))
#' )
#' @export
validate_dictionary <- function(x) {
  if (!is.data.frame(x))
    stop("A category dictionary must be a data.frame.", call. = FALSE)
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  need <- c("id", "category", "description")
  miss <- setdiff(need, names(x))
  if (length(miss))
    stop("Category dictionary is missing required column(s): ",
         paste(miss, collapse = ", "),
         ". Columns present: ", paste(names(x), collapse = ", "), call. = FALSE)

  if (nrow(x) == 0L)
    stop("Category dictionary has no rows.", call. = FALSE)

  x$id <- suppressWarnings(as.integer(x$id))
  if (anyNA(x$id))
    stop("Every `id` must be a whole number; ", sum(is.na(x$id)),
         " could not be coerced.", call. = FALSE)
  if (any(x$id <= 0L))
    stop("Every `id` must be positive.", call. = FALSE)
  if (anyDuplicated(x$id))
    stop("`id` must be unique; duplicated: ",
         paste(unique(x$id[duplicated(x$id)]), collapse = ", "), call. = FALSE)

  x$category    <- as.character(x$category)
  x$description <- as.character(x$description)
  if (any(is.na(x$category) | !nzchar(x$category)) ||
      any(is.na(x$description) | !nzchar(x$description)))
    stop("`category` and `description` must be non-empty for every row.",
         call. = FALSE)

  if (!"label_en" %in% names(x) || all(is.na(x$label_en)))
    x$label_en <- paste(x$category, x$description)
  x$label_en <- as.character(x$label_en)

  if (!"pattern" %in% names(x)) x$pattern <- "none"
  x$pattern[is.na(x$pattern) | !nzchar(x$pattern)] <- "none"
  ok_pat <- c("none", "dots", "dots2", "diag", "diag2", "cross",
              "horiz", "vert", "grid")
  bad <- setdiff(unique(x$pattern), ok_pat)
  if (length(bad))
    stop("Unknown pattern(s): ", paste(bad, collapse = ", "),
         ". Use one of: ", paste(ok_pat, collapse = ", "), call. = FALSE)

  if (!"fill" %in% names(x) || all(is.na(x$fill))) {
    # No colours supplied. A generated ramp is a fallback, not a good land-cover
    # palette: hues will be ordered by id and carry no ecological meaning.
    x$fill <- viridis_colours(nrow(x))
    attr(x, "fill_generated") <- TRUE
  } else {
    x$fill <- as.character(x$fill)
    gen <- is.na(x$fill) | !nzchar(x$fill)
    if (any(gen)) x$fill[gen] <- viridis_colours(nrow(x))[gen]
    bad <- x$fill[!grepl("^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", x$fill)]
    if (length(bad))
      stop("`fill` must be hex colours like \"#31A354\"; offending: ",
           paste(utils::head(bad, 3), collapse = ", "), call. = FALSE)
  }

  x$full_name <- paste(x$id, x$category, x$description, sep = "_")
  rownames(x) <- NULL
  x[, c("id", "category", "description", "label_en", "fill",
        "pattern", "full_name")]
}

#' Viridis colours without a package dependency
#'
#' Anchor colours sampled from the viridis colormap, interpolated with
#' [grDevices::colorRampPalette()]. Avoids depending on \pkg{viridisLite} for
#' what is a handful of hex codes.
#'
#' @param n Number of colours.
#' @return A character vector of `n` hex colours.
#' @examples
#' viridis_colours(5)
#' @export
viridis_colours <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 1L) stop("`n` must be a positive integer.", call. = FALSE)
  anchors <- c("#440154", "#482878", "#3E4A89", "#31688E", "#26828E",
               "#1F9E89", "#35B779", "#6DCD59", "#B4DE2C", "#FDE725")
  if (n == 1L) return(anchors[1])
  grDevices::colorRampPalette(anchors)(n)
}

#' Parse a polygon description into primary and secondary category codes
#'
#' Digitised polygons carry their class in the KML `<description>` field. A
#' single code (`"7"`) is unambiguous; a hyphenated pair (`"8-6"`) records a
#' primary and a secondary class where the interpreter was uncertain or the
#' surface is genuinely mixed.
#'
#' Naive coercion silently destroys these: `as.numeric("8-6")` is `NA`, and an
#' inner join on `NA` drops the polygon without warning. In the Maré data that
#' would discard roughly a fifth of all polygons, and non-randomly -- the
#' ambiguous ones are the mixed-material and transitional surfaces.
#'
#' @param x Character vector of description fields.
#' @return A [tibble][tibble::tibble] with `id_primary`, `id_secondary` and
#'   `n_codes`.
#' @examples
#' parse_class_codes(c("7", " 6 ", "8-6", "5 / 7", "", NA))
#' @export
parse_class_codes <- function(x) {
  x <- as.character(x)
  nums <- stringr::str_extract_all(x, "\\d+")
  # str_extract_all() returns a length-1 NA for an NA input, not an empty
  # vector, which would otherwise be counted as one code.
  nums <- lapply(nums, function(v) v[!is.na(v)])
  tibble::tibble(
    id_primary = vapply(nums, function(v)
      if (length(v) >= 1) as.integer(v[1]) else NA_integer_, integer(1)),
    id_secondary = vapply(nums, function(v)
      if (length(v) >= 2) as.integer(v[2]) else NA_integer_, integer(1)),
    n_codes = vapply(nums, length, integer(1))
  )
}


#' Fetch the built-in dictionary without relying on the search path
#'
#' Lazy-loaded datasets live in the package's data environment, which is not in
#' the lookup chain used by the package's own code. A bare reference therefore
#' works only when the package has been attached with `library()`, and fails for
#' `bufferscape::class_dictionary()`. This resolves it either way.
#'
#' @keywords internal
#' @noRd
.bs_mare <- function() {
  d <- get0("mare_categories", envir = asNamespace("bufferscape"),
            inherits = FALSE)
  if (!is.null(d)) return(d)
  e <- new.env(parent = emptyenv())
  utils::data("mare_categories", package = "bufferscape", envir = e)
  get("mare_categories", envir = e)
}
