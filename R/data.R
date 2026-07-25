#' Maré 29-class urban morphology dictionary
#'
#' The land-cover classification schema developed for oviposition-trap
#' surveillance in the Complexo da Maré favela complex, Rio de Janeiro. It
#' resolves distinctions that global land-cover products cannot at this scale:
#' fibrocement against sealed and unsealed concrete slab, individual water
#' containers, narrow alleys, polluted open channels, and active construction.
#'
#' The schema is a nested hierarchy -- macro land use, then vegetation
#' configuration, then building complexity -- which is why `category` and
#' `description` are separate columns.
#'
#' @format A data frame with 29 rows and 6 columns:
#' \describe{
#'   \item{id}{integer code written in the KML `<description>` field}
#'   \item{category}{coarse grouping, in Portuguese}
#'   \item{description}{specific class, in Portuguese}
#'   \item{label_en}{English label used on figures}
#'   \item{fill}{hex colour, chosen to echo the appearance in aerial imagery}
#'   \item{pattern}{overprinted texture: `"none"`, `"dots"`, `"dots2"`,
#'     `"diag"` or `"cross"`, used where classes share a base material}
#' }
#'
#' @source Planta M. Qualitative assessment schema for urban polygons in Rio de
#'   Janeiro favelas, interpreted from high-resolution aerial imagery.
#' @examples
#' head(mare_categories)
#' subset(mare_categories, pattern != "none")
"mare_categories"
