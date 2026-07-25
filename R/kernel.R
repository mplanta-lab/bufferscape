#' Distance-decay kernels
#'
#' Weights are bounded on \eqn{[0, 1]} with \eqn{w(0) = 1}, so a weighted area is
#' always between zero and the true area and is directly interpretable as an
#' "effective" area.
#'
#' This matters: the ratios `area / d` and `area / d^2` sometimes used for the
#' same purpose are not kernels. They diverge as a polygon approaches the
#' sampling point, have no upper bound, are not normalisable, and carry
#' uninterpretable units, so their coefficients cannot be compared across sites.
#'
#' @param d Numeric vector of distances, in the units of the projected CRS
#'   (metres for a UTM zone).
#' @param kernel One of `"exponential"` (default), `"gaussian"` or `"none"`.
#'   `"none"` returns 1 everywhere, giving unweighted areas.
#' @param lambda Decay scale, in the same units as `d`.
#'
#' @details
#' The default `lambda = 45` follows close-kin genetic estimates of mean
#' *Aedes aegypti* dispersal (Jasper et al. 2020, *BMC Biology*, 45.2 m,
#' 95% CI 39.7-51.3), consistent with a Brazilian mark-release-recapture
#' estimate of 52.8 m (Winskill et al. 2015). Fine-scale dispersal is
#' repeatedly described as exponential (Laplacian), which is why that is the
#' default form. For another taxon or another process, set `lambda` from the
#' relevant literature, and report a sensitivity analysis over a grid of values
#' compared by AIC.
#'
#' @return A numeric vector of weights the same length as `d`.
#' @examples
#' decay_kernel(c(0, 25, 50), lambda = 45)
#' decay_kernel(c(0, 25, 50), kernel = "gaussian", lambda = 45)
#' @export
decay_kernel <- function(d, kernel = "exponential", lambda = 45) {
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) || lambda <= 0)
    stop("`lambda` must be a single positive number.", call. = FALSE)
  switch(
    kernel,
    exponential = exp(-d / lambda),
    gaussian    = exp(-(d^2) / (2 * lambda^2)),
    none        = rep(1, length(d)),
    stop("Unknown kernel: ", kernel,
         '. Use "exponential", "gaussian" or "none".', call. = FALSE)
  )
}
