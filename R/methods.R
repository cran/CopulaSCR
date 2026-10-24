#' Standard Methods for 'mscr' Objects
#'
#' The following S3 methods are available for fitted objects returned by
#' \code{mscr()}:
#' \itemize{
#'   \item \code{print()} displays the fitted association analysis.
#'   \item \code{summary()} provides a formatted summary of the fitted model.
#'   \item \code{plot()} displays the marginal survival curves, data structure,
#'     or reconstructed association structure.
#'   \item \code{predict()} computes dynamic survival and restricted mean
#'     survival-time predictions.
#'   \item \code{logLik()} extracts the pseudo-log-likelihood used for model
#'     comparison.
#' }
#'
#'
#' @seealso \code{\link{mscr}}, \code{\link{plot.mscr}},
#'   \code{\link{predict.mscr}}, \code{\link{print.mscr}},
#'   \code{\link{summary.mscr}}, \code{\link{logLik.mscr}}
#'
#' @name mscr-methods
NULL


#' Standard Methods for 'scrsurv' Objects
#'
#' The following S3 methods are available for fitted objects returned by
#' \code{scrsurv()}:
#' \itemize{
#'   \item \code{print()} displays the fitted marginal survival analysis.
#'   \item \code{summary()} provides a formatted summary of the marginal
#'     survival fit.
#'   \item \code{plot()} displays the estimated marginal survival curves.
#'   \item \code{predict()} computes joint, conditional, or related survival
#'     predictions.
#' }
#'
#'
#' @seealso \code{\link{scrsurv}}, \code{\link{plot.scrsurv}},
#'   \code{\link{predict.scrsurv}}, \code{\link{print.scrsurv}},
#'   \code{\link{summary.scrsurv}}
#'
#' @name scrsurv-methods
NULL


#' Standard Methods for 'scrassonp' Objects
#'
#' Standard methods for extracting estimated copula parameters, their
#' uncertainty, the number of observations, and the model frame from a fitted
#' \code{"scrassonp"} object.
#' The following S3 methods are available:
#' \itemize{
#'   \item \code{plot()} displays the observed data or fitted association
#'     structure.
#'   \item \code{print()} displays the fitted association analysis.
#'   \item \code{summary()} provides a summary of the fitted association
#'     analysis.
#'   \item \code{coef()} returns the estimated copula parameter or parameters.
#'   \item \code{vcov()} returns a diagonal covariance matrix based on the
#'     reported standard errors.
#'   \item \code{confint()} returns confidence intervals.
#'   \item \code{nobs()} returns the number of observations used in the fit.
#'   \item \code{model.frame()} returns the model data retained in the fit.
#' }
#'
#'
#' @return The returned value depends on the selected S3 method; see the
#'   individual method documentation for details.
#'
#' @seealso \code{\link{scrassonp}}, \code{\link{plot.scrassonp}},
#'   \code{\link{print.scrassonp}}, \code{\link{summary.scrassonp}},
#'   \code{\link{association_estimates}}
#'
#' @name scrassonp-methods
NULL

