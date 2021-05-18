#' Compute CV statistics from a prediction matrix
#'
#' Compute CV statistics from a matrix of predictions.
#'
#' Note that for the setting where `family = "cox"` and
#' `type.measure = "deviance"` and `grouped = TRUE`, `predmat` needs to have
#' a `cvraw` attribute as computed by `buildPredMat()`. This is because the
#' usual matrix of pre-validated fits does not contain all the information
#' needed to compute the model deviance for this setting.
#'
#' @param predmat Array of predictions. If `y` is univariate, this has
#' dimensions `c(nobs, nlambda)`. If `y` is multivariate with `nc`
#' levels/columns (e.g. for `family = "multionmial"` or
#' `family = "mgaussian"`), this has dimensions `c(nobs, nc, nlambda)`.
#' Note that these should be on the same scale as `y` (unlike in the
#' glmnet package where it is the linear predictor).
#' @param y Response variable.
#' @param lambda Lambda values associated with the errors in `predmat`.
#' @param foldid Vector of values identifying which fold each observation is
#' in.
#' @param type.measure Loss function to use for cross-validation. See
#' `availableTypeMeasures()` for possible values for `type.measure`. Note that
#' the package does not check if the user-specified measure is appropriate
#' for the family.
#' @param family Model family; used to determine the correct loss function.
#' @param weights Observation weights.
#' @param grouped Experimental argument; see `kfoldcv()` documentation for
#' details.
#'
#' @export
computeError <- function(predmat, y, lambda, foldid, type.measure, family,
                         weights = rep(1, dim(predmat)[1]),
                         grouped = TRUE) {
  ### parameter checks
  checkValidTypeMeasure(type.measure, family)

  if (length(lambda) != dim(predmat)[length(dim(predmat))])
    stop("lambda should be a vector of length `dim(predmat)[length(dim(predmat))]`")

  ### end parameter checks

  nfolds <- length(sort(unique(foldid)))

  # Note: computeRawError can change type.measure and grouped
  cvstuff <- computeRawError(predmat, y, type.measure, family, weights, foldid,
                             grouped)

  grouped <- cvstuff$grouped
  if ((dim(predmat)[1] / nfolds < 3) && grouped) {
    warning(paste("Option grouped = FALSE enforced in computeError,",
                  "since < 3 observations per fold"),
            call. = FALSE)
    grouped <- FALSE
  }

  out <- computeStats(cvstuff, foldid, lambda, grouped)
  out$type.measure <- cvstuff$type.measure

  return(out)
}
