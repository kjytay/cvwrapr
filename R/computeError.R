#' Compute CV statistics from a prediction matrix
#'
#' Compute CV statistics from a matrix of predictions.
#'
#' @param predmat nobs by nlambda matrix of predictions. Note that these
#' should be on the same scale as `y` (unlike in the glmnet package where it
#' is the linear predictor).
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
#'
#' @export
computeError <- function(predmat, y, lambda, foldid, type.measure, family,
                         weights = rep(1, nrow(predmat)),
                         grouped = TRUE) {
  ### parameter checks
  if (length(lambda) != ncol(predmat))
    stop("lambda should be a vector of length `ncol(predmat)`")

  ### end parameter checks

  nfolds <- length(sort(unique(foldid)))

  # Note: computeRawError can change type.measure and grouped
  cvstuff <- computeRawError(predmat, y, type.measure, family, weights, foldid,
                             grouped)

  grouped <- cvstuff$grouped
  if ((nrow(predmat) / nfolds < 3) && grouped) {
    warning(paste("Option grouped = FALSE enforced in cv.glmnet,",
                  "since < 3 observations per fold"),
            call. = FALSE)
    grouped <- FALSE
  }

  out <- computeStats(cvstuff, foldid, lambda, grouped)
  out$type.measure <- cvstuff$type.measure

  return(out)
}
