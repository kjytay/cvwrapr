#' Compute the nobs by nlambda matrix of errors
#'
#' Computes the nobs by nlambda matrix of errors corresponding to the error
#' measure provided. Only works for "gaussian" and "poisson" families right
#' now.
#'
#' @param predmat nobs by nlambda matrix of predictions. Note that these
#' should be on the same scale as `y` (unlike in the glmnet package where it
#' is the linear predictor).
#' @param y Response variable.
#' @param type.measure Loss function to use for cross-validation. See
#' `availableTypeMeasures()` for possible values for `type.measure`. Note that
#' the package does not check if the user-specified measure is appropriate
#' for the family.
#' @param weights Observation weights.
#' @param foldid Vector of values identifying which fold each observation is
#' in. Not in use at the moment.
#'
#' @return A list with the following elements:
#' \item{cvraw}{An nobs by nlambda matrix of raw error values.}
#' \item{weights}{Observation weights.}
#' \item{N}{A vector of length nlambda representing the number of non-NA
#' predictions associated with each lambda value.}
#' \item{type.measure}{Loss function used for CV.}
computeRawError <- function(predmat, y, type.measure, weights, foldid) {
  all_measures <- sort(unique(unlist(availableTypeMeasures())))
  if (!(type.measure %in% all_measures))
    stop("Invalid type.measure; see availableTypeMeasures() for possibilities")

  N <- length(y) - apply(is.na(predmat), 2, sum)

  if (type.measure == "mse") {
    cvraw <- (y - predmat)^2
  } else if (type.measure == "mae") {
    cvraw <- abs(y - predmat)
  } else if (type.measure == "poisson-deviance") {
    deveta <- y * log(predmat) - predmat
    devy <- y * log(y) - y
    cvraw <- 2 * (devy - deveta)
  }

  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure))
}
