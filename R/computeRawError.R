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
#' @param family Model family; used to determine the correct loss function.
#' @param weights Observation weights.
#' @param foldid Vector of values identifying which fold each observation is
#' in. Not in use at the moment.
#' @param grouped Experimental argument; see `kfoldcv()` documentation for
#' details.
#'
#' @return A list with the following elements:
#' \item{cvraw}{An nobs by nlambda matrix of raw error values.}
#' \item{weights}{Observation weights.}
#' \item{N}{A vector of length nlambda representing the number of non-NA
#' predictions associated with each lambda value.}
#' \item{type.measure}{Loss function used for CV.}
computeRawError <- function(predmat, y, type.measure, family, weights, foldid,
                            grouped) {
  if (!(type.measure %in% availableTypeMeasures(family)))
    stop(paste("Invalid type.measure for this family;",
               "see availableTypeMeasures() for possibilities"))

  return(do.call(paste0("computeRawError.", family),
                 list(predmat = predmat, y = y, type.measure = type.measure,
                      weights = weights, foldid = foldid, grouped = grouped)))
}

computeRawError.gaussian <- function(predmat, y, type.measure, family,
                                     weights, foldid, grouped) {
  N <- length(y) - apply(is.na(predmat), 2, sum)

  if (type.measure %in% c("deviance", "mse")) {
    cvraw <- (y - predmat)^2
  } else if (type.measure == "mae") {
    cvraw <- abs(y - predmat)
  } else {
    stop("invalid type.measure for gaussian family")
  }
  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}

computeRawError.poisson <- function(predmat, y, type.measure, family,
                                     weights, foldid, grouped) {
  N <- length(y) - apply(is.na(predmat), 2, sum)

  if (type.measure == "mse") {
    cvraw <- (y - predmat)^2
  } else if (type.measure == "mae") {
    cvraw <- abs(y - predmat)
  } else if (type.measure == "deviance") {
    deveta <- y * log(predmat) - predmat
    devy <- y * log(y) - y
    cvraw <- 2 * (devy - deveta)
  } else {
    stop("invalid type.measure for poisson family")
  }
  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}
