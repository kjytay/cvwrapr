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
#' in.
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
  if (family %in% c("cox", "multinomial", "mgaussian", "GLM"))
    stop(paste("Not implemented yet for family =", family))

  if (!(type.measure %in% availableTypeMeasures(family)))
    stop(paste("Invalid type.measure for this family;",
               "see availableTypeMeasures() for possibilities"))

  return(do.call(paste0("computeRawError.", family),
                 list(predmat = predmat, y = y, type.measure = type.measure,
                      weights = weights, foldid = foldid, grouped = grouped)))
}

computeRawError.binomial <- function(predmat, y, type.measure,
                                     weights, foldid, grouped) {
  nc <- dim(y)
  if (is.null(nc)) {
    y <- as.factor(y)
    ntab <- table(y)
    nc <- as.integer(length(ntab))
    y <- diag(nc)[as.numeric(y), , drop=FALSE]
  }
  N <- nrow(y)

  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)

  if ((N / nfolds < 10) && type.measure == "auc") {
    warning(paste("Too few (< 10) observations per fold for type.measure='auc'",
                  "in cv.lognet; changed to type.measure='deviance'.",
                  "Alternatively, use smaller value for nfolds"),
            call. = FALSE)
    type.measure <- "deviance"
  }

  nlambda <- ncol(predmat)
  if (type.measure == "auc") {
    cvraw <- matrix(NA, nfolds, nlambda)
    good <- matrix(0, nfolds, nlambda)
    for (i in seq_along(foldid_vals)) {
      good[i, seq(nlambda)] <- 1
      which <- (foldid == foldid_vals[i])
      for (j in seq(nlambda)) {
        cvraw[i, j] <- getAUC(y[which, ], predmat[which, j], weights[which])
      }
    }
    N <- apply(good, 2, sum)
    weights <- tapply(weights, foldid, sum)
    grouped <- FALSE
  }
  else {
    ywt <- apply(y, 1, sum)
    y <- y / ywt
    weights <- weights * ywt
    N <- nrow(y) - apply(is.na(predmat), 2, sum)
    prob_min <- 1e-05
    prob_max <- 1 - prob_min
    cvraw <- switch(type.measure,
                    mse = (y[, 1] - (1 - predmat))^2 + (y[, 2] - predmat)^2,
                    mae = abs(y[, 1] - (1 - predmat)) + abs(y[, 2] - predmat),
                    deviance = {
                      predmat <- pmin(pmax(predmat, prob_min), prob_max)
                      lp <- y[, 1] * log(1 - predmat) + y[, 2] * log(predmat)
                      ly <- log(y)
                      ly[y == 0] <- 0
                      ly <- drop((y * ly) %*% c(1, 1))
                      2 * (ly - lp)
                    },
                    class = y[, 1] * (predmat > 0.5) + y[, 2] * (predmat <= 0.5)
    )
  }

  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}

computeRawError.gaussian <- function(predmat, y, type.measure,
                                     weights, foldid, grouped) {
  N <- length(y) - apply(is.na(predmat), 2, sum)

  cvraw <- switch(type.measure,
                  mse = (y - predmat)^2,
                  deviance = (y - predmat)^2,
                  mae = abs(y - predmat)
  )

  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}

computeRawError.poisson <- function(predmat, y, type.measure,
                                     weights, foldid, grouped) {
  N <- length(y) - apply(is.na(predmat), 2, sum)

  cvraw <- switch(type.measure,
                  mse = (y - predmat)^2,
                  mae = abs(y - predmat),
                  deviance = {
                    deveta <- y * log(predmat) - predmat
                    devy <- y * log(y) - y
                    2 * (devy - deveta)
                  }
  )

  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}
