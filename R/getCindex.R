#' Compute C index for a Cox model
#'
#' Computes Harrel's C (concordance) index for predictions, taking
#' censoring into account.
#'
#' @param pred A vector of predictions.
#' @param y Survival response variable, must be a \code{Surv} or
#' \code{stratifySurv} object.
#' @param weights Observation weights (default is all equal to 1).
#'
#' @importFrom survival concordance Surv
#' @export
getCindex <- function(pred, y, weights = rep(1,nrow(y))) {
    if (!is.Surv(y)) y <- Surv(y[, "time"], y[, "status"])
    f <- -pred
    if (missing(weights))
        return(concordance(y ~ f)$concordance)
    else
        return(concordance(y ~ f, weights = weights)$concordance)
}
