# only works for gaussian and poisson right now
# in the future needs to work with other families
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
