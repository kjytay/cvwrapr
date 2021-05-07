# only works for gaussian right now
# in the future needs to work with other families
computeRawError <- function(predmat, y, type.measure, weights, foldid) {
  N <- length(y) - apply(is.na(predmat), 2, sum)
  cvraw <- switch(type.measure,
                  mse = (y - predmat)^2,
                  deviance = (y - predmat)^2,
                  mae = abs(y - predmat))
  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure))
}
