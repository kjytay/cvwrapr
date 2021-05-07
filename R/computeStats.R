computeStats <- function(cvstuff, foldid, lambda) {
  # compute the statistics for each fold
  nfolds <- max(foldid)
  nlams <- dim(cvstuff$cvraw)[2]
  cvstuff <- cvcompute(cvstuff, foldid, nlams)

  cvm <- with(cvstuff, apply(cvraw, 2, weighted.mean, w = weights, na.rm = TRUE))
  cvsd <- with(cvstuff, sqrt(apply(scale(cvraw, cvm, FALSE)^2, 2, weighted.mean,
                                w = weights, na.rm = TRUE)/(N - 1)))
  nas <- is.na(cvsd)
  if (any(nas)) {
    lambda <- lambda[!nas]
    cvm <- cvm[!nas]
    cvsd <- cvsd[!nas]
  }
  return(list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm + cvsd,
              cvlo = cvm - cvsd))
}

cvcompute <- function (cvstuff, foldid, nlams) {
  weights <- cvstuff$weights
  mat <- cvstuff$cvraw
  wisum <- tapply(weights, foldid, sum)
  nfolds <- max(foldid)
  outmat <- matrix(NA, nfolds, ncol(mat))
  good <- matrix(0, nfolds, ncol(mat))
  mat[is.infinite(mat)] <- NA
  for (i in seq(nfolds)) {
    mati <- mat[foldid == i, , drop = FALSE]
    wi <- weights[foldid == i]
    outmat[i, ] <- apply(mati, 2, weighted.mean, w = wi, na.rm = TRUE)
    good[i, seq(nlams)] <- 1
  }
  N <- apply(good, 2, sum)
  return(list(cvraw = outmat, weights = wisum, N = N,
              type.measure = cvstuff$type.measure))
}
