buildPredMat <- function(outlist, lambda, x, foldid, predict_fun,
                         predict_params) {
  predmat <- matrix(NA, nrow(x), length(lambda))
  nfolds <- max(foldid)
  nlams <- double(nfolds)
  nlambda <- length(lambda)

  for (i in seq(nfolds)) {
    out_idx <- foldid == i
    predict_params$object <- outlist[[i]]
    predict_params$newx <- x[out_idx, , drop = FALSE]
    predict_params$s <- lambda  # this will have to change in the future
    preds <- do.call(predict_fun, predict_params)

    # if fold lambda is longer than overall lambda, we need to cut off the extra
    # if fold lambda is shorter than overall lambda, we copy the last column
    # to the end
    nlami <- min(ncol(preds),nlambda)
    predmat[out_idx, seq(nlami)] <- preds[, seq(nlami)]
    if (nlami < nlambda)
      predmat[out_idx, seq(from = nlami, to = nlambda)] <- preds[, nlami]
  }

  # add dimension names
  rn <- rownames(x)
  sn <- paste0("s", seq(0, length = nlambda))
  dimnames(predmat) <- list(rn, sn)

  return(predmat)
}
