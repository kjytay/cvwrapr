buildPredMat <- function(outlist, lambda, foldid, predict_fun,
                         predict_params, predict_row_params) {
  predmat <- matrix(NA, length(foldid), length(lambda))
  nfolds <- max(foldid)
  nlams <- double(nfolds)
  nlambda <- length(lambda)

  predict_params_copy <- predict_params
  for (i in seq(nfolds)) {
    out_idx <- foldid == i
    predict_params$object <- outlist[[i]]
    predict_params$s <- lambda  # this will have to change in the future

    # update the training parameters before fitting
    for (param in predict_row_params) {
      if (is.matrix(predict_params_copy[[param]])) {
        predict_params[[param]] <- predict_params_copy[[param]][out_idx, , drop = FALSE]
      } else {
        predict_params[[param]] <- predict_params_copy[[param]][out_idx]
      }
    }

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
  rn <- rownames(predict_params$newx)
  sn <- paste0("s", seq(0, length = nlambda))
  dimnames(predmat) <- list(rn, sn)

  return(predmat)
}
