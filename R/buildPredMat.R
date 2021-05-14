# y is passed to determine what dimensions the prediction array needs to have
buildPredMat <- function(outlist, y, lambda, foldid, predict_fun,
                         predict_params, predict_row_params, family) {
  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)
  nlams <- double(nfolds)
  nlambda <- length(lambda)

  # determine dimensions for the prediction matrix
  nc <- NULL
  if (family %in% c("multinomial", "mgaussian")) {
    nc <- dim(y)
    if (is.null(nc)) {
      y <- as.factor(y)
      ntab <- table(y)
      nc <- as.integer(length(ntab))
    }
    else nc <- nc[2]
  }
  predmat <- array(NA, c(length(foldid), nc, length(lambda)))

  predict_params_copy <- predict_params
  for (i in seq_along(foldid_vals)) {
    out_idx <- (foldid == foldid_vals[i])
    predict_params$object <- outlist[[i]]

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
    if (family %in% c("multinomial", "mgaussian")) {
      nlami <- min(dim(preds)[3], nlambda)
      predmat[out_idx, , seq(nlami)] <- preds[, , seq(nlami)]
      if (nlami < nlambda)
        predmat[out_idx, , seq(from = nlami, to = nlambda)] <- preds[, , nlami]
    } else {
      nlami <- min(ncol(preds), nlambda)
      predmat[out_idx, seq(nlami)] <- preds[, seq(nlami)]
      if (nlami < nlambda)
        predmat[out_idx, seq(from = nlami, to = nlambda)] <- preds[, nlami]
    }

  }

  # add dimension names
  rn <- rownames(predict_params$newx)
  sn <- paste0("s", seq(0, length = nlambda))
  if (family %in% c("multinomial", "mgaussian")) {
    cn <- dimnames(preds)[[2]]
    dimnames(predmat) <- list(rn, cn, sn)
  } else {
    dimnames(predmat) <- list(rn, sn)
  }

  if (family %in% c("binomial", "multinomial"))
    attr(predmat, "classnames") <- outlist[[1]]$classnames

  return(predmat)
}
