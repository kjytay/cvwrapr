# y is passed to determine what dimensions the prediction array needs to have
# type.measure, weights and grouped arguments only used for family = "cox"
buildPredMat <- function(outlist, y, lambda, foldid, predict_fun,
                         predict_params, predict_row_params, family,
                         type.measure, weights, grouped) {
  # family = "cox" needs its own way of building up the prediction matrix
  # because of the grouped = TRUE and type.measure = "deviance" case
  if (is.character(family) && family == "cox")
    return(buildPredMat.cox(outlist, y, lambda, foldid, predict_fun,
                            predict_params, predict_row_params,
                            type.measure, weights, grouped))

  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)
  nlambda <- length(lambda)

  # determine dimensions for the prediction matrix
  nc <- NULL
  if (is.character(family) && family %in% c("multinomial", "mgaussian")) {
    nc <- dim(y)
    if (is.null(nc)) {
      y <- as.factor(y)
      ntab <- table(y)
      nc <- as.integer(length(ntab))
    }
    else nc <- nc[2]
  }
  predmat <- array(NA, c(length(foldid), nc, nlambda))

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
    if (is.character(family) && family %in% c("multinomial", "mgaussian")) {
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
  if (is.character(family) && family %in% c("multinomial", "mgaussian")) {
    cn <- dimnames(preds)[[2]]
    dimnames(predmat) <- list(rn, cn, sn)
  } else {
    dimnames(predmat) <- list(rn, sn)
  }

  if (is.character(family) && family %in% c("binomial", "multinomial"))
    attr(predmat, "classnames") <- outlist[[1]]$classnames
  if ("family" %in% class(family))
    attr(predmat, "family") <- family

  return(predmat)
}

buildPredMat.cox <- function(outlist, y, lambda, foldid, predict_fun,
                             predict_params, predict_row_params,
                             type.measure, weights, grouped) {
  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)
  nlambda <- length(lambda)

  if ((length(weights) / nfolds < 10) && !grouped) {
    warning(paste("Too few (< 10) observations per fold for cox family;",
                  "grouped = TRUE enforced.",
                  "Alternatively, use smaller value for nfolds"),
            call. = FALSE)
    grouped <- TRUE
  }

  devtrue <- type.measure == "deviance"
  cvraw <- if(devtrue) matrix(NA, nfolds, nlambda) else NULL
  predmat <- matrix(NA, length(foldid), nlambda)
  predict_params_copy <- predict_params
  for (i in seq_along(foldid_vals)) {
    out_idx <- (foldid == foldid_vals[i])
    predict_params <- predict_params_copy
    predict_params$object <- outlist[[i]]

    # make predictions for the whole dataset
    preds <- do.call(predict_fun, predict_params)

    nlami <- min(ncol(preds), nlambda)
    if (devtrue) {
      if (grouped) {
        plfull <- coxnet.deviance(pred = preds, y = y, weights = weights)
        plminusk <- coxnet.deviance(pred = preds[!out_idx, ],
                                    y = y[!out_idx, ],
                                    weights = weights[!out_idx])
        cvraw[i, seq(nlami)] <- (plfull - plminusk)[seq(nlami)]
      } else {
        plk <- coxnet.deviance(pred = preds[out_idx, ],
                               y = y[out_idx, ],
                               weights = weights[out_idx])
        cvraw[i, seq(nlami)] <- plk[seq(nlami)]
      }
    }

    # if fold lambda is longer than overall lambda, we need to cut off the extra
    # if fold lambda is shorter than overall lambda, we copy the last column
    # to the end
    predmat[out_idx, seq(nlami)] <- preds[out_idx, seq(nlami)]
    if (nlami < nlambda) {
      if (devtrue) cvraw[i,seq(from = nlami, to = nlambda)] <- cvraw[i, nlami]
      predmat[out_idx, seq(from = nlami, to = nlambda)] <- preds[out_idx, nlami]
    }
  }
  if (devtrue) attr(predmat, "cvraw") <- cvraw

  # add dimension names
  rn <- rownames(predict_params$newx)
  sn <- paste0("s", seq(0, length = nlambda))
  dimnames(predmat) <- list(rn, sn)

  return(predmat)
}
