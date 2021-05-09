#' K-fold cross-validation wrapper
#'
#' Does k-fold cross-validation for a given model training function and
#' prediction function. The hyperparameter to be cross-validated is assumed
#' to be `lambda`. The training and prediction functions are assumed to be
#' able to fit/predict for multiple `lambda` values at the same time.
#'
#' The model training function is assumed to take in the data matrix as `x`,
#' the response as `y`, and the hyperparameter to be cross-validated as
#' `lambda`. It is assumed that in its returned output, the hyperparameter
#' values actually used are stored as `lambda`. The prediction function
#' is assumed to take in the new data matrix as `newx`.
#'
#' @param x Input matrix of dimension `nobs` by `nvars`; each row is an
#' observation vector.
#' @param y Response variable. Either a vector or a matrix, depending on the
#' type of model.
#' @param train_fun The model training function. This needs to take in an
#' input matrix as `x` and a response variable as `y`.
#' @param predict_fun The prediction function. This needs to take in the
#' output of `train_fun` as `object` and new input matrix as `newx`.
#' @param type.measure Loss function to use for cross-validation. NOT FULLY
#' IMPLEMENTED YET: only works for "mse", "mae", "deviance" for gaussian family
#' right now.
#' @param lambda Option user-supplied sequence representing the values of the
#' hyperparameter to be cross-validated.
#' @param lambda_predict_name The name of the function argument to specify
#' the hyperparameter values we want predictions at for `predict_fun`.
#' Default is "s".
#' @param train_params Any parameters that should be passed to
#' `train_fun` to fit the model (other than `x` and `y`). Default is the
#' empty list.
#' @param predict_params Any other parameters that should be passed tp
#' `predict_fun` to get predictions (other than `object` and `newx`). Default
#' is the empty list. (RIGHT NOW, s IS ALSO INCLUDED BY DEFAULT, BUT
#' I HAVE TO THINK THIS THROUGH.)
#' @param train_row_params A vector which is a subset of `names(train_params)`,
#' indicating which parameters have to be subsetted in the CV loop (other
#' than `x` and `y`. Default is `c()`.
#' Other parameters which should probably be included here
#' are "weights" (for observation weights) and "offset".
#' @param predict_row_params A vector which is a subset of
#' `names(predict_params)`, indicating which parameters have to be subsetted
#' in the CV loop (other than `newx`). Default is `c("newx")`.
#' Other parameters which should probably be included here
#' are "newoffset".
#' @param nfolds Number of folds (default is 10). Smallest allowable value
#' is 3.
#' @param foldid An optional vector of values between `1` and `nfolds`
#' (inclusive) identifying which fold each observation is in. If supplied,
#' `nfolds` can be missing.
#' @param parallel NOT IMPLEMENTED YET
#' @param keep If `keep = TRUE`, a prevalidated array is returned containing
#' fitted values for each observation and each value of lambda. This means
#' these fits are computed with this observation and the rest of its fold
#' omitted. The `foldid` vector is also returned. Default is `keep = FALSE`.
#'
#' @export
kfoldcv <- function(x,
                    y,
                    train_fun,
                    predict_fun,
                    type.measure = c("mse", "deviance", "mae"),
                    lambda = NULL,
                    lambda_predict_name = "s",
                    train_params = list(),
                    predict_params = list(),
                    train_row_params = c(),
                    predict_row_params = c(),
                    nfolds = 10,
                    foldid = NULL,
                    parallel = FALSE,  # not in use yet
                    keep = FALSE) {
  # arguments x, y, newx and lambda have a special status at the moment
  # we may want to remove this special status in the future
  train_params$x <- x
  train_params$y <- y
  train_params$lambda <- lambda
  predict_params$newx <- x
  train_row_params <- c("x", "y", train_row_params)
  predict_row_params <- c("newx", predict_row_params)
  N <- nrow(x)

  type.measure <- match.arg(type.measure)

  ### parameter checking section
  if (!is.null(lambda) && length(lambda) < 2)
    stop("Need more than one value of lambda for kfoldcv")

  # train_row_params should be a subset of train_params' names
  if (length(setdiff(train_row_params, names(train_params))) > 0)
    stop("train_row_params should be a subset of names(train_params)")

  # predict_row_params should be a subset of predict_params' names
  if (length(setdiff(predict_row_params, names(predict_params))) > 0)
    stop("predict_row_params should be a subset of names(predict_params)")

  ### end parameter checking section

  # get observation weights
  if ("weights" %in% names(train_params)) {
    weights <- train_params$weights
    if (!("weights" %in% train_row_params))
      warning(paste0("observation weights provided in train_params, so",
                     "'weights' should probably be in train_row_params"))
  } else {
    weights <- rep(1, N)
  }

  # set nfolds and foldid
  if (is.null(foldid)) {
    foldid <- sample(rep(seq(nfolds), length = N))
  } else {
    # if foldid is not 1 to "nfolds", we make it so
    foldid_vals <- sort(unique(foldid))
    nfolds <- length(foldid_vals)
    if (!identical(foldid_vals, 1:nfolds)) {
      foldid <- match(foldid, foldid_vals)
    }
  }
  if (nfolds < 3)
    stop("nfolds must be >= 3; nfolds = 10 recommended")

  # overall fit for the whole dataset
  train_params_copy <- train_params
  train_obj <- do.call(train_fun, train_params)

  # fit for each of the folds
  outlist <- as.list(seq(nfolds))  # to store the fits
  for (i in seq(nfolds)) {
    out_idx <- foldid == i

    # update the training parameters before fitting
    for (param in train_row_params) {
      if (is.matrix(train_params_copy[[param]])) {
        train_params[[param]] <- train_params_copy[[param]][!out_idx, , drop = FALSE]
      } else {
        train_params[[param]] <- train_params_copy[[param]][!out_idx]
      }
    }
    outlist[[i]] <- do.call(train_fun, train_params)
  }

  # build prediction matrix
  lambda <- train_obj$lambda
  predict_params[[lambda_predict_name]] <- lambda
  predmat <- buildPredMat(outlist, lambda, foldid, predict_fun,
                          predict_params, predict_row_params)

  # compute error metric
  cvstuff <- computeRawError(predmat, y, type.measure, weights, foldid)
  out <- computeStats(cvstuff, foldid, lambda)

  if (keep)
    out <- c(out, list(fit.preval = predmat, foldid = foldid))
  return(out)
}
