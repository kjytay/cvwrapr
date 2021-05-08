library(glmnet)

# passes when everything matches
compare_glmnet_fits <- function(target_fit, new_fit) {
  expect_equal(target_fit$lambda, new_fit$lambda,
               label = "lambda doesn't match")
  expect_equal(target_fit$cvm, new_fit$cvm,
               label = "cvm doesn't match")
  expect_equal(target_fit$cvsd, new_fit$cvsd,
               label = "cvsd doesn't match")
  expect_equal(target_fit$cvup, new_fit$cvup,
               label = "cvup doesn't match")
  expect_equal(target_fit$cvlo, new_fit$cvlo,
               label = "cvlo doesn't match")
  expect_equal(target_fit$fit.preval, new_fit$fit.preval,
               label = "fit.preval doesn't match")
}

set.seed(1)
nobs <- 50; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
y <- rowSums(x[, 1:2]) + rnorm(nobs)
foldid <- sample(rep(seq(5), length = nobs))
weights <- rep(1:2, length.out = nobs)
offset <- rnorm(nobs)

test_that("basic glmnet call", {
  target_fit <- cv.glmnet(x, y, foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call with weights", {
  target_fit <- cv.glmnet(x, y, weights = weights, foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    train_params = list(weights = weights),
                    train_row_params = c("x", "y", "weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call with weights and offset", {
  target_fit <- cv.glmnet(x, y, weights = weights, offset = offset,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    train_params = list(weights = weights, offset = offset),
                    predict_params = list(newoffset = offset),
                    train_row_params = c("x", "y", "weights", "offset"),
                    predict_row_params = c("newx", "newoffset"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})

test_that("basic glmnet call with weights, mae", {
  target_fit <- cv.glmnet(x, y, type.measure = "mae", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, y, train_fun = glmnet, predict_fun = predict,
                    type.measure = "mae",
                    train_params = list(weights = weights),
                    train_row_params = c("x", "y", "weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit)
})
