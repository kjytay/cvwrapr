# create fake data
set.seed(1)
nobs <- 50; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
y <- rowSums(x[, 1:2]) + rnorm(nobs)
biny <- ifelse(y > 0, 1, 0)
factorbiny <- factor(biny, labels = c("a", "b"))
poiy <- exp(y)
foldid <- sample(rep(seq(5), length = nobs))
weights <- rep(1:2, length.out = nobs)

test_that("glmnet binomial-deviance", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-deviance (factor", {
  target_fit <- cv.glmnet(x, factorbiny, family = "binomial", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, factorbiny, family = "binomial",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-mse", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          type.measure = "mse",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "mse",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-mae", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          type.measure = "mae",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "mae",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-class", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          type.measure = "class",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "class",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-auc", {
  target_fit <- cv.glmnet(x, biny, family = "binomial", weights = weights,
                          type.measure = "auc",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, biny, family = "binomial", type.measure = "auc",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "binomial",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet binomial-auc, too few observations", {
  set.seed(3)
  foldid2 <- sample(rep(seq(12), length = nobs))
  expect_warning(target_fit <- cv.glmnet(
    x, biny, family = "binomial", weights = weights,
    type.measure = "auc", foldid = foldid2, keep = TRUE))
  expect_warning(cv_fit <- kfoldcv(
    x, biny, family = "binomial", type.measure = "auc",
    train_fun = glmnet, predict_fun = predict,
    train_params = list(family = "binomial", weights = weights),
    predict_params = list(type = "response"),
    train_row_params = c("weights"), foldid = foldid2, keep = TRUE))

  compare_glmnet_fits(target_fit, cv_fit, family = "binomial")
})

test_that("glmnet poisson-deviance", {
  target_fit <- cv.glmnet(x, poiy, family = "poisson", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, poiy, family = "poisson",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "poisson",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "poisson")
})

test_that("glmnet poisson-mse", {
  target_fit <- cv.glmnet(x, poiy, family = "poisson", weights = weights,
                          type.measure = "mse",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, poiy, type.measure = "mse", family = "poisson",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "poisson",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "poisson")
})

test_that("glmnet poisson-mae", {
  target_fit <- cv.glmnet(x, poiy, family = "poisson", weights = weights,
                          type.measure = "mae",
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, poiy, type.measure = "mae", family = "poisson",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "poisson",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "poisson")
})
