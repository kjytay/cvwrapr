# create fake data
set.seed(1)
nobs <- 50; nvars <- 10
x <- matrix(rnorm(nobs * nvars), nrow = nobs)
y <- rowSums(x[, 1:2]) + rnorm(nobs)
poiy <- exp(y)
foldid <- sample(rep(seq(5), length = nobs))
weights <- rep(1:2, length.out = nobs)
offset <- rnorm(nobs)

test_that("glmnet poisson-deviance", {
  target_fit <- cv.glmnet(x, poiy, family = "poisson", weights = weights,
                          foldid = foldid, keep = TRUE)
  cv_fit <- kfoldcv(x, poiy, type.measure = "deviance", family = "poisson",
                    train_fun = glmnet, predict_fun = predict,
                    train_params = list(family = "poisson",
                                        weights = weights),
                    predict_params = list(type = "response"),
                    train_row_params = c("weights"),
                    foldid = foldid, keep = TRUE)

  compare_glmnet_fits(target_fit, cv_fit, family = "poisson")
})
