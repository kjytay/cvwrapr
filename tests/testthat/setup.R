library(glmnet)

# passes when everything matches
compare_glmnet_fits <- function(target_fit, new_fit, family = "gaussian") {
  if (family == "poisson") {
    new_fit$fit.preval <- log(new_fit$fit.preval)
  }

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
