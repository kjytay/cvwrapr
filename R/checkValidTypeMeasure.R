#' Check if loss function is valid for a given family
#'
#' Also throws error if family is invalid
checkValidTypeMeasure <- function(type.measure, family) {
  if ("family" %in% class(family)) family <- "GLM"

  if (!(family %in% c("gaussian", "binomial", "poisson", "multinomial",
                      "cox", "mgaussian", "GLM")))
    stop("Invalid family argument")

  if (!(type.measure %in% availableTypeMeasures(family)))
    stop(paste("Invalid type.measure for this family;",
               "see availableTypeMeasures() for possibilities"))
}
