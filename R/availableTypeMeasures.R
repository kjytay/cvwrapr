#' Display the names of the measures used in CV for different families
#'
#' Produces a list of names of measures that can be used in CV for different
#' families. Note, however, that the package does not check if the measure
#' the user specifies is appropriate for the family.
#'
#' @param family If a family is supplied, a list of the names of
#' measures available for that family are produced. Default is "all", in which
#' case the names of measures for all families are produced.
#'
#' @export
availableTypeMeasures <- function(family = c("all", "gaussian", "binomial",
                                             "poisson", "multinomial", "cox",
                                             "mgaussian", "GLM")) {
  family <- match.arg(family)
  type.measures <- c("mse", "deviance", "class", "auc", "mae",
                    "C")
  choices <- list(gaussian = c("mse", "mae"),
                  binomial = c("binomial-deviance", "binomial-class", "auc",
                               "mse", "mae"),
                  poisson = c("poisson-deviance", "mse", "mae"),
                  cox = c("cox-deviance", "C"),
                  multinomial = c("multinomial-deviance", "multinomial-class",
                                  "mse", "mae"),
                  mgaussian = c("mse", "mae"),
                  GLM = c("glm-deviance", "mse", "mae"))
  if (family == "all") {
    return(choices)
  } else {
    return(choices[[family]])
  }
}
