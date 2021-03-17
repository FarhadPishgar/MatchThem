get.2dfcom <- function(object, dfcom = NULL) {
  # Residual degrees of freedom of model fitted on hypothetically complete data
  # Unlike mice, using minimum across imputations to be conservative

  #Internal function
  #S3 method

  #Based on: mice:::get.dfcom()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Several

  #Importing functions
  #' @importFrom mice getfit
  #' @importFrom rlang is_bare_numeric
  #' @importFrom stats residuals coef

  if (rlang::is_bare_numeric(dfcom, 1) && is.finite(dfcom)) {
    return(max(dfcom, 1L))
  }
  else dfcom <- NULL

  if (!inherits(object, "mimira")) stop("The input for the object must be an object of the 'mimira' class.")

  glanced <- try(summary(mice::getfit(object), type = "glance"), silent = TRUE)

  if (!inherits(glanced, "try-error")) {

    # try to extract from df.residual
    if ("df.residual" %in% names(glanced)) {
      dfcom <- min(glanced$df.residual)
    }
    else {

      # try n - p (or nevent - p for Cox model)
      model <- mice::getfit(object, 1L)
      if (inherits(model, "coxph") && "nevent" %in% names(glanced)) {
        dfcom <- min(glanced$nevent - length(coef(model)))
      }
      else {
        if (!"nobs" %in% names(glanced)) {
          glanced$nobs <- min(lengths(lapply(object$analyses, stats::residuals)), na.rm = TRUE)
        }
        dfcom <- min(glanced$nobs - length(coef(model)))
      }
    }
  }

  #Not found
  #Warning("Infinite sample size assumed.")
  if (is.null(dfcom) || !is.finite(dfcom)) dfcom <- 999999

  dfcom
}