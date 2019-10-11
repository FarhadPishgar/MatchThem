df.residual.mira <- function(object, ...) {

  #Internal function

  #Based on: The mice:::df.residual()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  #Importing functions
  #' @importFrom stats df.residual
  stats::df.residual

  fit <- object$analyses[[1]]
  return(df.residual(fit))
}
