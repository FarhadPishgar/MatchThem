#' @export

plot.mimids <- function(x, n = 1, type = "QQ", discrete.cutoff = 5,
                        numdraws = 5000, interactive = TRUE, which.xs = NULL, ...){

  #S3 method

  #Importing functions
  #' @importFrom graphics plot
  graphics::plot
  #' @export

  #Based on: The MatchIt:::plot.matchit()
  #URL: <https://cran.r-project.org/package=MatchIt>
  #URL: <https://github.com/kosukeimai/MatchIt>
  #URL: <https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf>
  #URL: <https://imai.fas.harvard.edu/research/files/matchit.pdf>
  #Authors: Daniel Ho et al.
  #Changes: Few

  #Checking inputs format
  if(x$object$m < n) {stop("The input for the n is out of bounds.")}

  #Printing out
  cat("Plotting               | dataset: #", n,  "\n", sep = "")

  #Plotting
  graphics::plot(x$models[[n+1]], discrete.cutoff = discrete.cutoff, type = type,
                           numdraws = numdraws, interactive = interactive, which.xs = which.xs, ...)
}

#' @export

print.mimids <- function(x, n = 1, digits = getOption("digits"), ...) {

  #S3 method

  #Based on: The MatchIt:::print.matchit()
  #URL: <https://cran.r-project.org/package=MatchIt>
  #URL: <https://github.com/kosukeimai/MatchIt>
  #URL: <https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf>
  #URL: <https://imai.fas.harvard.edu/research/files/matchit.pdf>
  #Authors: Daniel Ho et al.
  #Changes: Some

  #Checking inputs format
  if(x$object$m < n) {stop("The input for the n is out of bounds.")}

  #Printing out
  cat("Printing               | dataset: #", n,  "\n", sep = "")

  #Printing out
  output <- x$models[[n+1]]
  output$call <- x$others$call.
  return(output)
}

#' @export

summary.mimids <- function(object, n = 1, interactions = FALSE, addlvariables = NULL,
                           standardize = FALSE, covariates = FALSE, ...) {

  #S3 method

  #Based on: The MatchIt:::summary.matchit()
  #URL: <https://cran.r-project.org/package=MatchIt>
  #URL: <https://github.com/kosukeimai/MatchIt>
  #URL: <https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf>
  #URL: <https://imai.fas.harvard.edu/research/files/matchit.pdf>
  #Authors: Daniel Ho et al.
  #Changes: Some

  #Checking inputs format
  if(object$object$m < n) {stop("The input for the n is out of bounds.")}

  #Printing out
  cat("Summarizing            | dataset: #", n,  "\n", sep = "")

  #Printing out
  output <- summary(object$models[[n+1]])
  output$call <- object$others$call.
  return(output)

}

#' @export

print.wimids <- function(x, n = 1, digits = getOption("digits"), ...) {

  #S3 method

  #Based on: The WeightIt:::print.weighit()
  #URL: <https://cran.r-project.org/package=WeightIt>
  #URL: <https://github.com/ngreifer/WeightIt>
  #URL: <https://cran.r-project.org/web/packages/WeightIt/vignettes/WeightIt_A0_basic_use.html>
  #Author: Noah Greifer
  #Changes: NA

  #Checking inputs format
  if(x$object$m < n) {stop("The input for the n is out of bounds.")}

  #Printing out
  cat("Printing               | dataset: #", n,  "\n", sep = "")

  #Printing
  output <- x$models[[n+1]]
  return(output)
}

#' @export

summary.wimids <- function(object, n = 1, interactions = FALSE, addlvariables = NULL,
                           standardize = FALSE, ...) {

  #S3 method

  #Based on: The WeightIt:::summary.weighit()
  #URL: <https://cran.r-project.org/package=WeightIt>
  #URL: <https://github.com/ngreifer/WeightIt>
  #URL: <https://cran.r-project.org/web/packages/WeightIt/vignettes/WeightIt_A0_basic_use.html>
  #Author: Noah Greifer
  #Changes: NA

  #Checking inputs format
  if(object$object$m < n) {stop("The input for the n is out of bounds.")}

  #Printing out
  cat("Summarizing            | dataset: #", n,  "\n", sep = "")

  #Summarizing
  output <- summary(object$models[[n+1]])
  return(output)
}
