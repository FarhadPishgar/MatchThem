##### mimids

#' @export

plot.mimids <- function(x, n = 1, ...){

  #External function
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
  cat2("Plotting               | dataset: #", n, "\n", sep = "")

  #Plotting
  plot(x$models[[n]], ...)
}

#' @export

print.mimids <- function(x, n = 1, ...) {

  #External function
  #S3 method

  #Checking inputs format
  if(x$object$m < n) {stop("The input for the n is out of bounds.")}

  #Printing out
  cat2("Printing               | dataset: #", n, "\n", sep = "")

  #Printing out
  output <- x$models[[n]]
  output$call <- x$call
  return(print(output, ...))
}

#' @export

summary.mimids <- function(object, n = 1, ...) {

  #External function
  #S3 method

  #Checking inputs format
  if(object$object$m < n) {stop("The input for the n is out of bounds.")}

  #Printing out
  cat2("Summarizing            | dataset: #", n, "\n", sep = "")

  #Printing out
  output <- summary(object$models[[n]], ...)
  output$call <- object$call
  return(output)

}

##### wimids

#' @export

print.wimids <- function(x, n = 1, ...) {

  #External function
  #S3 method

  #Checking inputs format
  if(x$object$m < n) {stop("The input for the n is out of bounds.")}

  #Printing out
  cat2("Printing               | dataset: #", n, "\n", sep = "")

  #Printing
  output <- x$models[[n]]
  return(print(output, ...))
}

#' @export

summary.wimids <- function(object, n = 1, ...) {

  #External function
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
  cat2("Summarizing            | dataset: #", n, "\n", sep = "")

  #Summarizing
  output <- summary(object$models[[n]], ...)
  return(output)
}

##### mimipo

#' @export

print.mimipo <- function(x, ...) {

  #S3 method

  #Based on: The mice:::print.mipo()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  cat2("Pooling estimates      | number of datasets: ", x$m, "\n", sep = "")

  print.data.frame(x$pooled, ...)
  invisible(x)
}