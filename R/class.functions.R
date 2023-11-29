##### mimids

#' @exportS3Method plot mimids

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
  message("Plotting               | dataset: #", n, "\n", appendLF = FALSE)

  #Plotting
  plot(x$models[[n]], ...)
}

#' @exportS3Method print mimids

print.mimids <- function(x, n = 1, ...) {

  #External function
  #S3 method

  #Checking inputs format
  if(x$object$m < n) stop("The input for 'n' is out of bounds.")

  #Printing out
  message("Printing               | dataset: #", n, "\n", appendLF = FALSE)

  #Printing out
  output <- x$models[[n]]
  output$call <- x$call
  print(output, ...)
}

#' @exportS3Method summary mimids

summary.mimids <- function(object, n = 1, ...) {

  #External function
  #S3 method

  #Checking inputs format
  if(object$object$m < n) {stop("The input for 'n' is out of bounds.")}

  #Printing out
  output <- summary(object$models[[n]], ...)
  output$call <- object$call
  attr(output, ".imp") <- n
  class(output) <- c("summary.mimids", class(output))
  output
}

#' @exportS3Method print summary.mimids

print.summary.mimids <- function(x, ...) {
  #Printing out
  message("Summarizing            | dataset: #", attr(x, ".imp"), "\n", appendLF = FALSE)

  NextMethod("print")
}

##### wimids

#' @exportS3Method print wimids

print.wimids <- function(x, n = 1, ...) {

  #External function
  #S3 method

  #Checking inputs format
  if(x$object$m < n) {stop("The input for 'n' is out of bounds.")}

  #Printing out
  message("Printing               | dataset: #", n, "\n", appendLF = FALSE)

  #Printing
  output <- x$models[[n]]
  print(output, ...)
}

#' @exportS3Method summary wimids

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
  if(object$object$m < n) {stop("The input for 'n' is out of bounds.")}

  #Summarizing
  output <- summary(object$models[[n]], ...)
  output$call <- object$call
  attr(output, ".imp") <- n
  class(output) <- c("summary.wimids", class(output))
  output
}

#' @exportS3Method print summary.wimids
print.summary.wimids <- print.summary.mimids

##### mimipo

#' @exportS3Method print mimipo

print.mimipo <- function(x, ...) {

  #S3 method

  #Based on: The mice:::print.mipo()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  message("Pooling estimates      | number of datasets: ", x$m, "\n", appendLF = FALSE)

  print.data.frame(x$pooled, ...)
  invisible(x)
}