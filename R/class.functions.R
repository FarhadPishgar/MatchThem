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
  plot(x$models[[n+1]], ...)
}

#' @export

print.mimids <- function(x, n = 1, ...) {

  #External function
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
  cat2("Printing               | dataset: #", n, "\n", sep = "")

  #Printing out
  output <- x$models[[n+1]]
  output$call <- x$call
  return(print(output, ...))
}

#' @export

summary.mimids <- function(object, n = 1, ...) {

  #External function
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
  cat2("Summarizing            | dataset: #", n, "\n", sep = "")

  #Printing out
  output <- summary(object$models[[n+1]], ...)
  output$call <- object$call
  return(output)

}

#

merge.mimids <- function(x, y, by = NULL, ...) {
  #NG: I don't think we need to export this. cbind.mimids() should be sufficient.

  #External function
  #S3 method

  #Importing functions
  #' @importFrom mice complete
  mice::complete

  #Checking inputs format
  if(!is.data.frame(y)) {stop("The input for the y must be a data frame.")}

  if (is.mimids(x)) {
    #Polishing variables
    call <- x$call
    modelslist <- x$models
    others <- x$others
    datasets <- x$object

    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- merge(data.0, y, by = by, ...)

    #Preparing the list
    datasetslist <- vector("list", datasets$m + 1)
    datasetslist[[1]] <- data.0

    #Merging
    for (i in 1:datasets$m) {
      data.i <- mice::complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, y, by = by, ...)
      datasetslist[[i+1]] <- data.i
    }

    #Prepating the output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    matched.datasets <- mice::as.mids(new.datasets)

    #Returning output
    output <- list(call = call,
                   object = matched.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
    class(output) <- "mimids"
    return(output)
  }
}

##### wimids

#' @export

print.wimids <- function(x, n = 1, ...) {

  #External function
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
  cat2("Printing               | dataset: #", n, "\n", sep = "")

  #Printing
  output <- x$models[[n+1]]
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
  output <- summary(object$models[[n+1]], ...)
  return(output)
}

#

merge.wimids <- function(x, y, by = NULL, ...) {
  #NG: I don't think we need to export this. cbind.wimids() should be sufficient.

  #External function
  #S3 method

  #Checking inputs format
  if(!is.data.frame(y)) {stop("The input for the y must be a data frame.")}

  if (is.wimids(x)) {
    #Polishing variables
    call <- x$call
    modelslist <- x$models
    others <- x$others
    datasets <- x$object

    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- merge(data.0, y, by = by, ...)

    #Preparing the list
    datasetslist <- vector("list", datasets$m + 1)
    datasetslist[[1]] <- data.0

    #Binding
    for (i in 1:datasets$m) {
      data.i <- mice::complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, y, by = by, ...)
      datasetslist[[i+1]] <- data.i
    }

    #Prepating the output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    weighted.datasets <- mice::as.mids(new.datasets)

    #Returning output
    output <- list(call = call,
                   object = weighted.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
    class(output) <- "wimids"
    return(output)
  }
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