##### mids

#' @export

merge.mids <- function(x, y, by = NULL, ...) {

  #External function
  #S3 method

  #Importing functions
  #' @importFrom mice is.mids
  mice::is.mids
  #' @export

  #Checking inputs format
  if(!is.data.frame(y)) {stop("The input for the y must be a data frame.")}

  if (mice::is.mids(x)) {
    #Polishing variables
    data.0 <- x$data
    data.0$.id <- 1:nrow(x$data)
    data.0$.imp <- 0
    data.0 <- merge(data.0, y, by = by, ...)

    #Preparing the list
    datasetslist <- vector("list", x$m + 1)
    datasetslist[[1]] <- data.0

    #Merging
    for (i in 1:x$m) {
      data.i <- complete(x, i)
      data.i$.id <- 1:nrow(x$data)
      data.i$.imp <- i
      data.i <- merge(data.i, y, by = by, ...)
      datasetslist[[i+1]] <- data.i
    }

    #Returning output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    new.datasets <- as2.mids(new.datasets)
    return(new.datasets)
  }
}

#

cbind.mids <- function(datasets, data, ...) {

  #Internal function
  #S3 method

  #Importing functions
  #' @importFrom mice is.mids
  mice::is.mids

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(is.null(data)) {stop("The input for the data must be specified.")}
  if(!(mice::is.mids(datasets))) {stop("The input for the datasets must be an object of the 'mids' class.")}
  if(!is.data.frame(data)) {stop("The input for the data must be a dataframe.")}

  if (mice::is.mids(datasets)) {
    #Polishing variables
    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- cbind(data.0, data, ...)

    #Preparing the list
    datasetslist <- vector("list", datasets$m + 1)
    datasetslist[[1]] <- data.0

    #Binding
    for (i in 1:datasets$m) {
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- cbind(data.i, data, ...)
      datasetslist[[i+1]] <- data.i
    }

    #Returning output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    new.datasets <- as2.mids(new.datasets)
    return(new.datasets)
  }
}

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
  cat("Plotting               | dataset: #", n,  "\n", sep = "")

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
  cat("Printing               | dataset: #", n,  "\n", sep = "")

  #Printing out
  output <- x$models[[n+1]]
  output$call <- x$others$call.
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
  cat("Summarizing            | dataset: #", n,  "\n", sep = "")

  #Printing out
  output <- summary(object$models[[n+1]], ...)
  output$call <- object$others$call.
  return(output)

}

#' @export

merge.mimids <- function(x, y, by = NULL, ...) {

  #External function
  #S3 method

  #Checking inputs format
  if(!is.data.frame(y)) {stop("The input for the y must be a data frame.")}

  if (is.mimids(x)) {
    #Polishing variables
    modelslist <- x$models
    others <- x$others
    originals <- x$original.datasets
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
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, y, by = by, ...)
      datasetslist[[i+1]] <- data.i
    }

    #Prepating the output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    matched.datasets <- as2.mids(new.datasets)

    #Returning output
    output <- list(object = matched.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.datasets = originals)
    class(output) <- c("mimids", "list")
    return(output)
  }
}

#

cbind.mimids <- function(datasets, data, ...) {

  #Internal function
  #S3 method

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(is.null(data)) {stop("The input for the data must be specified.")}
  if(!(is.mimids(datasets))) {stop("The input for the datasets must be an object of the 'mimids' class.")}
  if(!is.data.frame(data)) {stop("The input for the data must be a dataframe.")}

  if (is.mimids(datasets)) {
    #Polishing variables
    modelslist <- datasets$models
    others <- datasets$others
    originals <- datasets$original.datasets
    datasets <- datasets$object

    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- cbind(data.0, data, ...)

    #Preparing the list
    datasetslist <- vector("list", datasets$m + 1)
    datasetslist[[1]] <- data.0

    #Binding
    for (i in 1:datasets$m) {
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- cbind(data.i, data, ...)
      datasetslist[[i+1]] <- data.i
    }

    #Prepating the output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    matched.datasets <- as2.mids(new.datasets)

    #Returning output
    output <- list(object = matched.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.datasets = originals)
    class(output) <- c("mimids", "list")
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
  cat("Printing               | dataset: #", n,  "\n", sep = "")

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
  cat("Summarizing            | dataset: #", n,  "\n", sep = "")

  #Summarizing
  output <- summary(object$models[[n+1]], ...)
  return(output)
}

#' @export

merge.wimids <- function(x, y, by = NULL, ...) {

  #External function
  #S3 method

  #Checking inputs format
  if(!is.data.frame(y)) {stop("The input for the y must be a data frame.")}

  if (is.wimids(x)) {
    #Polishing variables
    modelslist <- x$models
    others <- x$others
    originals <- x$original.datasets
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
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, y, by = by, ...)
      datasetslist[[i+1]] <- data.i
    }

    #Prepating the output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    weighted.datasets <- as2.mids(new.datasets)

    #Returning output
    output <- list(object = weighted.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.datasets = originals)
    class(output) <- c("wimids", "list")
    return(output)
  }
}

#

cbind.wimids <- function(datasets, data, ...) {

  #Internal function
  #S3 method

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(is.null(data)) {stop("The input for the data must be specified.")}
  if(!(is.wimids(datasets))) {stop("The input for the datasets must be an object of the 'wimids' class.")}
  if(!is.data.frame(data)) {stop("The input for the data must be a dataframe.")}

  if (is.wimids(datasets)) {
    #Polishing variables
    modelslist <- datasets$models
    others <- datasets$others
    originals <- datasets$original.datasets
    datasets <- datasets$object

    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- cbind(data.0, data, ...)

    #Preparing the list
    datasetslist <- vector("list", datasets$m + 1)
    datasetslist[[1]] <- data.0

    #Binding
    for (i in 1:datasets$m) {
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- cbind(data.i, data, ...)
      datasetslist[[i+1]] <- data.i
    }

    #Prepating the output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    weighted.datasets <- as2.mids(new.datasets)

    #Returning output
    output <- list(object = weighted.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.datasets = originals)
    class(output) <- c("wimids", "list")
    return(output)
  }
}

##### mira

df.residual.mira <- function(object, ...) {

  #Internal function
  #S3 method

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

