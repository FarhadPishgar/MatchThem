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
  cat("Plotting               | dataset: #", n, "\n", sep = "")

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
  cat("Printing               | dataset: #", n, "\n", sep = "")

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
  cat("Summarizing            | dataset: #", n, "\n", sep = "")

  #Printing out
  output <- summary(object$models[[n+1]], ...)
  output$call <- object$call
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
    output <- list(call = call,
                   object = matched.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
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
    call <- datasets$call
    modelslist <- datasets$models
    others <- datasets$others
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
    output <- list(call = call,
                   object = matched.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
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
  cat("Printing               | dataset: #", n, "\n", sep = "")

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
  cat("Summarizing            | dataset: #", n, "\n", sep = "")

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
    output <- list(call = call,
                   object = weighted.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
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
    call <- datasets$call
    modelslist <- datasets$models
    others <- datasets$others
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
    output <- list(call = call,
                   object = weighted.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
    class(output) <- c("wimids", "list")
    return(output)
  }
}

##### mimira

#' @export

print.mimira <- function(x, ...) {

  #S3 method

  #Based on: The mice:::print.mira()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  if (class(x)[[1]] == "mimira") {
    print.listof(x, ...)
  } else {
    print(x, ...)
  }
  invisible()
}

#' @export

summary.mimira <- function(object, type = c("tidy", "glance", "summary"), ...) {

  #S3 method

  #Based on: The mice:::summary.mira()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  #Importing functions
  #' @importFrom mice getfit
  #' @importFrom broom tidy
  #' @importFrom broom glance
  mice::getfit
  broom::tidy
  broom::glance

  type <- match.arg(type)
  fitlist <- mice::getfit(object)

  #The unusual way
  tidy <- NA
  glance <- NA

  if (type == "tidy") {
    v <- lapply(fitlist, tidy, effects = "fixed", ...) %>% dplyr::bind_rows()
  }
  if (type == "glance") {
    v <- lapply(fitlist, glance, ...) %>% dplyr::bind_rows()
  }
  if (type == "summary") {
    v <- lapply(fitlist, summary, ...)
  }

  return(v)
}

#

df.residual.mimira <- function(object, ...) {

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

  cat("Pooling estimates      | number of datasets: ", x$m, "\n", sep = "")

  print.data.frame(x$pooled, ...)
  invisible(x)
}

#' @export

print.mimipo.summary <- function(x, ...) {

  #S3 method

  #Based on: The mice:::print.mipo.summary()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  print.data.frame(x, ...)
  invisible(x)
}

#' @export

summary.mimipo <- function(object, type = c("tests", "all"), conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...) {

  #S3 method

  #Based on: The mice:::summary.mipo()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  #Importing functions
  #' @importFrom stats pt
  stats::pt

  type <- match.arg(type)
  m <- object$m
  x <- object$pooled
  std.error <- sqrt(x$t)
  statistic <- x$estimate / std.error
  p.value <- 2 * (1 - pt(abs(statistic), pmax(x$df, 0.001)))

  z <- data.frame(x,
                  std.error = std.error,
                  statistic = statistic,
                  p.value = p.value)
  z <- process2.mimipo(z, object,
                       conf.int = conf.int,
                       conf.level = conf.level,
                       exponentiate = exponentiate)

  if (type == "tests") {
    out <- c("riv", "lambda", "fmi", "ubar", "b", "t", "dfcom")
    keep <- base::setdiff(names(z), out)
    z <- z[, keep]
  }

  class(z) <- c("mimipo.summary", "data.frame")
  return(z)
}

#

vcov.mimipo <- function(object, ...) {

  #S3 method

  #Based on: The mice:::vcov.mipo()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  so <- diag(object$t)
  dimnames(so) <- list(object$term, object$term)

  return(so)
}

#

confint.mimipo <- function(object, parm, level = 0.95, ...) {

  #S3 method

  #Based on: The mice:::confint.mipo()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: NA

  #Importing functions
  #' @importFrom mice getqbar
  #' @importFrom stats qt
  mice::getqbar
  stats::qt

  class(object) <- c("mipo", "data.frame")
  pooled <- object$pooled
  cf <- mice::getqbar(object)
  df <- pooled$df
  se <- sqrt(pooled$t)
  pnames <- names(df) <- names(se) <- names(cf) <- row.names(pooled)

  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }

  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- stats::qt(a, df)
  pct <- format2.perc(a, 3)
  ci <- array(NA, dim = c(length(parm), 2L),
              dimnames = list(parm, pct))
  ci[, 1] <- cf[parm] + qt(a[1], df[parm]) * se[parm]
  ci[, 2] <- cf[parm] + qt(a[2], df[parm]) * se[parm]

  return(ci)
}
