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
  if(x$object$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Polishing variables
  model <- x$models[[n + 1]]

  #Printing out
  cat("Dataset: #", n,  "\n", sep = "")

  #Plotting
  graphics::plot(model, discrete.cutoff = discrete.cutoff, type = type,
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
  if(x$object$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Printing out
  cat("Dataset: #", n,  "\n", sep = "")
  if (x$others$method. == 'exact') {cat("\nExact subclasses: ", max(x$models[[n + 1]]$subclass, na.rm = TRUE), "\n", sep="")}
  cat("\nSample sizes: ", sep="\n")

  #Printing
  print(x$models[[n + 1]]$nn)
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
  if(object$object$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Printing out
  cat("Dataset: #", n,  "\n", sep = "")

  #Summarizing
  if (object$others$method. == 'exact') {
    cat("\nSample sizes:", "\n", sep = "")
    print(summary(object$models[[n + 1]], covariates = covariates)[[2]])
    cat("\n    Matched sample sizes by subclass:", "\n", sep = "")
    print(summary(object$models[[n + 1]], covariates = covariates)[[1]])

  }

  if (object$others$method. == 'nearest') {
    cat("\nSummary of balance for all data:", "\n", sep = "")
    print(summary(object$models[[n + 1]], interactions = interactions, addlvariables = addlvariables, standardize = standardize)[[3]])
    cat("\nSummary of balance for matched data:", "\n", sep = "")
    print(summary(object$models[[n + 1]], interactions = interactions, addlvariables = addlvariables, standardize = standardize)[[4]])
    cat("\nPercent balance improvement:", "\n", sep = "")
    print(summary(object$models[[n + 1]], interactions = interactions, addlvariables = addlvariables, standardize = standardize)[[5]])
    cat("\nSample sizes:", "\n", sep = "")
    print(summary(object$models[[n + 1]], interactions = interactions, addlvariables = addlvariables, standardize = standardize)[[2]])
  }
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
  if(x$object$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Printing out
  cat("Dataset: #", n,  "\n", sep = "")

  #Printing
  print(x$models[[n + 1]])
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
  if(object$object$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Printing out
  cat("Dataset: #", n,  "\n", sep = "")

  #Summarizing
  summary(object$models[[n + 1]])
}
