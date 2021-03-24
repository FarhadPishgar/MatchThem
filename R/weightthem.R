#' @title Weights Multiply Imputed Datasets
#'
#' @rdname weightthem
#'
#' @aliases weightthem
#'
#' @param formula A \code{formula} of the form \code{z ~ x1 + x2}, where \code{z} is the exposure and \code{x1} and \code{x2} are the covariates to be balanced, which is passed directly to code{\link[WeightIt:weightit]{WeightIt::weightit()}} to specify the propensity score model or treatment and covariates to be used to estimate the weights. See \code{\link[WeightIt:weightit]{weightit()}} for details.
#' @param datasets The datasets containing the exposure and covariates mentioned in the \code{formula}. This argument must be an object of the \code{mids} or \code{amelia} class, which is typically produced by a previous call to \code{mice()} from the \pkg{mice} package or to \code{amelia()} from the \pkg{Amelia} package (the \pkg{Amelia} package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, currently, the \pkg{MatchThem} package only supports the former datasets).
#' @param approach The approach used to combine information across imputed datasets. Currently, \code{"within"} (estimating weights within each imputed dataset) and \code{"across"} (estimating propensity scores within each dataset, averaging them across datasets, and computing a single set of weights to be applied to all datasets) approaches are available. The default is \code{"within"}, which has been shown to have superior performance in most cases.
#' @param method The method used to estimate weights. See \code{\link[WeightIt:weightit]{weightit()}} for allowable options. Only methods that produce a propensity score (\code{"ps"}, \code{"gbm"}, \code{"cbps"}, \code{"super"}, and \code{"bart"}) are compatible with the \code{"across"} approach). The default is \code{"ps"} propensity score weighting using logistic regression propensity scores.
#' @param ... Additional arguments to be passed to \code{weightit()}. see \code{\link[WeightIt:weightit]{weightit()}} for more details.
#'
#' @description \code{weightthem()} performs weighting in the supplied imputed datasets, given as \code{mids} or \code{amelia} objects, by running \code{\link[WeightIt:weightit]{WeightIt::weightit()}} on each of the imputed datasets with the supplied arguments.
#'
#' @details If an \code{amelia} object is supplied to \code{datasets}, it will first be transformed into a \code{mids} object for further use. \code{weightthem()} works by calling \code{\link[mice:complete]{mice::complete()}} on the \code{mids} object to extract a complete dataset, and then calls \code{\link[WeightIt:weightit]{WeightIt::weightit()}} on each one, storing the output of each \code{weightit()} call and the \code{mids} in the output. All arguments supplied to \code{weightthem()} except \code{datasets} and \code{approach} are passed directly to \code{weightit()}. With the across method, the estimated propensity scores are averaged across imputations and re-supplied to another set of calls to \code{weightit()}.
#'
#' @return An object of the \code{\link{wimids}} (weighted multiply imputed datasets) class, which includes the supplied \code{mids} object (or an \code{amelia} object transformed into a \code{mids} object if supplied) and the output of the calls to \code{weightit()} on each imputed dataset.
#'
#' @seealso \code{\link[=wimids]{wimids}}
#' @seealso \code{\link[=with]{with}}
#' @seealso \code{\link[=pool]{pool}}
#' @seealso \code{\link[=matchthem]{matchthem}}
#' @seealso \code{\link[WeightIt:weightit]{WeightIt::weightit}}
#'
#' @author Farhad Pishgar and Noah Greifer
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'
#' @export
#'
#' @examples \donttest{#1
#'
#' #Loading libraries
#' library(MatchThem)
#'
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice::mice(osteoarthritis, m = 5)
#'
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                                 imputed.datasets,
#'                                 approach = 'within',
#'                                 method = 'ps',
#'                                 estimand = "ATT")
#'
#' #2
#'
#' #Loading libraries
#' library(Amelia)
#' library(MatchThem)
#'
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- amelia(osteoarthritis, m = 5, noms = c("SEX", "RAC", "SMK", "OSP", "KOA"))
#'
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                                 imputed.datasets,
#'                                 approach = 'within',
#'                                 method = 'ps',
#'                                 estimand = "ATT")}

weightthem <- function (formula, datasets,
                        approach = "within",
                        method = "ps", ...) {

  #External function

  #Importing functions
  #' @importFrom WeightIt weightit
  #' @importFrom mice complete as.mids
  #' @importFrom stats as.formula
  WeightIt::weightit
  mice::complete
  stats::as.formula
  #' @export

  #Polishing variables
  called <- match.call()
  originals <- datasets
  classed <- class(originals)
  if (identical(approach, "pool-then-match")) {approach <- "across"}
  else if (identical(approach, "match-then-pool")) {approach <- "within"}

  #Checking inputs format
  if(missing(datasets) || length(datasets) == 0) {stop("The input for the datasets must be specified.")}
  if(!inherits(datasets, "mids")  && !inherits(datasets, "amelia")) {stop("The input for the datasets must be an object of the 'mids' or 'amelia' class.")}
  if(!is.null(datasets$data$estimated.distance) && approach == "across") {stop("The input for the datasets shouldn't have a variable named 'estimated.distance', when the 'across' weighting approch is selected.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  approach <- match.arg(approach, c("within","across"))
  if(approach == "across" && (!(method %in% c("ps", "gbm", "cbps", "super", "bart")))) {stop("The input for the weighting method must be 'ps', 'gbm', 'cbps', 'super', or 'bart' when the 'across' weighting approch is selected.")}

  #Compatibility with amelia objects
  if (inherits(datasets, "amelia")) {
    imp0 <- datasets$imputations[[1]]
    is.na(imp0) <- datasets$missMatrix
    imp0$.id <- 1:nrow(imp0)
    imp0$.imp <- 0

    implist <- vector("list", datasets$m + 1)
    implist[[1]] <- imp0

    for (i in 1:datasets$m) {
      imp <- datasets$imputations[[i]]
      imp$.id <- 1:nrow(imp0)
      imp$.imp <- i
      implist[[i+1]] <- imp
    }

    imp.datasets <- do.call(base::rbind, as.list(noquote(implist)))
    datasets <- mice::as.mids(imp.datasets)
    originals <- datasets
  }

  #Within
  if (approach == "within") {

    #Defining the lists
    modelslist <- vector("list", datasets$m)

    #Longing the datasets
    for (i in 1:datasets$m) {

      #Printing out
      if (i == 1) message(paste0("Estimating weights     | dataset: #", i), appendLF = FALSE)
      else        message(paste0(" #", i), appendLF = FALSE)

      #Building the model
      dataset <- mice::complete(datasets, i)
      model <- WeightIt::weightit(formula, dataset,
                                  method = method, ...)

      #Updating the lists
      modelslist[[i]] <- model
    }
  }

  #Across
  if (approach == "across") {

    #Defining the lists
    modelslist <- vector("list", datasets$m)
    distancelist <- vector("list", datasets$m)

    #Calculating the averaged distances
    for (i in 1:datasets$m) {

      #Printing out
      if (i == 1) message(paste0("Estimating distances   | dataset: #", i), appendLF = FALSE)
      else        message(paste0(" #", i), appendLF = FALSE)

      #Building the model
      dataset <- mice::complete(datasets, i)
      modelslist[[i]] <- model <- WeightIt::weightit(formula, dataset,
                                                     method = method, ...)

      #Measures
      distancelist[[i]] <- model$ps
    }

    #Updating the weights
    d <- rowMeans(as.matrix(do.call(base::cbind, distancelist)))

    #Adding averaged weights to datasets
    for (i in 1:(datasets$m)) {
      dataset <- mice::complete(datasets, i)

      #Printing out
      if (i == 1) message(paste0("\n", "Estimating weights     | dataset: #", i), appendLF = FALSE)
      else        message(paste0(" #", i), appendLF = FALSE)

      #Building the model
      model <- WeightIt::weightit(formula, dataset,
                                  method = "ps",
                                  ps = d, ...)

      #Updating the list
      modelslist[[i]][c("weights", "ps")] <- model[c("weights", "ps")]
    }
  }

  #Returning output
  output <- list(call = called,
                 object = datasets,
                 models = modelslist,
                 approach = approach)
  class(output) <- "wimids"
  message("\n", appendLF = FALSE)
  return(output)
}