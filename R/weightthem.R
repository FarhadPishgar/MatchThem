#' @title Weights Multiply Imputed Datasets
#'
#' @rdname weightthem
#'
#' @aliases weightthem
#'
#' @param formula This argument takes the usual syntax of R formula, \code{z ~ x1 + x2}, where \code{z} is a binary exposure indicator and \code{x1} and \code{x2} are the potential confounders. Both the exposure indicator and the potential confounders must be contained in the imputed datasets, which is specified as \code{datasets} (see below). All of the usual R syntax for formula works. For example, \code{x1:x2} represents the first order interaction term between \code{x1} and \code{x2} and \code{I(x1^2)} represents the square term of \code{x1}. See \code{help(formula)} for details.
#' @param datasets This argument specifies the datasets containing the exposure indicator and the potential confounders called in the \code{formula}. This argument must be an object of the \code{mids} or \code{amelia} class, which is typically produced by a previous call to \code{mice()} or \code{mice.mids()} functions from the \pkg{mice} package or to \code{amelia()} function from the \pkg{Amelia} package (the \pkg{Amelia} package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, currently, the \pkg{MatchThem} package only supports the former datasets).
#' @param approach This argument specifies a matching approach. Currently, \code{"within"} (calculating distance measures within each imputed dataset and weighting observations based on them ) and \code{"across"} (calculating distance measures within each imputed dataset, averaging distance measure for each observation across imputed datasets, and weighting based on the averaged measures) approaches are available. The default is \code{"within"} which has been shown to produce unbiased results.
#' @param method This argument specifies the method that should be used to estimate weights. Currently, \code{"ps"} (propensity score weighting using generalized linear models), \code{"gbm"} (propensity score weighting using generalized boosted modeling), \code{"cbps"} (covariate balancing propensity score weighting), \code{"npcbps"} (non-parametric covariate balancing propensity score weighting), \code{"ebal"} (entropy balancing), \code{"ebcw"} (empirical balancing calibration weighting), \code{"optweight"} (optimization-based weighting), \code{"super"} (propensity score weighting using SuperLearner), and \code{"user-defined"} (weighting using a user-defined weighting function) are available (only the \code{"ps"}, \code{"gbm"}, \code{"cbps"}, and \code{"super"} weighting methods are compatible with the \code{"across"} approach). The default is \code{"ps"}. Note that within each of these weighting methods, \pkg{MatchThem} offers a variety of options.
#' @param estimand This argument specifies the desired estimand. For binary and multinomial treatments, can be \code{"ATE"}, \code{"ATT"}, \code{"ATC"}, and, for some weighting methods, \code{"ATO"} or \code{"ATM"}. The default is \code{"ATE"}. Please see the \pkg{WeightIt} package reference manual <https://cran.r-project.org/package=WeightIt> for more details.
#' @param ... Additional arguments to be passed to the weighting method (please see the \pkg{WeightIt} package reference manual <https://cran.r-project.org/package=WeightIt> for more details).
#'
#' @description \code{weightthem()} function enables parametric models for causal inference to work better by estimating weights of the control and treated units in each imputed dataset of a \code{mids} or \code{amelia} class object.
#'
#' @details The weighting is done using the \code{weightthem(z ~ x1, ...)} command, where \code{z} is the exposure indicator and \code{x1} represents the potential confunders to be used in the weighting model. The default syntax is \code{weightthem(formula, datasets, approach = "within", method = "ps", estimand = "ATE", ...)}. Summaries of the results can be seen numerically using \code{summary()} function. The \code{print()} function also prints out the output.
#'
#' @return This function returns an object of the \code{wimids} (weighted multiply imputed datasets) class, that includes weights of observations of the imputed datasets (listed as the \code{weights} variables in each) primarily passed to the function by the \code{datasets} argument.
#'
#' @seealso \code{\link[=wimids]{wimids}}
#' @seealso \code{\link[=with]{with}}
#' @seealso \code{\link[=pool]{pool}}
#'
#' @author Farhad Pishgar and Noah Greifer
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'
#' @export
#'
#' @examples \donttest{#Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice(osteoarthritis, m = 5, maxit = 10,
#'                          method = c("", "", "mean", "polyreg",
#'                                     "logreg", "logreg", "logreg"))
#'
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                                 approach = 'within', method = 'ps', estimand = "ATT")}

weightthem <- function (formula, datasets,
                        approach = "within",
                        method = "ps", estimand = "ATE", ...) {

  #External function

  #Importing functions
  #' @importFrom WeightIt weightit
  #' @importFrom stats as.formula
  WeightIt::weightit
  stats::as.formula
  #' @export

  #Polishing variables
  formula <- stats::as.formula(formula)
  called <- match.call()
  originals <- datasets
  classed <- class(originals)
  if(approach == "pool-then-weight") {approach <- "across"}
  if(approach == "weight-then-pool") {approach <- "within"}

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(class(datasets) != "mids" && class(datasets) != "amelia") {stop("The input for the datasets must be an object of the 'mids' or 'amelia' class.")}
  if(!is.null(datasets$data$estimated.distance) && approach == "across") {stop("The input for the datasets shouldn't have a variable named 'estimated.distance', when the 'across' weighting approch is selected..")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(!(approach %in% c("within","across"))) {stop("The input for the weighting approach must be either 'within' or 'across'.")}
  if(approach == "across" && (!(method %in% c("ps", "gbm", "cbps", "super")))) {stop("The input for the weighting method must be 'ps', 'gbm', 'cbps', or 'super', when the 'across' weighting approch is selected.")}
  if(!(method %in% c("ps", "gbm", "cbps", "npcbps", "ebal", "ebcw", "optweight", "super", "user-defined"))) {stop("The input for the weighting method must be 'ps', 'gbm', 'cbps', 'npcbps', 'ebal', 'ebcw', 'optweight', 'super', or 'user-defined'.")}
  if(!(estimand %in% c("ATE", "ATT", "ATC", "ATM", "ATO"))) {stop("The input for the estimand must be 'ATE', 'ATT', 'ATC', 'ATM', or 'ATO'.")}

  #Compatibility with amelia objects
  if (class(datasets) == "amelia") {
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

    imp.datasets <- do.call("rbind", as.list(noquote(implist)))
    datasets <- as2.mids(imp.datasets)
    originals <- datasets
  }

  #Within
  if (approach == "within") {

    #Defining the lists
    datasetslist <- vector("list", datasets$m + 1)
    modelslist <- vector("list", datasets$m + 1)

    #Longing the datasets
    for (i in 1:datasets$m) {

      #Printing out
      if (i == 1) cat("Estimating weights     | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Building the model
      dataset <- complete(datasets, i)
      model <- WeightIt::weightit(formula, dataset,
                                  method = method, estimand = estimand, ...)

      #Dataset
      dataset$weights <- model$weights
      dataset$.id <- 1:nrow(datasets$data)
      dataset$.imp <- i

      #Updating the lists
      datasetslist[[i+1]] <- dataset
      modelslist[[i+1]] <- model
    }

    #The raw data
    dataset0 <- datasets$data
    dataset0$weights <- NA_real_
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$.imp <- 0

    #Updating the lists
    datasetslist[[1]] <- dataset0

    #Binding the datasets
    weighted.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    weighted.datasets <- as2.mids(weighted.datasets)

    #Others
    others <- list(source = originals, class = classed)

    #Returning output
    output <- list(call = called,
                   object = weighted.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
    class(output) <- c("wimids", "list")
    cat("\n")
    return(output)
  }

  #Across
  if (approach == "across") {

    #Defining the lists
    datasetslist <- vector("list", datasets$m + 1)
    modelslist <- vector("list", datasets$m + 1)
    distancelist <- vector("list", datasets$m)

    #Calculating the averaged distances
    for (i in 1:datasets$m) {

      #Printing out
      if (i == 1) cat("Estimating distances   | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Building the model
      dataset <- complete(datasets, i)
      model <- WeightIt::weightit(formula, dataset,
                                  method = method, estimand = estimand, ...)

      #Measures
      distancelist[[i]] <- model$ps
    }

    #Updating the weights
    d <- rowMeans(as.matrix(do.call("cbind", distancelist)))

    #Adding averaged weights to datasets
    for (i in 1:(datasets$m)) {
      dataset <- complete(datasets, i)
      dataset$estimated.distance <- d

      #Printing out
      if (i == 1) cat("\n", "Estimating weights     | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Building the model
      model <- WeightIt::weightit(formula, dataset,
                                  method = "ps",
                                  ps = dataset$estimated.distance, estimand = estimand, ...)

      #Dataset
      dataset$weights <- model$weights
      dataset$.id <- 1:nrow(datasets$data)
      dataset$.imp <- i
      dataset$estimated.distance <- NULL

      #Updating the list
      datasetslist[[i+1]] <- dataset
      modelslist[[i+1]] <- model
    }

    #Raw data
    dataset0 <- datasets$data
    dataset0$weights <- NA_real_
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$.imp <- 0

    #Updating the list
    datasetslist[[1]] <- dataset0

    #Binding the datasets
    weighted.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    weighted.datasets <- as2.mids(weighted.datasets)

    #Others
    others <- list(source = originals, class = classed)

    #Returning output
    output <- list(call = called,
                   object = weighted.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
    class(output) <- c("wimids", "list")
    cat("\n")
    return(output)
  }
}
