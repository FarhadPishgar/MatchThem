#' @title Matches Multiply Imputed Datasets
#'
#' @rdname matchthem
#'
#' @aliases matchthem
#'
#' @param formula This argument takes the usual syntax of R formula, \code{z ~ x1 + x2}, where \code{z} is a binary exposure indicator and \code{x1} and \code{x2} are the potential confounders. Both the exposure indicator and the potential confounders must be contained in the imputed datasets, which is specified as \code{datasets} (see below). All of the usual R syntax for formula works. For example, \code{x1:x2} represents the first order interaction term between \code{x1} and \code{x2} and \code{I(x1^2)} represents the square term of \code{x1}. See \code{help(formula)} for details.
#' @param datasets This argument specifies the datasets containing the exposure indicator and the potential confounders called in the \code{formula}. This argument must be an object of the \code{mids} or \code{amelia} class, which is typically produced by a previous call to \code{mice()} or \code{mice.mids()} functions from the \pkg{mice} package or to \code{amelia()} function from the \pkg{Amelia} package (the \pkg{Amelia} package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, currently, the \pkg{MatchThem} package only supports the former datasets).
#' @param approach This argument specifies a matching approach. Currently, \code{"within"} (calculating distance measures within each imputed dataset and matching observations based on them) and \code{"across"} (calculating distance measures within each imputed dataset, averaging distance measure for each observation across imputed datasets, and matching based on the averaged measures) approaches are available. The default is \code{"within"} which has been shown to produce unbiased results.
#' @param method This argument specifies a matching method. Currently, \code{"nearest"} (nearest neighbor matching), \code{"exact"} (exact matching), \code{"full"} (full matching), \code{"genetic"} (genetic matching), \code{"subclass"} (subclassication), \code{"cem"} (coarsened exact matching), and \code{"optimal"} (optimal matching) methods are available (only the \code{"nearest"}, \code{"full"}, \code{"subclass"}, and \code{"optimal"} matching methods are compatible with the \code{"across"} approach). The default is \code{"nearest"}. Note that within each of these matching methods, \pkg{MatchThem} offers a variety of options.
#' @param distance This argument specifies the method that should be used to estimate the distance measure (the \code{"mahalanobis"} method for distance measure is not compatible with the \code{"across"} approach). The default is logistic regression, \code{"logit"}. A variety of other methods are available (please see the \pkg{MatchIt} package reference manual <https://cran.r-project.org/package=MatchIt> for more details).
#' @param distance.options This optional argument specifies the arguments that are passed to the model for estimating the distance measure. The input to this argument should be a list.
#' @param discard This argument specifies whether to discard observations that fall outside some measure of support of the distance score before matching and not allow them to be used at all in the matching procedure. Note that discarding observations may change the quantity of interest being estimated. The current options are \code{"none"} (discarding no observations before matching), \code{"both"} (discarding all observations, both the control and treated observations, that are outside the support of the distance measure), \code{"control"} (discarding only control observations outside the support of the distance measure of the treated observations), and \code{"treat"} (discarding only treated observations outside the support of the distance measure of the control observations). The default is \code{"none"}.
#' @param reestimate This argument specifies whether the model for estimating the distance measure should be reestimated after observations are discarded. The input must be a logical value. The default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the matching method (please see the \pkg{MatchIt} package reference manual <https://cran.r-project.org/package=MatchIt> for more details).
#'
#' @description \code{matchthem()} function enables parametric models for causal inference to work better by selecting matched subsets of the control and treated subgroups of imputed datasets of a \code{mids} or \code{amelia} class object.
#'
#' @details The matching is done using the \code{matchthem(z ~ x1, ...)} command, where \code{z} is the exposure indicator and \code{x1} represents the potential confounder to be used in the matching model. There are a number of matching options. The default syntax is \code{matchthem(formula, datasets, approach = "within", method = "nearest", distance = "logit", ...)}. Summaries of the results can be seen graphically using \code{plot()} or numerically using \code{summary()} functions. The \code{print()} function also prints out the output.
#'
#' @return This function returns an object of the \code{mimids} (matched multiply imputed datasets) class, that includes matched subsets of the imputed datasets primarily passed to the function by the \code{datasets} argument.
#'
#' @seealso \code{\link[=mimids]{mimids}}
#' @seealso \code{\link[=with]{with}}
#' @seealso \code{\link[=pool]{pool}}
#'
#' @author Farhad Pishgar and Noah Greifer
#'
#' @references Daniel Ho, Kosuke Imai, Gary King, and Elizabeth Stuart (2007). Matching as Nonparametric Preprocessing for Reducing Model Dependence in Parametric Causal Inference. \emph{Political Analysis}, 15(3): 199-236. \url{http://gking.harvard.edu/files/abs/matchp-abs.shtml}
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#' @references Gary King, James Honaker, Anne Joseph, and Kenneth Scheve (2001). Analyzing Incomplete Political Science Data: An Alternative Algorithm for Multiple Imputation. \emph{American Political Science Review}, 95: 49–69. \url{http://j.mp/2oOrtGs}
#'
#' @export
#'
#' @examples \donttest{#Loading libraries
#' library(mice)
#' library(MatchThem)
#'
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice(osteoarthritis, m = 5, maxit = 10,
#'                          method = c("", "", "mean", "polyreg",
#'                                     "logreg", "logreg", "logreg"))
#'
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                               approach = 'within', method = 'nearest')}

matchthem <- function (formula, datasets,
                       approach = "within",
                       method = "nearest", distance = "logit", distance.options = list(),
                       discard = "none", reestimate = FALSE, ...) {

  #External function

  #Importing functions
  #' @importFrom MatchIt matchit
  #' @importFrom mice complete
  #' @importFrom stats as.formula
  MatchIt::matchit
  mice::complete
  stats::as.formula
  #' @export

  #Polishing variables
  formula <- stats::as.formula(formula)
  called <- match.call()
  originals <- datasets
  classed <- class(originals)
  if(approach == "pool-then-match") {approach <- "across"}
  if(approach == "match-then-pool") {approach <- "within"}

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(class(datasets) != "mids" && class(datasets) != "amelia") {stop("The input for the datasets must be an object of the 'mids' or 'amelia' class.")}
  if(!is.null(datasets$data$distance)) {stop("The input for the datasets shouldn't have a variable named 'distance'.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(!is.null(datasets$data$subclass)) {stop("The input for the datasets shouldn't have a variable named 'subclass'.")}
  if(!is.null(datasets$data$discarded)) {stop("The input for the datasets shouldn't have a variable named 'discarded'.")}
  if(!is.null(datasets$data$estimated.distance) && approach == "across") {stop("The input for the datasets shouldn't have a variable named 'estimated.distance', when the 'across' matching approch is selected.")}
  if(!(approach %in% c("within","across"))) {stop("The input for the matching approach must be either 'within' or 'across'.")}
  if(approach == "across" && (!(method %in% c("nearest", "full", "subclass", "optimal")))) {stop("The input for the matching method must be 'nearest', 'full', 'subclass', or 'optimal', when the 'across' matching approch is selected.")}
  if(approach == "across" && distance == "mahalanobis" ) {stop("The input for the distance shouldn't be 'mahalanobis', when the 'across' matching approch is selected.")}
  if(!(method %in% c("nearest", "exact", "full", "genetic", "subclass", "cem", "optimal"))) {stop("The input for the matching method must be either 'nearest', 'exact', 'full', 'genetic', 'subclass', 'cem', or 'optimal'.")}

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

      #Building the model
      dataset <- mice::complete(datasets, i)
      dataset$.id <- 1:nrow(datasets$data)

      #Printing out
      if (!(method %in% c("genetic", "cem"))){
        if (i == 1) cat("Matching Observations  | dataset: #", i, sep = "")
        if (i != 1) cat(" #", i, sep = "")
      } else {
        if (i == 1) cat("Matching Observations  | dataset: #", i, "\n", sep = "")
        if (i != 1) cat("\n", "Matching Observations  | dataset: #", i, "\n", sep = "")
      }

      #Building the model
      model <- MatchIt::matchit(formula, dataset,
                                method = method, distance = distance,
                                distance.options = distance.options, discard = discard,
                                reestimate = reestimate, ...)

      #Matched dataset
      dataset$weights <- model$weights
      dataset$distance <- model$distance
      dataset$discarded <- model$discarded
      dataset$subclass <- model$subclass
      dataset$.imp <- i

      #Updating the lists
      datasetslist[[i+1]] <- dataset
      modelslist[[i+1]] <- model
    }

    #The raw data
    dataset0 <- datasets$data
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$weights <- NA_real_
    if (!is.null(datasetslist[[2]]$distance)) {dataset0$distance <- NA_real_}
    if (!is.null(datasetslist[[2]]$discarded)) {dataset0$discarded <- NA_real_}
    if (!is.null(datasetslist[[2]]$subclass)) {dataset0$subclass <- NA_real_}
    dataset0$.imp <- 0

    #Updating the lists
    datasetslist[[1]] <- dataset0

    #Binding the datasets
    matched.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    matched.datasets <- as2.mids(matched.datasets)

    #Others
    others <- list(source = originals, class = classed)

    #Returning output
    output <- list(call = called,
                   object = matched.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
    class(output) <- c("mimids", "list")
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

      #Building the model
      dataset <- mice::complete(datasets, i)
      dataset$.id <- 1:nrow(datasets$data)

      #Printing out
      if (i == 1) cat("Estimating distances   | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Building the model
      model <- MatchIt::matchit(formula, dataset,
                                method = "nearest", distance = distance,
                                distance.options = distance.options,
                                discard = "none",
                                reestimate = FALSE, ...)

      #Distance
      distancelist[[i]] <- model$distance
    }

    #Updating the distance
    d <- rowMeans(as.matrix(do.call("cbind", distancelist)))

    #Matching each dataset
    for (i in 1:datasets$m) {
      dataset <- mice::complete(datasets, i)
      dataset$.id <- 1:nrow(datasets$data)
      dataset$estimated.distance <- d

      #Printing out
      if (i == 1) cat("\n", "Matching Observations  | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Building the model
      model <- MatchIt::matchit(formula, dataset,
                                method = method, distance = dataset$estimated.distance,
                                distance.options = distance.options, discard = discard,
                                reestimate = reestimate, ...)

      #Matched dataset
      dataset$weights <- model$weights
      dataset$distance <- model$distance
      dataset$discarded <- model$discarded
      dataset$subclass <- model$subclass
      dataset$estimated.distance <- NULL
      dataset$.imp <- i

      #Updating the lists
      datasetslist[[i+1]] <- dataset
      modelslist[[i+1]] <- model
    }

    #The raw data
    dataset0 <- datasets$data
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$weights <- NA_real_
    dataset0$distance <- NA_real_
    if (!is.null(datasetslist[[2]]$discarded)) {dataset0$discarded <- NA_real_}
    if (!is.null(datasetslist[[2]]$subclass)) {dataset0$subclass <- NA_real_}
    dataset0$.imp <- 0

    #Updating the lists
    datasetslist[[1]] <- dataset0

    #Binding the datasets
    matched.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    matched.datasets <- as2.mids(matched.datasets)

    #Others
    others <- list(source = originals, class = classed)

    #Returning output
    output <- list(call = called,
                   object = matched.datasets,
                   models = modelslist,
                   datasets = datasetslist,
                   others = others)
    class(output) <- c("mimids", "list")
    cat("\n")
    return(output)
  }
}
