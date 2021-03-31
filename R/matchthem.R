#' @title Matches Multiply Imputed Datasets
#'
#' @rdname matchthem
#'
#' @aliases matchthem
#'
#' @param formula A \code{formula} of the form \code{z ~ x1 + x2}, where \code{z} is the exposure and \code{x1} and \code{x2} are the covariates to be balanced, which is passed directly to code{\link[MatchIt:matchit]{MatchIt::matchit()}} to specify the propensity score model or treatment and covariates to be used in matching. See \code{\link[MatchIt:matchit]{matchit()}} for details.
#' @param datasets This argument specifies the datasets containing the exposure indicator and the potential confounders called in the \code{formula}. This argument must be an object of the \code{mids} or \code{amelia} class, which is typically produced by a previous call to \code{mice()} from the \pkg{mice} package or to \code{amelia()} from the \pkg{Amelia} package (the \pkg{Amelia} package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, currently, the \pkg{MatchThem} package only supports the former datasets).
#' @param approach The approach used to combine information across imputed datasets. Currently, \code{"within"} (performing matching within each imputed dataset) and \code{"across"} (estimating propensity scores within each dataset, averaging them across datasets, and performing matching on the averaged propensity scores in each dataset) approaches are available. The default is \code{"within"}, which has been shown to have superior performance in most cases.
#' @param method This argument specifies a matching method. Currently, \code{"nearest"} (nearest neighbor matching), \code{"exact"} (exact matching), \code{"full"} (full matching), \code{"genetic"} (genetic matching), \code{"subclass"} (subclassication), \code{"cem"} (coarsened exact matching), and \code{"optimal"} (optimal matching) methods are available. Only methods that produce a propensity score (\code{"nearest"}, \code{"full"}, \code{"genetic"}, \code{"subclass"}, and \code{"optimal"}) are compatible with the \code{"across"} approach. The default is \code{"nearest"} for nearest neighbor matching. See \code{\link[MatchIt:matchit]{matchit()}} for details.
#' @param distance The method used to estimate the distance measure (e.g., propensity scores) used in matching, if any. Only options that specify a method of estimating propensity scores (i.e., not \code{"mahalanobis"}) are compatible with the \code{"across"} approach. The default is \code{"glm"} for propensity scores estimating using logistic regression. See \code{\link[MatchIt:matchit]{matchit()}} and \code{\link[MatchIt:distance]{distance}} for details and allowable options.
#' @param link,distance.options,discard,reestimate Arguments passed to \code{\link[MatchIt:matchit]{matchit()}} to control estimation of the distance measure (e.g., propensity scores).
#' @param ... Additional arguments passed to \code{\link[MatchIt:matchit]{matchit()}}.
#'
#' @description \code{matchthem()} performs matching in the supplied imputed datasets, given as \code{mids} or \code{amelia} objects, by running \code{\link[MatchIt:matchit]{MatchIt::matchit()}} on each of the imputed datasets with the supplied arguments.
#'
#' @details If an \code{amelia} object is supplied to \code{datasets}, it will first be transformed into a \code{mids} object for further use. \code{matchthem()} works by calling \code{\link[mice:complete]{mice::complete()}} on the \code{mids} object to extract a complete dataset, and then calls \code{\link[MatchIt:matchit]{MatchIt::matchit()}} on each one, storing the output of each \code{matchit()} call and the \code{mids} in the output. All arguments supplied to \code{matchthem()} except \code{datasets} and \code{approach} are passed directly to \code{matchit()}. With the across method, the estimated propensity scores are averaged across imputations and re-supplied to another set of calls to \code{matchit()}.
#'
#' @return An object of the \code{\link{mimids}} (matched multiply imputed datasets) class, which includes the supplied \code{mids} object (or an \code{amelia} object transformed into a \code{mids} object if supplied) and the output of the calls to \code{matchit()} on each imputed dataset.
#'
#' @seealso \code{\link[=mimids]{mimids}}
#' @seealso \code{\link[=with]{with}}
#' @seealso \code{\link[=pool]{pool}}
#' @seealso \code{\link[=weightthem]{weightthem}}
#' @seealso \code{\link[MatchIt:matchit]{MatchIt::matchit}}
#'
#' @author Farhad Pishgar and Noah Greifer
#'
#' @references Daniel Ho, Kosuke Imai, Gary King, and Elizabeth Stuart (2007). Matching as Nonparametric Preprocessing for Reducing Model Dependence in Parametric Causal Inference. \emph{Political Analysis}, 15(3): 199-236. \url{https://gking.harvard.edu/files/abs/matchp-abs.shtml}
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v045/i03/}
#' @references Gary King, James Honaker, Anne Joseph, and Kenneth Scheve (2001). Analyzing Incomplete Political Science Data: An Alternative Algorithm for Multiple Imputation. \emph{American Political Science Review}, 95: 49–69. \url{https://gking.harvard.edu/files/abs/evil-abs.shtml}
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
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                               imputed.datasets,
#'                               approach = 'within',
#'                               method = 'nearest')
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
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                               approach = 'across', method = 'nearest')}

matchthem <- function (formula, datasets,
                       approach = "within",
                       method = "nearest", distance = "glm", link = "logit",
                       distance.options = list(), discard = "none",
                       reestimate = FALSE, ...) {

  #External function

  #Importing functions
  #' @importFrom MatchIt matchit
  #' @importFrom mice complete as.mids
  #' @importFrom stats as.formula
  MatchIt::matchit
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
  if(!is.null(datasets$data$distance)) {stop("The input for the datasets shouldn't have a variable named 'distance'.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(!is.null(datasets$data$subclass)) {stop("The input for the datasets shouldn't have a variable named 'subclass'.")}
  if(!is.null(datasets$data$discarded)) {stop("The input for the datasets shouldn't have a variable named 'discarded'.")}
  if(!is.null(datasets$data$estimated.distance) && approach == "across") {stop("The input for the datasets shouldn't have a variable named 'estimated.distance', when the 'across' matching approch is selected.")}

  approach <- match.arg(approach, c("within","across"))
  if(approach == "across" && (!(method %in% c("nearest", "full", "subclass", "optimal", "genetic")))) {stop("The input for the matching method must be 'nearest', 'full', 'subclass', 'optimal', or 'genetic' when the 'across' matching approch is selected.")}
  if(approach == "across" && distance == "mahalanobis" ) {stop("The input for the distance should not be 'mahalanobis' when the 'across' matching approch is selected.")}
  if(!(method %in% c("nearest", "exact", "full", "genetic", "subclass", "cem", "optimal"))) {stop("The input for the matching method must be either 'nearest', 'exact', 'full', 'genetic', 'subclass', 'cem', or 'optimal'.")}

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

      #Building the model
      dataset <- mice::complete(datasets, i)

      #Printing out
      if (i == 1) message(paste0("\n", "Matching Observations  | dataset: #", i), appendLF = FALSE)
      else        message(paste0(" #", i), appendLF = FALSE)

      #Building the model
      model <- MatchIt::matchit(formula, dataset,
                                method = method, distance = distance,
                                distance.options = distance.options, discard = discard,
                                reestimate = reestimate, ...)

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

      #Building the model
      dataset <- mice::complete(datasets, i)

      #Printing out
      if (i == 1) message(paste0("Estimating distances   | dataset: #", i), appendLF = FALSE)
      else        message(paste0(" #", i), appendLF = FALSE)

      #Building the model
      model <- MatchIt::matchit(formula, dataset,
                                method = NULL, distance = distance,
                                distance.options = distance.options,
                                discard = modelslist[[i]]$discard,
                                reestimate = FALSE, ...)

      #Distance
      distancelist[[i]] <- model$distance
    }

    #Updating the distance
    d <- rowMeans(as.matrix(do.call(base::cbind, distancelist)))

    #Matching each dataset
    for (i in 1:datasets$m) {
      dataset <- mice::complete(datasets, i)

      #Printing out
      if (i == 1) message(paste0("\n", "Matching Observations  | dataset: #", i), appendLF = FALSE)
      else        message(paste0(" #", i), appendLF = FALSE)

      #Building the model
      model <- MatchIt::matchit(formula, data = dataset,
                                method = method, distance = d,
                                distance.options = NULL, discard = discard,
                                reestimate = FALSE, ...)

      #Updating the lists
      modelslist[[i]] <- model
    }
  }

  #Returning output
  output <- list(call = called,
                 object = datasets,
                 models = modelslist,
                 approach = approach)
  class(output) <- "mimids"
  message("\n", appendLF = FALSE)
  return(output)
}
