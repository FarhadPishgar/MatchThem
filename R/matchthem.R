#' @title Matches Multiply Imputed Datasets
#'
#' @rdname matchthem
#'
#' @aliases matchthem
#'
#' @param formula This argument takes the usual syntax of R formula, \code{z ~ x1 + x2}, where \code{z} is a binary treatment indicator and \code{x1} and \code{x2} are the potential confounders. Both the treatment indicator and the potential confounders must be contained in the imputed datasets, which is specified as \code{datasets} (see below). All of the usual R syntax for formula works. For example, \code{x1:x2} represents the first order interaction term between \code{x1} and \code{x2} and \code{I(x1^2)} represents the square term of \code{x1}. See \code{help(formula)} for details.
#' @param datasets This argument specifies the datasets containing the treatment indicator and the potential confounders called in the \code{formula}. This argument must be an object of the \code{mids} or \code{amelia} class, which is typically produced by a previous call to \code{mice()} or \code{mice.mids()} functions from the \pkg{mice} package or to \code{amelia} function from the \pkg{Amelia} package (the \pkg{Amelia} package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, although it may work with the latter, currently, the \pkg{MatchThem} package only supports the former datasets).
#' @param approach This argument specifies a matching approach. Currently, \code{"within"} (calculating distance measures within each imputed dataset and matching observations based on them ) and \code{"across"} (calculating distance measures within each imputed dataset, averaging distance measure for each observation across imputed datasets, and matching based on the averaged measures) approaches are available. The default is \code{"within"} which has been shown previously to produce unbiased results.
#' @param method This argument specifies a matching method. Currently, \code{"nearest"} (nearest neighbor matching) and \code{"exact"} (exact matching) methods are available. The default is \code{"nearest"}. Note that within each of these matching methods, \pkg{MatchThem} offers a variety of options.
#' @param distance This argument specifies the method used to estimate the distance measure. The default is logistic regression, \code{"logit"}. A variety of other methods are available.
#' @param distance.options This optional argument specifies the arguments that are passed to the model for estimating the distance measure. The input to this argument should be a list.
#' @param discard This argument specifies whether to discard observations that fall outside some measure of support of the distance score before matching and not allow them to be used at all in the matching procedure. Note that discarding observations may change the quantity of interest being estimated. The current options are \code{"none"} (discarding no observations before matching), \code{"both"} (discarding all observations, both the control and treatment observations, that are outside the support of the distance measure), \code{"control"} (discarding only control observations outside the support of the distance measure of the treatment observations), and \code{"treat"} (discarding only treatment observations outside the support of the distance measure of the control observations). The default is \code{"none"}.
#' @param reestimate This argument specifies whether the model for estimating the distance measure should be reestimated after observations are discarded. The input must be a logical value. The default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the matching method.
#'
#' @description The \code{matchthem()} function enables parametric models for causal inference to work better by selecting matched subsets of the control and treatment groups of imputed datasets of a \code{mids} or \code{amelia} class object.
#'
#' @details The matching is done using the \code{matchthem(z ~ x1, ...)} command, where \code{z} is the treatment indicator and \code{x1} represents the potential cofoudenr to be used in the matching model. There are a number of matching options. The default syntax is \code{matchthem(formula, datasets = NULL, method = "nearest", model = "logit", ratio = 1, caliper = 0, ...)}. Summaries of the results can be seen graphically using \code{plot()} or numerically using \code{summary()} functions. The \code{print()} function also prints out the output.
#'
#' @return This function returns an object of the \code{mimids} (matched multiply imputed datasets) class, that includes matched subsets of the imputed datasets primarily passed to the function by the \code{datasets} argument.
#'
#' @seealso \code{\link[=mimids]{mimids}}
#' @seealso \code{\link[=with]{with}}
#' @seealso \code{\link[=pool]{pool}}
#'
#' @author Farhad Pishgar
#'
#' @references Daniel Ho, Kosuke Imai, Gary King, and Elizabeth Stuart (2007). Matching as Nonparametric Preprocessing for Reducing Model Dependence in Parametric Causal Inference. \emph{Political Analysis}, 15(3): 199-236. \url{http://gking.harvard.edu/files/abs/matchp-abs.shtml}
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#' @references Gary King, James Honaker, Anne Joseph, and Kenneth Scheve (2001). Analyzing Incomplete Political Science Data: An Alternative Algorithm for Multiple Imputation. \emph{American Political Science Review}, 95: 49–69. \url{http://j.mp/2oOrtGs}
#'
#' @export
#'
#' @examples
#' \donttest{
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice(osteoarthritis, m = 5, maxit = 10,
#'                          method = c("", "", "mean", "polyreg", "logreg", "logreg", "logreg"))
#'
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                               approach = 'within', method = 'nearest')
#' }

matchthem <- function (formula, datasets,
                       approach = "within",
                       method = "nearest", distance = "logit", distance.options = list(),
                       discard = "none", reestimate = FALSE, ...) {

  #External function

  #Importing functions
  #' @importFrom mice complete
  #' @importFrom MatchIt matchit
  #' @importFrom stats as.formula
  mice::complete
  MatchIt::matchit
  stats::as.formula
  #' @export

  #Polishing variables
  formula <- stats::as.formula(formula)
  originals <- datasets
  if(approach == "pool-then-match") {approach == "across"}
  if(approach == "match-then-pool") {approach == "within"}

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(class(datasets) != "mids" && class(datasets) != "amelia") {stop("The input for the datasets must be an object of the 'mids' or 'amelia' class.")}
  if(!is.null(datasets$data$distance)) {stop("The input for the datasets shouldn't have a variable named 'distance'.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(method != "nearest" && method != "exact") {stop("The input for the matching method must be either 'nearest' or 'exact'.")}
  if(approach != "within" && approach != "across") {stop("The input for the matching approach must be either 'within' or 'across'.")}
  if(approach == "across" && method == "exact") {stop("The input for the matching method must be 'nearest', if the 'across' matching approch is selected.")}

  #Compatibility with amelia objects
  if (class(datasets) == "amelia") {
    imp0 <- datasets$imputations[[1]]
    is.na(imp0) <- datasets$missMatrix
    imp0$.id <- 1:nrow(imp0)
    imp0$.imp <- 0
    implist <- list(imp0)
    for (i in 1:datasets$m) {
      imp <- datasets$imputations[[i]]
      imp$.id <- 1:nrow(imp0)
      imp$.imp <- i
      implist[i+1] <- list(imp)
    }
    imp.datasets <- do.call("rbind", as.list(noquote(implist)))
    datasets <- as2.mids(imp.datasets)
  }

  #Within
  if (approach == "within") {

    #The raw data
    dataset0 <- datasets$data
    if (method != "exact") {dataset0$distance <- NA}
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$.imp <- 0

    #Defining the lists
    datasetslist <- list(dataset0)
    modelslist <- list(0)

    #Longing the datasets
    for (i in 1:datasets$m) {

      #Building the model
      dataset <- mice::complete(datasets, i)
      dataset$.id <- 1:nrow(datasets$data)
      model <- MatchIt::matchit(formula, dataset,
                                method = method, distance = distance,
                                distance.options = distance.options, discard = discard,
                                reestimate = reestimate, ...)

      #Printing out
      if (i == 1) cat("Matching Observations  | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Matched dataset
      matched.dataset <- match2.data(model, environment = environment())
      matched.dataset$weights <- NULL

      all.list <- 1:nrow(datasets$data)
      inc.list <- matched.dataset$.id
      exc.list <- setdiff(all.list, inc.list)
      num.list <- nrow(matched.dataset) + 1
      for (j in 1:length(exc.list)){
        matched.dataset[num.list, ".id"] <- exc.list[j]
        num.list <- num.list + 1
      }
      matched.dataset$.imp <- i
      matched.dataset <- matched.dataset[order(matched.dataset$.id),]
      row.names(matched.dataset) <- 1:nrow(datasets$data)

      #Updating the lists
      datasetslist[i+1] <- list(matched.dataset)
      modelslist[i+1] <- list(model)
    }

    #Binding the datasets
    matched.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    matched.datasets <- as2.mids(matched.datasets)

    #Others
    others <- list(approach. = approach, method. = method, source. = class(originals))

    #Returning output
    output <- list(object = matched.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.datasets = originals)
    class(output) <- "mimids"
    return(output)
  }

  #Across
  if (approach == "across") {

    #Raw data
    dataset0 <- datasets$data
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$distance <- NA
    dataset0$.imp <- 0

    #Defining the lists
    datasetslist <- list(dataset0)
    modelslist <- list(0)

    #Calculating the averaged distances
    for (i in 1:datasets$m) {

      #Building the model
      dataset <- mice::complete(datasets, i)
      dataset$.id <- 1:nrow(datasets$data)
      model <- MatchIt::matchit(formula, dataset,
                                method = method, distance = distance,
                                distance.options = distance.options, discard = discard,
                                reestimate = reestimate, ...)

      #Printing out
      if (i == 1) cat("Estimating distances   | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Distance
      if (i == 1) d <- model$distance
      if (i != 1) d <- d + model$distance
    }

    #Updating the distance
    d <- d / (datasets$m)

    #Matching each dataset
    for (i in 1:datasets$m) {
      dataset <- mice::complete(datasets, i)
      dataset$.id <- 1:nrow(datasets$data)
      dataset$estimated.distance <- d

      #Building the model
      model <- MatchIt::matchit(formula, dataset,
                                method = method, distance = dataset$estimated.distance,
                                distance.options = distance.options, discard = discard,
                                reestimate = reestimate, ...)

      #Printing out
      if (i == 1) cat("\n", "Matching Observations  | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Matched dataset
      matched.dataset <- match2.data(model, environment = environment())
      matched.dataset$weights <- NULL
      matched.dataset$estimated.distance <- NULL

      all.list <- 1:nrow(datasets$data)
      inc.list <- matched.dataset$.id
      exc.list <- setdiff(all.list, inc.list)
      num.list <- nrow(matched.dataset) + 1
      for (j in 1:length(exc.list)){
        matched.dataset[num.list, ".id"] <- exc.list[j]
        num.list <- num.list + 1
      }
      matched.dataset$.imp <- i
      matched.dataset <- matched.dataset[order(matched.dataset$.id),]
      row.names(matched.dataset) <- 1:nrow(datasets$data)

      #Updating the lists
      datasetslist[i+1] <- list(matched.dataset)
      modelslist[i+1] <- list(model)
    }

    #Binding the datasets
    matched.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    matched.datasets <- as2.mids(matched.datasets)

    #Others
    others <- list(approach. = approach, method. = method, source. = class(originals))

    #Returning output
    output <- list(object = matched.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.datasets = originals)
    class(output) <- "mimids"
    return(output)
  }
}
