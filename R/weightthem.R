#' @title Weights Multiply Imputed Datasets
#'
#' @rdname weightthem
#'
#' @aliases weightthem
#'
#' @param formula This argument takes the usual syntax of R formula, \code{z ~ x1 + x2}, where \code{z} is a binary treatment indicator and \code{x1} and \code{x2} are the potential confounders. Both the treatment indicator and the potential confounders must be contained in the imputed datasets, which is specified as \code{datasets} (see below). All of the usual R syntax for formula works. For example, \code{x1:x2} represents the first order interaction term between \code{x1} and \code{x2} and \code{I(x1^2)} represents the square term of \code{x1}. See \code{help(formula)} for details.
#' @param datasets This argument specifies the datasets containing the treatment indicator and the potential confounders called in the \code{formula}. This argument must be an object of the \code{mids} or \code{amelia} class, which is typically produced by a previous call to \code{mice()} or \code{mice.mids()} functions from the \pkg{mice} package or to \code{amelia} function from the \pkg{Amelia} package (the \pkg{Amelia} package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, although it may work with the latter, currently, the \pkg{MatchThem} package only supports the former datasets).
#' @param approach This argument specifies a matching approach. Currently, \code{"within"} (calculating distance measures within each imputed dataset and weighting observations based on them ) and \code{"across"} (calculating distance measures within each imputed dataset, averaging distance measure for each observation across imputed datasets, and weighting based on the averaged measures) approaches are available. The default is \code{"within"} which has been shown previously to produce unbiased results.
#' @param method This argument specifies the method that will be used to estimate weights. Currently, \code{"ps"} (propensity score weighting using generalized linear models), \code{"gbm"} (propensity score weighting using generalized boosted modeling), \code{"cbps"} (covariate balancing propensity score weighting), \code{"npcbps"} (non-parametric covariate balancing propensity score weighting), \code{"ebal"} (entropy balancing), \code{"ebcw"} (empirical balancing calibration weighting), \code{"optweight"} (optimization-based weighting), \code{"super"} (propensity score weighting using SuperLearner), and \code{"user-defined"} (weighting using a user-defined weighting function) are available.  The default is \code{"ps"}. Note that within each of these weighting methods, \pkg{MatchThem} offers a variety of options.
#' @param estimand This argument specifies the desired estimand. For binary and multinomial treatments, can be \code{"ATE"}, \code{"ATT"}, \code{"ATC"}, and, for some weighting methods, \code{"ATO"} or \code{"ATM"}. The default is \code{"ATE"}. Please see the \pkg{WeightIt} package reference manual <https://cran.r-project.org/package=WeightIt> for more details.
#' @param stabilize This argument specifies whether to stabilize the weights. For the methods that involve estimating propensity scores, this involves multiplying each observation weight by the sum of the weights in the observation group (control or treatment). The default is \code{FALSE}. Please see the \pkg{WeightIt} package reference manual <https://cran.r-project.org/package=WeightIt> for more details.
#' @param focal This argument specifies which group to consider as the treatment or the focal group (when multinomial treatments are used and the \code{"ATT"} is requested). This group will not be weighted, and the other groups will be weighted to be more like the focal group. Please see the \pkg{WeightIt} package reference manual <https://cran.r-project.org/package=WeightIt> for more details.
#' @param by This argument specifies a vector or the names of variables in the datasets, for which weighting should be done within categories. For example, if by = \code{"gender"}, weights will be generated separately within each level of the variable \code{"gender"}.
#' @param s.weights This argument specifies a vector of sampling weights or the name of a variable in the datasets that contains sampling weights. These can also be matching weights if weighting is to be used on matched data.
#' @param ps This argument specifies a vector of propensity scores or the name of a variable in the datasets containing the propensity scores. If not \code{NULL}, weighting method is ignored, and the propensity scores will be used to create weights (in this case, \code{formula} must include the treatment indicator in the datasets, but the listed covariates will play no role in the weight estimation).
#' @param moments This argument specifies the greatest moment of the covariate distribution to be balanced (for entropy balancing, empirical balancing calibration weights, and optimization-based weights). For example, if \code{moments = 3}, for all non-categorical covariates, the mean, second moment (variance), and third moments (skew) of the covariates will be balanced. This argument is ignored for other weighting methods; to balance powers of the covariates, appropriate functions must be entered in the \code{formula}. Please see the \pkg{WeightIt} package reference manual <https://cran.r-project.org/package=WeightIt> for more details.
#' @param int This argument specifies whether first-order interactions of the covariates should be balanced (essentially balancing the covariances between covariates, for entropy balancing, empirical balancing calibration weights, and optimization-based weights). This argument is ignored for other weighting methods; to balance interactions between the variables, appropriate functions must be entered in the \code{formula}. The default is \code{FALSE}. Please see the \pkg{WeightIt} package reference manual <https://cran.r-project.org/package=WeightIt> for more details.
#' @param verbose This argument specifies whether to print additional information output by the fitting function. The default is \code{FALSE}.
#' @param include.obj This argument specifies whether to include in the output any fit objects created in the process of estimating the weights. For example, with \code{method = "ps"}, the \code{glm} objects containing the propensity score model will be included.  The default is \code{FALSE}. Please see the \pkg{WeightIt} package reference manual <https://cran.r-project.org/package=WeightIt> for more details.
#' @param ... Additional arguments to be passed to the matching method.
#'
#' @description The \code{weightthem()} function enables parametric models for causal inference to work better by estimating weights of the control and treatment observations in each imputed dataset of a \code{mids} or \code{amelia} class object.
#'
#' @details The weighting is done using the \code{weightthem(z ~ x1, ...)} command, where \code{z} is the treatment indicator and \code{x1} represents the potential confunders to be used in the weighting model. The default syntax is \code{weightthem(formula, datasets = NULL, method = "ps", ...)}. Summaries of the results can be seen numerically using \code{summary()} functions. The \code{print()} function also prints out the output.
#'
#' @return This function returns an object of the \code{wimids} (weighted multiply imputed datasets) class, that includes weights of observations of the imputed datasets (listed as the \code{weights} variables in each) primarily passed to the function by the \code{datasets} argument.
#'
#' @seealso \code{\link[=wimids]{wimids}}
#' @seealso \code{\link[=with]{with}}
#' @seealso \code{\link[=pool]{pool}}
#'
#' @author Farhad Pishgar
#'
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
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                                 approach = 'within', method = 'ps')
#' }

weightthem <- function (formula, datasets,
                        approach = "within",
                        method = "ps", estimand = "ATE", stabilize = FALSE, focal = NULL, by = NULL, s.weights = NULL,
                        ps = NULL, moments = 1, int = FALSE, verbose = FALSE, include.obj = FALSE, ...) {

  #External function

  #Importing functions
  #' @importFrom mice complete
  #' @importFrom WeightIt weightit
  #' @importFrom stats as.formula
  mice::complete
  WeightIt::weightit
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
  if(!is.null(datasets$data$p.s.)) {stop("The input for the datasets shouldn't have a variable named 'p.s.'.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(!(method %in% c("ps", "gbm", "cbps", "npcbps", "ebal", "ebcw", "optweight", "super", "user-defined"))) {stop("The input for the weighting method must be 'ps', 'gbm', 'cbps', 'npcbps', 'ebal', 'ebcw', 'optweight', 'super', or 'user-defined'.")}
  if(approach != "within" && approach != "across") {stop("The input for the weighting approach must be either 'within' or 'across'.")}
  if(approach == "across" && (!(method %in% c("ps", "gbm", "cbps", "super")))) {stop("The input for the weighting method must be 'ps', if the 'across' weighting approch is selected.")}

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
    dataset0$weights <- NA
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$.imp <- 0

    #Defining the lists
    datasetslist <- list(dataset0)
    modelslist <- list(0)

    #Longing the datasets
    for (i in 1:datasets$m) {

      #Building the model
      dataset <- mice::complete(datasets, i)
      model <- WeightIt::weightit(formula, dataset,
                        method = method, estimand = estimand, stabilize = stabilize, focal = focal, by = by, s.weights = s.weights,
                        ps = ps, moments = moments, int = int, verbose = verbose, include.obj = include.obj, ...)

      #Printing out
      if (i == 1) cat("Estimating weights     | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Dataset
      dataset$weights <- model$weights
      dataset$.id <- 1:nrow(datasets$data)
      dataset$.imp <- i

      #Updating the lists
      datasetslist[i+1] <- list(dataset)
      modelslist[i+1] <- list(model)
    }

    #Binding the datasets
    weighted.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    weighted.datasets <- as2.mids(weighted.datasets)

    #Others
    others <- list(approach. = approach, method. = method, source. = class(originals))

    #Returning output
    output <- list(object = weighted.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.datasets = originals)
    class(output) <- "wimids"
    return(output)
  }

  #Across
  if (approach == "across") {

    #Raw data
    dataset0 <- datasets$data
    dataset0$weights <- NA
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$.imp <- 0

    #Defining the lists
    datasetslist <- list(dataset0)
    modelslist <- list(0)

    #Calculating the averaged distances
    for (i in 1:datasets$m) {

      #Building the model
      dataset <- mice::complete(datasets, i)
      model <- WeightIt::weightit(formula, dataset,
                        method = method, estimand = estimand, stabilize = stabilize, focal = focal, by = by, s.weights = s.weights,
                        ps = ps, moments = moments, int = int, verbose = verbose, include.obj = include.obj, ...)

      #Printing out
      if (i == 1) cat("Estimating distances   | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Measures
      if (i == 1) p <- model$ps
      if (i != 1) p <- p + model$ps
    }

    #Updating the weights
    p <- p / (datasets$m)

    #Adding averaged weights to datasets
    for (i in 1:(datasets$m)) {
      dataset <- mice::complete(datasets, i)
      dataset$p.s. <- p

      #Building the model
      model <- WeightIt::weightit(formula, dataset,
                        method = method, estimand = estimand, stabilize = stabilize, focal = focal, by = by, s.weights = s.weights,
                        ps = dataset$p.s., moments = moments, int = int, verbose = verbose, include.obj = include.obj, ...)

      #Dataset
      dataset$weights <- model$weights
      dataset$.id <- 1:nrow(datasets$data)
      dataset$.imp <- i
      dataset$p.s. <- NULL

      #Printing out
      if (i == 1) cat("\n", "Estimating weights     | dataset: #", i, sep = "")
      if (i != 1) cat(" #", i, sep = "")

      #Updating the list
      datasetslist[i+1] <- list(dataset)
      modelslist[i+1] <- list(model)
    }

    #Binding the datasets
    weighted.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    weighted.datasets <- as2.mids(weighted.datasets)

    #Others
    others <- list(approach. = approach, method. = method, source. = class(originals))

    #Returning output
    output <- list(object = weighted.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.datasets = originals)
    class(output) <- "wimids"
    return(output)
  }
}
