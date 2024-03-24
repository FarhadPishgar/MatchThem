#' @title Matches Multiply Imputed Datasets
#'
#' @rdname matchthem
#'
#' @aliases matchthem
#'
#' @param formula A `formula` of the form `z ~ x1 + x2`, where `z` is the exposure and `x1` and `x2` are the covariates to be balanced, which is passed directly to [MatchIt::matchit()] to specify the propensity score model or treatment and covariates to be used in matching. See [MatchIt::matchit()] for details.
#' @param datasets This argument specifies the datasets containing the exposure and the potential confounders called in the `formula`. This argument must be an object of the `mids` or `amelia` class, which is typically produced by a previous call to `mice()` function from the \pkg{mice} package or to `amelia()` function from the \pkg{Amelia} package (the \pkg{Amelia} package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, currently, the \pkg{MatchThem} package only supports the former datasets).
#' @param approach The approach that should be used to combine information in multiply imputed datasets. Currently, `"within"` (performing matching within each dataset) and `"across"` (estimating propensity scores within each dataset, averaging them across datasets, and performing matching using the averaged propensity scores in each dataset) approaches are available. The default is `"within"`, which has been shown to have superior performance in most cases.
#' @param method This argument specifies a matching method. Currently, `"nearest"` (nearest neighbor matching), `"exact"` (exact matching), `"full"` (optimal full matching), `"genetic"` (genetic matching), `"subclass"` (subclassication), `"cem"` (coarsened exact matching), `"optimal"` (optimal pair matching), `"quick"` (generalized full matching), and `("cardinality")` (cardinality and profile matching) methods are available. Only methods that produce a propensity score (`"nearest"`, `"full"`, `"genetic"`, `"subclass"`, `"optimal"`, and `"quick"`) are compatible with the `"across"` approach. The default is `"nearest"` for nearest neighbor matching. See [MatchIt::matchit()] for details.
#' @param distance The method used to estimate the distance measure (e.g., propensity scores) used in matching, if any. Only options that specify a method of estimating propensity scores (i.e., not `"mahalanobis"`) are compatible with the `"across"` approach. The default is `"glm"` for estimating propensity scores using logistic regression. See [MatchIt::matchit()] and [`MatchIt::distance`] for details and allowable options.
#' @param link,distance.options,discard,reestimate Arguments passed to [MatchIt::matchit()] to control estimation of the distance measure (e.g., propensity scores).
#' @param ... Additional arguments passed to [MatchIt::matchit()].
#'
#' @description `matchthem()` performs matching in the supplied multiply imputed datasets, given as `mids` or `amelia` objects, by running [MatchIt::matchit()] on each of the multiply imputed datasets with the supplied arguments.
#'
#' @details If an `amelia` object is supplied to `datasets`, it will be transformed into a `mids` object for further use. `matchthem()` works by calling [mice::complete()] on the `mids` object to extract a complete dataset, and then calls [MatchIt::matchit()] on each one, storing the output of each `matchit()` call and the `mids` in the output. All arguments supplied to `matchthem()` except `datasets` and `approach` are passed directly to `matchit()`. With the `"across"` approach, the estimated propensity scores are averaged across multiply imputed datasets and re-supplied to another set of calls to `matchit()`.
#'
#' @return An object of the [mimids()] (matched multiply imputed datasets) class, which includes the supplied `mids` object (or an `amelia` object transformed into a `mids` object if supplied) and the output of the calls to `matchit()` on each multiply imputed dataset.
#'
#' @seealso [`mimids`]
#' @seealso [with()]
#' @seealso [pool()]
#' @seealso [weightthem()]
#' @seealso [MatchIt::matchit()]
#'
#' @author Farhad Pishgar and Noah Greifer
#'
#' @references Daniel Ho, Kosuke Imai, Gary King, and Elizabeth Stuart (2007). Matching as Nonparametric Preprocessing for Reducing Model Dependence in Parametric Causal Inference. *Political Analysis*, 15(3): 199-236. <https://gking.harvard.edu/files/abs/matchp-abs.shtml>
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). `mice`: Multivariate Imputation by Chained Equations in `R`. *Journal of Statistical Software*, 45(3): 1-67. \doi{10.18637/jss.v045.i03}
#' @references Gary King, James Honaker, Anne Joseph, and Kenneth Scheve (2001). Analyzing Incomplete Political Science Data: An Alternative Algorithm for Multiple Imputation. *American Political Science Review*, 95: 49–69. <https://gking.harvard.edu/files/abs/evil-abs.shtml>
#'
#' @export
#'
#' @examples \donttest{#1
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
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- Amelia::amelia(osteoarthritis, m = 5,
#'                                    noms = c("SEX", "RAC", "SMK", "OSP", "KOA"))
#'
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                               imputed.datasets,
#'                               approach = 'across',
#'                               method = 'nearest')}

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

  approach <- match.arg(approach, c("within", "across"))
  if(approach == "across" && (!(method %in% c("nearest", "full", "subclass", "optimal", "genetic", "quick")))) {stop("The input for the matching method must be 'nearest', 'full', 'subclass', 'optimal', 'genetic', or 'quick' when the 'across' matching approch is selected.")}
  if(approach == "across" && distance == "mahalanobis" ) {stop("The input for the distance should not be 'mahalanobis' when the 'across' matching approch is selected.")}
  if(!(method %in% c("nearest", "exact", "full", "genetic", "subclass", "cem", "optimal", "quick", "cardinality"))) {stop("The input for the matching method must be either 'nearest', 'exact', 'full', 'genetic', 'subclass', 'cem', 'optimal', 'quick', or 'cardinality'.")}

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
