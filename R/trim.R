#' @title Trim Weights
#'
#' @name trim
#'
#' @rdname trim
#'
#' @aliases trim trim.wimids
#'
#' @inheritParams WeightIt::trim
#' @param x A `wimids` object; the output of a call to [`weightthem()`][weightthem].
#' @param ... Ignored.
#'
#' @description Trims (i.e., truncates) large weights by setting all weights higher than that at a given quantile to the weight at the quantile. This can be useful in controlling extreme weights, which can reduce effective sample size by enlarging the variability of the weights.
#'
#' @details `trim.wimids()` works by calling [WeightIt::trim()] on each `weightit` object stored in the `models` component of the `wimids` object. Because `trim()` itself is not exported from \pkg{MatchThem}, it must be called using `WeightIt::trim()` or by attaching \pkg{WeightIt} (i.e., running `library(WeightIt)`) before use.
#'
#' @return An object from the `wimids` class, identical to the original object except with `trim()` applied to each of the `weightit` objects in the `models` component.
#'
#' @seealso [WeightIt::trim()]
#'
#' @author Noah Greifer
#'
#' @export trim
#'
#' @examples \donttest{#Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice::mice(osteoarthritis, m = 5)
#'
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                                 imputed.datasets,
#'                                 approach = 'within',
#'                                 method = 'glm',
#'                                 estimand = "ATE")
#'
#' #Trimming the top 10% of weights in each dataset
#' #to the 90th percentile
#' trimmed.datasets <- trim(weighted.datasets, at = 0.9)}

#' @method trim wimids
#'
#' @export

trim.wimids <- function (x, at = 0, lower = FALSE, ...) {

  #External function

  #Importing functions
  #' @importFrom WeightIt trim
  WeightIt::trim
  #' @export

  for (i in seq_along(x$models)) {
    suppressMessages(x$models[[i]] <- WeightIt::trim(x$models[[i]], at = at, lower = lower, ...))
  }
  return(x)
}
