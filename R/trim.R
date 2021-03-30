#' @title Trim Weights
#'
#' @name trim
#'
#' @rdname trim
#'
#' @aliases trim trim.wimids
#'
#' @param w A \code{wimids} object; the output of a call to \code{\link[=weightthem]{weightthem()}}.
#' @param at \code{numeric}; either the quantile of the weights above which weights are to be trimmed (given as a single number between .5 and 1) or the number of weights to be trimmed (e.g., \code{at = 3} for the top 3 weights to be set to the 4th largest weight).
#' @param lower \code{logical}; whether also to trim at the lower quantile (e.g., for \code{at = .9}, trimming at both the .1 and .9 quantiles, or for \code{at = 3}, trimming the top and bottom 3 weights).
#' @param ... Ignored.
#'
#' @description Trims (i.e., truncates) large weights by setting all weights higher than that at a given quantile to the weight at the quantile. This can be useful in controlling extreme weights, which can reduce effective sample size by enlarging the variability of the weights.
#'
#' @details \code{trim.wimids()} works by calling \code{\link[WeightIt:trim]{WeightIt::trim()}} on each \code{weightit} object stored in the \code{models} component of the \code{wimid} object. Because \code{trim()} itself is not exported from \pkg{MatchThem}, it must be called using \code{WeightIt::trim()} or by attaching \pkg{WeightIt} (i.e., running \code{library(WeightIt)}) before use. See Example.
#'
#' @return An object of class \code{wimids}, identical to the original object except with \code{trim()} applied to each of the \code{weightit} objects in the \code{models} component.
#'
#' @seealso \code{\link[WeightIt:trim]{WeightIt::trim()}}
#'
#' @author Noah Greifer
#'
#' @export trim
#'
#' @examples \donttest{#Loading libraries
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
#'                                 estimand = "ATE")
#'
#' #Trimming the top 10% of weights in each dataset
#' #to the 90th percentile
#' trimmed.datasets <- trim(weighted.datasets, at = .9)}

#' @method trim wimids
#'
#' @export

trim.wimids <- function (w, at = 0, lower = FALSE, ...) {

  #External function

  #Importing functions
  #' @importFrom WeightIt trim
  WeightIt::trim
  #' @export

  for (i in seq_along(w$models)) {
    suppressMessages(w$models[[i]] <- WeightIt::trim(w$models[[i]], at = at, lower = lower, ...))
  }
  return(w)
}