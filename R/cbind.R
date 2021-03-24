#' @title Combine \code{mimids} and \code{wimids} Objects by Columns
#'
#' @name cbind
#'
#' @rdname cbind
#'
#' @aliases cbind cbind.mimids cbind.wimids
#'
#' @param ... Objects to combine columnwise. The first should be a \code{mimids} or \code{wimids} object. Additional \code{data.frame}s, \code{matrix}es, \code{factor}s, or \code{vector}s can be supplied. These can be given as named arguments.
#' @param deparse.level Ignored.
#'
#' @description This function combines a \code{mimids} or \code{wimids} object columnwise with additional datasets or variables. Typically these would be variables not included in the original imputation and therefore absent in the \code{mimids} or \code{wimids} object. \code{with()} can then be used on the output to run models with the added variables.
#'
#' @return An object with the same class as the first input object with the additional variables added to the components.
#'
#' @seealso \code{\link[mice:cbind.mids]{mice::cbind.mids}}, \code{\link{cbind}}
#'
#' @author Farhad Pishgar and Noah Greifer
#'
#' @export
#'
#' @examples \donttest{#Loading libraries
#' library(MatchThem)
#' library(survey)
#'
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice::mice(osteoarthritis, m = 5)
#'
#' #Weighting the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                                approach = 'within')
#'
#' #Adding additional variables
#' weighted.datasets <- cbind(weighted.datasets,
#'                            logAGE = log(osteoarthritis$AGE))
#'
#' #Using the additional variables in an analysis
#' models <- with(weighted.datasets,
#'                svyglm(KOA ~ OSP + logAGE, family = quasibinomial))
#'
#' #Pooling results obtained from analyzing the datasets
#' results <- pool(models)
#' summary(results)}

cbind <- function(..., deparse.level = 1) {

  #External function
  #S3 method

  UseMethod("cbind")
}

#' @rdname cbind
#'
#' @method cbind mimids
#'
#' @export

cbind.mimids <- function(..., deparse.level = 1) {

  #S3 method

  #Importing functions
  #' @importFrom mice cbind
  mice::cbind
  #' @export

  #Checking inputs format
  if(...length() == 1) return(..1)
  if(!(is.mimids(..1)) && !(is.wimids(..1)))
    stop("The first argument must be a 'mimids' or 'wimids' object.")

  dots <- list(...)
  classed <- class(dots[[1]])
  x <- dots[[1]]
  dots[[1]] <- x$object

  x$object <- do.call(mice::cbind, dots)

  #Returning output
  return(x)
}

#' @rdname cbind
#'
#' @method cbind wimids
#'
#' @export

cbind.wimids <- cbind.mimids