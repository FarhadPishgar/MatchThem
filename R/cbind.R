#' @title Combine \code{mimids} and \code{wimids} objects by columns
#'
#' @aliases cbind.mimids cbind.wimids
#'
#' @param ... Objects to combine columnwise. The first should be a \code{mimids} or \code{wimids} object. Additional \code{data.frame}s, \code{matrix}es, \code{factor}s, or \code{vector}s can be supplied. These can be given as named arguments.
#' @param deparse.level Passed to \code{\link[base]{cbind}}.
#'
#' @description This function combines a \code{mimids} or \code{wimids} object columnwise with additional datasets or variables. Typically these would be variables not included in the original imputation and therefore absent in the \code{mimids} or \code{wimids} object. \code{with()} can then be used on the output to run models with the added variables.
#'
#' @return An object with the same class as the first input object with the additional variables added to the components.
#'
#' @seealso \code{\link[mice:cbind.mids]{mice::cbind.mids}}
#'
#' @author Farhad Pishgar and Noah Greifer
#'
#' @examples \donttest{#Loading libraries
#' library(mice)
#' library(MatchThem)
#' library(survey)
#'
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice(osteoarthritis, m = 5, maxit = 10,
#'                          method = c("", "", "mean", "polyreg",
#'                                     "logreg", "logreg", "logreg"))
#'
#' #Weighting the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                                approach = 'within')
#'
#' #Adding additional variables
#' weighted.datasets <- cbind(weighted.datasets, logAGE = log(osteoarthritis$AGE))
#'
#' #Using the additional variables in an analysis
#' pool(with(weighted.datasets, svyglm(KOA ~ OSP + logAGE, family = quasibinomial)))
#'}

#' @export
#' @method cbind mimids
cbind.mimids <- function(..., deparse.level = 1) {

  #Internal function
  #S3 method

  #Importing functions
  #' @importFrom mice complete cbind
  mice::complete
  mice::cbind

  #Checking inputs format
  if(...length() == 1) return(..1)
  if(!(is.mimids(..1)) && !(is.wimids(..1))) {stop("The first argument must be a 'mimids' or 'wimids' object.")}

  # for (i in seq_len(...length())[-1]) {
  #   if(!is.data.frame(...elt(i))) {
  #     stop(paste0("Each other supplied input must be a data frame, matrix, or atomic vector when the first argument is a '", class(x), "' object."))
  #   }
  # }

  dots <- list(...)
  x <- dots[[1]]

  #Polishing variables
  call <- x$call
  modelslist <- x$models
  others <- x$others
  datasets <- x$object

  data.0 <- datasets$data
  data.0$.id <- 1:nrow(datasets$data)
  data.0$.imp <- 0
  data.0 <- do.call("cbind", c(list(data.0), dots[-1], list(deparse.level = deparse.level)), quote = TRUE)

  #Preparing the list
  datasetslist <- vector("list", datasets$m + 1)
  datasetslist[[1]] <- data.0

  #Binding
  for (i in 1:datasets$m) {
    data.i <- mice::complete(datasets, i)
    data.i$.id <- 1:nrow(datasets$data)
    data.i$.imp <- i
    data.i <- do.call("cbind", c(list(data.i), dots[-1], list(deparse.level = deparse.level)), quote = TRUE)
    datasetslist[[i+1]] <- data.i
  }

  #Prepating the output
  new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
  matched.datasets <- as2.mids(new.datasets)
  others$source <- do.call("cbind", c(list(others$source), dots[-1]), quote = TRUE) #cbind.mids from mice

  #Returning output
  output <- list(call = call,
                 object = matched.datasets,
                 models = modelslist,
                 datasets = datasetslist,
                 others = others)
  class(output) <- class(x)
  return(output)

}


#' @rdname cbind.mimids
#' @export
#' @method cbind wimids

cbind.wimids <- cbind.mimids