#' @title Outputs Weighted Imputed Datasets
#'
#' @rdname weightthem.data
#'
#' @aliases weightthem.data
#'
#' @param object This argument specifies an object of the \code{wimids} class.
#' @param n This argument specifies the weighted imputed dataset number, intended to extract its data. The input must be a positive integer. The default is \code{1}.
#'
#' @description The \code{weightthem.data()} function extracts data from an object of the \code{wimids} class.
#'
#' @details The weighted datasets within the \code{wimids} class object are extracted.
#'
#' @return This function returns the imputed dataset after weighting, with weights of observations included in the dataset (listed as the \code{weights} variables).
#'
#' @seealso \code{\link[=wimids]{wimids}}
#'
#' @author Farhad Pishgar
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
#'
#' #Extracting the first imputed dataset
#' weighted.dataset.1 <- weightthem.data(weighted.datasets, n = 1)
#' }

weightthem.data <- function (object, n = 1) {

  #External function

  #Importing functions
  #' @importFrom stats complete.cases
  stats::complete.cases
  #' @export

  #Checking inputs format
  if(object$object$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Returning the output
  output <- object$datasets[[n + 1]][stats::complete.cases(object$datasets[[n + 1]][ , "weights"]),]
  output$.id <- NULL
  output$.imp <- NULL
  return(output)
}
