#' @title Outputs Matched Imputed Datasets
#'
#' @rdname matchthem.data
#'
#' @aliases matchthem.data
#'
#' @param object This argument specifies an object of the \code{mimids} class.
#' @param n This argument specifies the matched imputed dataset number, intended to extract its matching data. The input must be a positive integer. The default is \code{1}.
#'
#' @description The \code{matchthem.data()} function extracts matching data from an object of the \code{mimids} class.
#'
#' @details The matched datasets within the \code{mimids} class object are extracted.
#'
#' @return This function returns a subset of the imputed dataset after matching with just the matched observations from control and treatment groups.
#'
#' @seealso \code{\link[=mimids]{mimids}}
#'
#' @author Farhad Pishgar
#'
#' @references Daniel Ho, Kosuke Imai, Gary King, and Elizabeth Stuart (2007). Matching as Nonparametric Preprocessing for Reducing Model Dependence in Parametric Causal Inference. \emph{Political Analysis}, 15(3): 199-236. \url{http://gking.harvard.edu/files/abs/matchp-abs.shtml}
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
#'
#' #Extracting the first imputed dataset
#' matched.dataset.1 <- matchthem.data(matched.datasets, n = 1)
#' }

matchthem.data <- function (object, n = 1) {

  #External function

  #Importing functions
  #' @importFrom stats complete.cases
  stats::complete.cases
  #' @export

  #Checking inputs format
  if(object$object$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Returning the output
  output <- object$datasets[[n + 1]][stats::complete.cases(object$datasets[[n + 1]][ , "distance"]),]
  output$.id <- NULL
  output$.imp <- NULL
  return(output)
}

