#' @title Pools Estimates by Rubin's Rules
#'
#' @rdname pool
#'
#' @aliases pool
#'
#' @param object This argument specifies an object of the \code{mimira} class (produced by a previous call to \code{with()} function).
#' @param dfcom This argument specifies a positive number representing the degrees of freedom in the data analysis. The default is \code{NULL}, which means to extract this information from the fitted model with the lowest number of observations or the first fitted model (when that fails the warning \code{The function cannot extract the dfcom from the datasets, hence, large sample is assumed.} is printed and the parameter is set to \code{999999}).
#'
#' @description \code{pool()} function pools estimates from \code{n} repeated data analyses. The typical sequence of steps to do a matching procedure on the imputed datasets are:
#' \enumerate{
#'  \item Impute the missing values by the \code{mice()} function (from the \pkg{mice} package) or the \code{amelia()} function (from the \pkg{Amelia} package), resulting in a multiple imputed dataset (an object of the \code{mids} or \code{amelia} class);
#'  \item Match each imputed dataset using a matching model by the \code{matchthem()} function, resulting in an object of the \code{mimids} class;
#'  \item Check the extent of balance of covariates across the matched datasets;
#'  \item Fit the statistical model of interest on each matched dataset by the \code{with()} function, resulting in an object of the \code{mimira} class; and
#'  \item Pool the estimates from each model into a single set of estimates and standard errors, resulting in an object of the \code{mimipo} class.
#' }
#'
#' @details \code{pool()} function averages the estimates of the model and computes the total variance over the repeated analyses by Rubin’s rules.
#'
#' @return This function returns an object of the \code{mimipo} class.
#'
#' @seealso \code{\link[=with]{with}}
#'
#' @author Extracted from the \pkg{mice} package written by Stef van Buuren et al. with changes
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'
#' @export
#'
#' @examples \donttest{#Loading libraries
#' library(mice)
#' library(MatchThem)
#'
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice(osteoarthritis, m = 5, maxit = 10,
#'                          method = c("", "", "mean", "polyreg",
#'                                     "logreg", "logreg", "logreg"))
#'
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                               approach = 'within', method = 'nearest')
#'
#' #Analyzing the matched datasets
#' models <- with(data = matched.datasets,
#'                exp = glm(KOA ~ OSP, family = binomial))
#'
#' #Pooling results obtained from analysing the datasets
#' results <- pool(models)}

pool <- function (object, dfcom = NULL) {

  #External function
  #S3 method

  #Based on: The mice::pool()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Few

  UseMethod("pool")
}

#' @export

pool.mira <- function (object, dfcom = NULL) {

  #External function
  #S3 method

  #Based on: The mice::pool()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Few

  #Importing functions
  #' @importFrom mice is.mira pool
  mice::is.mira
  mice::pool
  #' @export

  #mids
  if (mice::is.mira(object)) {
    output <- mice::pool(object = object, dfcom = dfcom)
    return(output)
  }
}

#' @export

pool.mimira <- function (object, dfcom = NULL) {

  #External function
  #S3 method

  #Based on: The mice::pool()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Few

  #Importing functions
  #' @importFrom mice pool getfit
  #' @importFrom utils packageVersion
  mice::pool
  #' @export

  call <- match.call()

  dfcom <- get.dfcom2(object, dfcom)

  #Make sure robust SEs are used for coxph models
  #Not needed for mice version >= 3.13.2
  if (utils::packageVersion("mice") < "3.13.2") {
    model <- mice::getfit(object, 1L)
    if (inherits(model, "coxph")) {
      for (i in seq_along(object$analyses)) {
        #Remove naive.var so summary.coxph uses var as std.error
        object$analyses[[i]]$naive.var <- NULL
      }
    }
  }

  output <- mice::pool(object, dfcom = dfcom)

  output$call <- NULL
  output <- c(list(call = call), as.list(output))

  class(output) <- c("mimipo", "mipo", "data.frame")

  #Return the output
  return(output)
}
