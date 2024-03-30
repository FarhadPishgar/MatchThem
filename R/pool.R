#' @title Pools Estimates by Rubin's Rules
#'
#' @rdname pool
#'
#' @aliases pool
#'
#' @param object An object of the [`mimira`] class (produced by a previous call to `with()`).
#' @param dfcom A positive number representing the degrees of freedom in the data analysis. The default is `NULL`, which means to extract this information from the fitted model with the lowest number of observations or the first fitted model (when that fails the parameter is set to `999999`).
#'
#' @description `pool()` pools estimates from the analyses done within each multiply imputed dataset. The typical sequence of steps to do a matching or weighting procedure on multiply imputed datasets are:
#' \enumerate{
#'  \item Multiply impute the missing values using the `mice()` function (from the \pkg{mice} package) or the `amelia()` function (from the \pkg{Amelia} package), resulting in a multiply imputed dataset (an object of the `mids` or `amelia` class);
#'  \item Match or weight each multiply imputed dataset using `matchthem()` or `weightthem()`, resulting in an object of the `mimids` or `wimids` class;
#'  \item Check the extent of balance of covariates in the datasets (using functions from the \pkg{cobalt} package);
#'  \item Fit the statistical model of interest on each dataset by the `with()` function, resulting in an object of the `mimira` class; and
#'  \item Pool the estimates from each model into a single set of estimates and standard errors, resulting in an object of the `mimipo` class.
#' }
#'
#' @details `pool()` function averages the estimates of the model and computes the total variance over the repeated analyses by Rubin’s rules. It calls [mice::pool()] after computing the model degrees of freedom.
#'
#' @return This function returns an object from the `mimipo` class. Methods for `mimipo` objects (e.g., `print()`, `summary()`, etc.) are imported from the \pkg{mice} package.
#'
#' @seealso [with()]
#' @seealso [mice::pool()]
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). `mice`: Multivariate Imputation by Chained Equations in `R`. *Journal of Statistical Software*, 45(3): 1-67. \doi{10.18637/jss.v045.i03}
#'
#' @export
#'
#' @examples \donttest{#Loading libraries
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice::mice(osteoarthritis, m = 5)
#'
#' #Weighting the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                                 imputed.datasets,
#'                                 approach = 'within',
#'                                 method = 'glm')
#'
#' #Analyzing the weighted datasets
#' models <- with(weighted.datasets,
#'                WeightIt::glm_weightit(KOA ~ OSP,
#'                                       family = binomial))
#'
#' #Pooling results obtained from analyzing the datasets
#' results <- pool(models)
#' summary(results)}

pool <- function (object, dfcom = NULL) {

  #External function
  #S3 method

  #Based on: mice::pool()
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
  #' @importFrom mice pool
  mice::pool
  #' @export

  #mids
  mice::pool(object = object, dfcom = dfcom)
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

  dfcom <- get.2dfcom(object, dfcom)

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
