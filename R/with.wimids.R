#' @title Evaluates an Expression in Weighted Imputed Datasets
#'
#' @rdname with.wimids
#'
#' @method with wimids
#'
#' @param data This argument specifies an object of the \code{wimids} class, typically produced by a previous call to the function \code{weightthem()}.
#' @param expr This argument specifies an expression of the usual syntax of R formula. See \code{help(formula)} for details.
#' @param method This argument specifies the method that should be used for calculating standard errors of the estimates. Currently, the \code{'conventional'} (default) and \code{'weighted'} (using functions from the \pkg{survey} package to build the \code{glm} and \code{coxph} models).
#' @param ... Additional arguments to be passed to \code{expr}.
#'
#' @description The \code{with()} function performs a statistical computation on the \code{n} imputed datasets of the \code{wimids} object. The typical sequence of steps to do a weighting procedure on the imputed datasets are:
#' \enumerate{
#'  \item Impute the missing values by the \code{mice()} function (from the \pkg{mice} package) or the \code{amelia()} function (from the \pkg{Amelia} package), resulting in a multiple imputed dataset (an object of the \code{mids} or \code{amelia} class);
#'  \item Estimate weights of observation in each imputed dataset using a weighting model by the \code{weightthem()} function, resulting in an object of the \code{wimids} class;
#'  \item Fit the statistical model of interest on each weighted dataset by the \code{with()} function, resulting in an object of the \code{mira} class;
#'  \item Pool the estimates from each model into a single set of estimates and standard errors, resulting in an object of the \code{mipo} class.
#' }
#'
#' @details The \code{with()} performs a computation on the imputed datasets.
#'
#' @return This function returns an object of the \code{mira} class (multiply imputed repeated analyses).
#'
#' @seealso \code{\link[=weightthem]{weightthem}}
#'
#' @author Extracted from the \pkg{mice} package written by Stef van Buuren et al. with changes
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
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
#'                          method = c("", "", "mean", "polyreg",
#'                                     "logreg", "logreg", "logreg"))
#'
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                                 approach = 'within', method = 'ps')
#'
#' #Analyzing the weighted datasets
#' models <- with(data = weighted.datasets,
#'                exp = glm(KOA ~ OSP, weights = weights, family = binomial),
#'                method = 'weighted')
#' }

with.wimids <- function(data, expr, method = "conventional", ...) {

  #S3 method

  #Based on: The mice:::with.mids()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Some

  #Importing functions
  #' @importFrom survey svyglm
  survey::svyglm
  #' @export

  #Polishing variables
  object <- data$object
  call <- match.call()
  analyses <- as.list(seq_len(object$m))

  #Do the repeated analysis, store the result
  for (i in seq_along(analyses)){
    data.i <- matchthem.data(data, i)
    analyses[[i]] <- eval(expr = substitute(expr), envir = data.i, enclos = parent.frame())
    if (is.expression(analyses[[i]])){
      analyses[[i]] <- eval(expr = analyses[[i]], envir = data.i, enclos = parent.frame())
    }
  }

  #Weighted
  if (method == "weighted") {
    weighted.formula <- analyses[[1]]$formula
    if (!is.null(analyses[[1]]$family$family)) weighted.family <- noquote(analyses[[1]]$family$family)

    #Estimate the weighted standard errors
    for (i in seq_along(analyses)) {
      design.i <- data$others$survey.objects.[[i+1]]

      #Weighted models
      if (analyses[[i]]$call[1] == "glm()") analyses[[i]] <- survey::svyglm(weighted.formula, design.i, family = weighted.family)
      if (analyses[[i]]$call[1] == "coxph()") analyses[[i]] <- survey::svycoxph(weighted.formula, design.i)
    }
  }

  #Return the complete data analyses as a list of length nimp
  output <- list(call = call, call1 = object$call, nmis = object$nmis, analyses = analyses)

  #Return the output
  oldClass(output) <- c("mira", "matrix")
  return(output)
}
