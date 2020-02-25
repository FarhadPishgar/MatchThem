#' @title Evaluates an Expression in Matched or Weighted Imputed Datasets
#'
#' @name with
#'
#' @rdname with
#'
#' @aliases with with.mimids with.wimids
#'
#' @method with mimids
#'
#' @param data This argument specifies an object of the \code{mimids} or \code{wimids} class, typically produced by a previous call to the \code{matchthem()} or \code{weightthem()}.
#' @param expr This argument specifies an expression of the usual syntax of R formula (it also accepts expressions from \pkg{survey} package, like \code{svyglm()}, please note that you shouldn't include the \code{weights = weights} argument, see the package vignette for details).
#' @param ... Additional arguments to be passed to \code{expr}.
#'
#' @description \code{with()} function performs a statistical computation on the \code{n} imputed datasets of the \code{mimids} or \code{wimids} objects. The typical sequence of steps to do a matching or weighting procedure on the imputed datasets are:
#' \enumerate{
#'  \item Impute the missing values by the \code{mice()} function (from the \pkg{mice} package) or the \code{amelia()} function (from the \pkg{Amelia} package), resulting in a multiple imputed dataset (an object of the \code{mids} or \code{amelia} class);
#'  \item Match or weight imputed datasets using a matching or weighting model by the \code{matchthem()} or \code{weightthem()} function, resulting in an object of the \code{mimids} or \code{wimids} class;
#'  \item Check the extent of balance of covariates across the datasets;
#'  \item Fit the statistical model of interest on each dataset by the \code{with()} function, resulting in an object of the \code{mimira} class; and
#'  \item Pool the estimates from each model into a single set of estimates and standard errors, resulting in an object of the \code{mimipo} class.
#' }
#'
#' @details \code{with()} performs a computation on the imputed datasets.
#'
#' @return This function returns an object of the \code{mimira} class.
#'
#' @seealso \code{\link[=matchthem]{matchthem}}
#' @seealso \code{\link[=weightthem]{weightthem}}
#'
#' @author Farhad Pishgar
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'
#' @export
#'
#' @examples \donttest{#Loading the dataset
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
#'                exp = svyglm(KOA ~ OSP, family = binomial))}

with.mimids <- function(data, expr, ...) {

  #S3 method

  #Based on: The mice:::with.mids()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Some

  #Importing functions
  #' @importFrom survey svydesign
  survey::svydesign
  #' @export

  #Polishing variables
  object <- data$object
  call <- match.call()

  #Do the repeated analysis, store the result
  if (substr(substitute(expr)[1], 1, 3) != "svy") {
    con.expr <- substitute(expr)
    con.expr$weights <- quote(weights)
    analyses <- lapply(seq_len(object$m), function(i) {
      data.i <- complete.mimids(data, i, all = FALSE)
      out <- eval(expr = con.expr, envir = data.i, enclos = parent.frame())
      if (is.expression(out)){
        out <- eval(expr = out, envir = data.i, enclos = parent.frame())
      }
      out
    })
  } else {
    svy.expr <- substitute(expr)
    svy.expr$design <- quote(design.i)
    if (!is.null(svy.expr$weights)) warning("Including weights (estimated by the 'matchthem()' function) in the expr is unnecessary and may result in biased estimates.")
    analyses <- lapply(seq_len(object$m), function(i) {
      data.i <- complete.mimids(data, i, all = FALSE)
      design.i <- survey::svydesign(~ 1, weights = ~ weights, data = data.i)
      out <- eval(expr = svy.expr)
      if (is.expression(out)){
        out <- eval(expr = out)
      }
      out
    })
  }

  #Return the complete data analyses as a list of length nimp
  output <- list(call = call, called = data$call, nmis = data$others$source$nmis, analyses = analyses)

  #Return the output
  oldClass(output) <- c("mimira", "matrix")
  return(output)
}

#' @rdname with
#'
#' @method with wimids
#'
#' @export

with.wimids <- function(data, expr, ...) {

  #S3 method

  #Based on: The mice:::with.mids()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Some

  #Importing functions
  #' @importFrom survey svydesign
  survey::svydesign
  #' @export

  #Polishing variables
  object <- data$object
  call <- match.call()

  #Do the repeated analysis, store the result
  if (substr(substitute(expr)[1], 1, 3) != "svy") {
    con.expr <- substitute(expr)
    con.expr$weights <- quote(weights)
    analyses <- lapply(seq_len(object$m), function(i) {
      data.i <- complete.wimids(data, i, all = FALSE)
      out <- eval(expr = con.expr, envir = data.i, enclos = parent.frame())
      if (is.expression(out)){
        out <- eval(expr = out, envir = data.i, enclos = parent.frame())
      }
      out
    })
  } else {
    svy.expr <- substitute(expr)
    svy.expr$design <- quote(design.i)
    if (!is.null(svy.expr$weights)) warning("Including weights (estimated by the 'weightthem()' function) in the expr is unnecessary and may result in biased estimates.")
    analyses <- lapply(seq_len(object$m), function(i) {
      data.i <- complete.wimids(data, i, all = FALSE)
      design.i <- survey::svydesign(~ 1, weights = ~ weights, data = data.i)
      out <- eval(expr = svy.expr)
      if (is.expression(out)){
        out <- eval(expr = out)
      }
      out
    })
  }

  #Return the complete data analyses as a list of length nimp
  output <- list(call = call, called = data$call, nmis = data$others$source$nmis, analyses = analyses)

  #Return the output
  oldClass(output) <- c("mimira", "matrix")
  return(output)
}
