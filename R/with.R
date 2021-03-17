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
#' @param data An \code{mimids} or \code{wimids} object, typically produced by a previous call to the \code{matchthem()} or \code{weightthem()}.
#' @param expr An expression (usually a call to a modeling function like \code{glm()}, \code{coxph()}, \code{svyglm()}, etc.) to evaluate for each imputed data set. See Details.
#' @param cluster When a function from \pkg{survey} (e.g., \code{\link[survey:svyglm]{svyglm()}}) is supplied in \code{expr}, whether the standard errors should incorporate clustering due to dependence between matched pairs. This is done by supplying the variable containing pair membership to the \code{ids} argument of \code{link[survey:svydesign]{svydesign()}}. If unspecified, it will be set to \code{TRUE} if subclasses (i.e., pairs) are present in the output and there are 20 or more unique subclasses. It will be ignored for matching methods that don't return subclasses (e.g., matching with replacement).
#' @param ... Additional arguments to be passed to \code{expr}.
#'
#' @description \code{with()} runs a model on the \code{n} imputed datasets of the supplied \code{mimids} or \code{wimids} object. The typical sequence of steps to do a matching procedure on the imputed datasets are:
#' \enumerate{
#'  \item Impute the missing values using the \code{mice()} function (from the \pkg{mice} package) or the \code{amelia()} function (from the \pkg{Amelia} package), resulting in a multiple imputed dataset (an object of the \code{mids} or \code{amelia} class);
#'  \item Match or weight each imputed dataset using \code{matchthem()} or \code{weightthem()}, resulting in an object of the \code{mimids} or \code{wimids} class;
#'  \item Check the extent of balance of covariates across the matched datasets (using functions in \pkg{cobalt});
#'  \item Fit the statistical model of interest on each matched dataset by the \code{with()} function, resulting in an object of the \code{mimira} class; and
#'  \item Pool the estimates from each model into a single set of estimates and standard errors, resulting in an object of the \code{mipo} class.
#' }
#'
#' @details \code{with()} applies the supplied model in \code{expr} to the matched or weighrd imputed datasets, automatically incorporating the (matching) weights when possible. The argument to \code{expr} should be of the form \code{glm(y ~ z, family = quasibinomial)}, for example, excluding the data or weights argument, which are automatically supplied. \cr
#' Functions from the \pkg{survey} package, such as \code{svyglm()}, are treated a bit differently. No \code{svydesign} objcect needs to be supplied because \code{with()} automatically constructs and supplies it with the imputed dataset and estimated weights. When \code{cluster = TRUE} (or \code{with()} detects that pairs should be clustered; see Arguments above), pair membership is supplied to the \code{ids} argument of \code{svydesign()}. \cr
#' For generalized linear models, it is always recommended to use \code{svyglm()} rather than \code{glm()} in order to correctly compute standard errors. For Cox models, \code{coxph()} will produce correct standard errors when used with weighting but \code{svycoxph()} will produce more accurate standard errors when matching is used.
#'
#' @return An object of the \code{mimira} class containing the output of the analyses.
#'
#' @seealso \code{\link[=matchthem]{matchthem()}}
#' @seealso \code{\link[=weightthem]{weightthem()}}
#' @seealso \code{\link[mice:with.mids]{mice::with.mids()}}
#'
#' @author Farhad Pishgar and Noah Greifer
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
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
#' #Matching in the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                                 imputed.datasets,
#'                                 approach = 'within',
#'                                 method = 'nearest')
#'
#' #Analyzing the matched datasets
#' models <- with(matched.datasets,
#'                svyglm(KOA ~ OSP, family = binomial),
#'                cluster = TRUE)}

with.mimids <- function(data, expr, cluster, ...) {

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
  #' @importFrom mice complete
  #' @importFrom MatchIt match.data
  survey::svydesign
  mice::complete
  MatchIt::match.data
  #' @export

  #Polishing variables
  object <- data$object
  call <- match.call()

  #Do the repeated analysis, store the result
  if (substr(substitute(expr)[1], 1, 3) != "svy") {
    con.expr <- substitute(expr)
    con.expr$weights <- quote(weights)
    if (deparse1(con.expr[[1]]) == "coeftest") con.expr$save <- TRUE
    if (!missing(cluster)) warning("The 'cluster' argument can only be used with functions from the survey package (e.g., svyglm()). Ignoring 'cluster'.")
    analyses <- lapply(seq_len(object$m), function(i) {

      data.i <- mice::complete(data$object, i)
      m.data.i <- MatchIt::match.data(data$models[[i]], data = data.i)

      out <- eval(expr = con.expr, envir = m.data.i, enclos = parent.frame())
      if (is.expression(out)){
        out <- eval(expr = out, envir = m.data.i, enclos = parent.frame())
      }
      out
    })
  } else {
    svy.expr <- substitute(expr)
    svy.expr$design <- quote(design.i)
    missing.cluster <- missing(cluster)
    if (!is.null(svy.expr$weights)) warning("Including weights (estimated by the 'matchthem()' function) in the expr is unnecessary and may result in biased estimates.")
    analyses <- lapply(seq_len(object$m), function(i) {

      data.i <- mice::complete(data$object, i)
      m.data.i <- MatchIt::match.data(data$models[[i]], data = data.i)

      if ((missing.cluster && !is.null(m.data.i$subclass) && nlevels(m.data.i$subclass) >= 20) ||
          (!missing.cluster && isTRUE(cluster) && !is.null(m.data.i$subclass))){
        design.i <- survey::svydesign(ids = ~subclass, weights = ~ weights, data = m.data.i)
      }
      else {
        design.i <- survey::svydesign(ids = ~1, weights = ~ weights, data = m.data.i)
      }

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
  class(output) <- c("mimira", "mira")
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
    if (deparse1(con.expr[[1]]) == "coeftest") con.expr$save <- TRUE
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
  class(output) <- c("mimira", "mira")
  return(output)
}
