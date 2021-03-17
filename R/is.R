#' @title Checks for the \code{mimids} Class
#'
#' @rdname is.mimids
#'
#' @aliases is.mimids
#'
#' @param object This argument specifies the object that should be checked to see if is of the \code{mimids} class or not.
#'
#' @description \code{is.mimids()} function checks whether class of objects is \code{mimids} or not.
#'
#' @details The class of objects is checked to be of the \code{mimids}.
#'
#' @return This function returns a logical value indicating whether \code{object} is of the \code{mimids} class.
#'
#' @seealso \code{\link[=matchthem]{matchthem}}
#' @seealso \code{\link[=mimids]{mimids}}
#'
#' @author Farhad Pishgar
#'
#' @export
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
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                               approach = 'within', method = 'nearest')
#'
#' #Checking the 'matched.datasets' object
#' is.mimids(matched.datasets)
#' is(matched.datasets)}

is.mimids <- function(object) {

  #' @export

  output <- inherits(object, "mimids")
  return(output)
}

#' @title Checks for the \code{wimids} Class
#'
#' @rdname is.wimids
#'
#' @aliases is.wimids
#'
#' @param object This argument specifies the object that should be checked to see if is of the \code{wimids} class or not.
#'
#' @description \code{is.wimids()} function checks whether class of objects is \code{wimids} or not.
#'
#' @details The class of objects is checked to be of the \code{wimids}.
#'
#' @return This function returns a logical value indicating whether \code{object} is of the \code{wimids} class.
#'
#' @seealso \code{\link[=weightthem]{weightthem}}
#' @seealso \code{\link[=wimids]{wimids}}
#'
#' @author Farhad Pishgar
#'
#' @export
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
#'                                 estimand = "ATT")
#'
#' #Checking the 'weighted.datasets' object
#' is.wimids(weighted.datasets)
#' is(weighted.datasets)}

is.wimids <- function(object) {

  #' @export

  output <- inherits(object, "wimids")
  return(output)
}

#' @title Checks for the \code{mimira} Class
#'
#' @rdname is.mimira
#'
#' @aliases is.mimira
#'
#' @param object This argument specifies the object that should be checked to see if is of the \code{mimira} class or not.
#'
#' @description \code{is.mimira()} function checks whether class of objects is \code{mimira} or not.
#'
#' @details The class of objects is checked to be of the \code{mimira}.
#'
#' @return This function returns a logical value indicating whether \code{object} is of the \code{mimira} class.
#'
#' @seealso \code{\link[=with]{with}}
#' @seealso \code{\link[=mimira]{mimira}}
#'
#' @author Farhad Pishgar
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
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                                 imputed.datasets,
#'                                 approach = 'within',
#'                                 method = 'ps',
#'                                 estimand = "ATT")
#'
#' #Analyzing the weighted datasets
#' models <- with(data = weighted.datasets,
#'                exp = svyglm(KOA ~ OSP, family = binomial))
#'
#' #Checking the 'models' object
#' is.mimira(models)
#' is(models)}

is.mimira <- function(object) {

  #' @export

  output <- inherits(object, "mimira")
  return(output)
}

#' @title Checks for the \code{mimipo} Class
#'
#' @rdname is.mimipo
#'
#' @aliases is.mimipo
#'
#' @param object This argument specifies the object that should be checked to see if is of the \code{mimipo} class or not.
#'
#' @description \code{is.mimipo()} function checks whether class of objects is \code{mimipo} or not.
#'
#' @details The class of objects is checked to be of the \code{mimipo}.
#'
#' @return This function returns a logical value indicating whether \code{object} is of the \code{mimipo} class.
#'
#' @seealso \code{\link[=pool]{pool}}
#' @seealso \code{\link[=mimipo]{mimipo}}
#'
#' @author Farhad Pishgar
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
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                                 imputed.datasets,
#'                                 approach = 'within',
#'                                 method = 'ps',
#'                                 estimand = "ATT")
#'
#' #Analyzing the weighted datasets
#' models <- with(data = weighted.datasets,
#'                exp = svyglm(KOA ~ OSP, family = binomial))
#'
#' #Pooling results obtained from analysing the datasets
#' results <- pool(models)
#'
#' #Checking the 'results' object
#' is.mimipo(results)
#' is(results)}

is.mimipo <- function(object) {

  #' @export

  output <- inherits(object, "mimipo")
  return(output)
}