#' @title Checks for the `mimids` Class
#'
#' @rdname is.mimids
#'
#' @aliases is.mimids
#'
#' @param object This argument specifies the object that should be checked to see if it is of the `mimids` class or not.
#'
#' @description `is.mimids()` function checks whether class of objects is `mimids` or not.
#'
#' @details The class of objects is checked to be of the `mimids`.
#'
#' @return This function returns a logical value indicating whether `object` is of the `mimids` class.
#'
#' @seealso [matchthem()]
#' @seealso [`mimids`]
#' @seealso [inherits()]
#'
#' @author Farhad Pishgar
#'
#' @export
#'
#' @examples \donttest{#Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice::mice(osteoarthritis, m = 5)
#'
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                               imputed.datasets,
#'                               approach = 'within',
#'                               method = 'nearest')
#'
#' #Checking the 'matched.datasets' object
#' is.mimids(matched.datasets)}

is.mimids <- function(object) {

  #' @export

  inherits(object, "mimids")
}

#' @title Checks for the `wimids` Class
#'
#' @rdname is.wimids
#'
#' @aliases is.wimids
#'
#' @param object This argument specifies the object that should be checked to see if it is of the `wimids` class or not.
#'
#' @description `is.wimids()` function checks whether class of objects is `wimids` or not.
#'
#' @details The class of objects is checked to be of the `wimids`.
#'
#' @return This function returns a logical value indicating whether `object` is of the `wimids` class.
#'
#' @seealso [`weightthem()`][weightthem]
#' @seealso [`wimids()`][wimids]
#'
#' @author Farhad Pishgar
#'
#' @export
#'
#' @examples \donttest{#Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice::mice(osteoarthritis, m = 5)
#'
#' #Estimating weights of observations in the multiply imputed datasets
#' weighted.datasets <- weightthem(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                                 imputed.datasets,
#'                                 approach = 'within',
#'                                 method = 'glm',
#'                                 estimand = "ATT")
#'
#' #Checking the 'weighted.datasets' object
#' is.wimids(weighted.datasets)}

is.wimids <- function(object) {

  #' @export

  inherits(object, "wimids")
}

#' @title Checks for the `mimira` Class
#'
#' @rdname is.mimira
#'
#' @aliases is.mimira
#'
#' @param object This argument specifies the object that should be checked to see if it is of the `mimira` class or not.
#'
#' @description `is.mimira()` function checks whether class of objects is `mimira` or not.
#'
#' @details The class of objects is checked to be of the `mimira`.
#'
#' @return This function returns a logical value indicating whether `object` is of the `mimira` class.
#'
#' @seealso [`with()`][with]
#' @seealso [`mimira()`][mimira]
#'
#' @author Farhad Pishgar
#'
#' @export
#'
#' @examples \donttest{#Loading libraries
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
#'                                 method = 'glm',
#'                                 estimand = "ATT")
#'
#' #Analyzing the weighted datasets
#' models <- with(weighted.datasets,
#'                svyglm(KOA ~ OSP, family = binomial))
#'
#' #Checking the 'models' object
#' is.mimira(models)}

is.mimira <- function(object) {

  #' @export

  inherits(object, "mimira")
}

#' @title Checks for the `mimipo` Class
#'
#' @rdname is.mimipo
#'
#' @aliases is.mimipo
#'
#' @param object This argument specifies the object that should be checked to see if it is of the `mimipo` class or not.
#'
#' @description `is.mimipo()` function checks whether class of objects is `mimipo` or not.
#'
#' @details The class of objects is checked to be of the `mimipo`.
#'
#' @return This function returns a logical value indicating whether `object` is of the `mimipo` class.
#'
#' @seealso [`pool()`][pool]
#' @seealso [`mimipo()`][mimipo]
#'
#' @author Farhad Pishgar
#'
#' @export
#'
#' @examples \donttest{#Loading libraries
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
#'                                 method = 'glm',
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
#' is.mimipo(results)}

is.mimipo <- function(object) {

  #' @export

  inherits(object, "mimipo")
}
