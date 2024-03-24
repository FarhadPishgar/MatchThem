#' @title Extracts Multiply Imputed Datasets
#'
#' @name complete
#'
#' @aliases complete complete.mimids complete.wimids
#'
#' @param data A [`mimids`] or [`wimids`] object; the output of a call to [matchthem()] or [weightthem()].
#' @param action The imputed dataset number, intended to extract its data, or an action. The input must be a positive integer or a keyword. The keywords include `"all"` (produces a `mild` object of the multiply imputed datasets), `"long"` (produces a dataset with multiply imputed datasets stacked vertically), and `"broad"` (produces a dataset with multiply imputed datasets stacked horizontally). The default is `1`.
#' @param include Whether the original data with the missing values should be included. The input must be a logical value. The default is `FALSE`.
#' @param mild Whether the return value should be an object of `mild` class. Please note that setting `mild = TRUE` overrides `action` keywords of `"long"`, `"broad"`, and `"repeated"`. The default is `FALSE`.
#' @param all Whether to include observations with a zero estimated weight. The default is `TRUE`.
#' @param ... Ignored.
#'
#' @description `complete()` extracts data from an object of the `mimids` or `wimids` class.
#'
#' @details `complete()` works by running [mice::complete()] on the `mids` object stored within the `mimids` or `wimids` object and appending the outputs of the matching or weighting procedure. For `mimids` objects, the appended outputs include the matching weights, the propensity score (if included), pair membership (if included), and whether each unit was discarded. For `wimids` objects, the appended output is the estimated weights.
#'
#' @return This function returns the imputed dataset within the supplied `mimids` or `wimids` objects.
#'
#' @seealso [`mimids`]
#' @seealso [`wimids`]
#' @seealso [mice::complete()]
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). `mice`: Multivariate Imputation by Chained Equations in `R`. *Journal of Statistical Software*, 45(3): 1-67. \doi{10.18637/jss.v045.i03}
#'
#' @export complete
#'
#' @examples \donttest{
#' #Loading the dataset
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
#' #Extracting the first imputed dataset
#' matched.dataset.1 <- complete(matched.datasets, n = 1)}

#' @rdname complete
#'
#' @method complete mimids
#'
#' @export

complete.mimids <- function(data, action = 1, include = FALSE, mild = FALSE, all = TRUE, ...) {

  #External function
  #S3 method

  #Based on: The mice::complete()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Some

  #Importing functions
  #' @importFrom mice complete
  mice::complete
  #' @export

  #Polishing variables
  object <- data
  m <- as.integer(data$object$m)

  #mimids and wimids
  #Shape
  if (is.numeric(action)) {
    action <- as.integer(action)
    idx <- action[action >= 0L & action <= m]
    if (include && all(idx != 0L))
      idx <- c(0L, idx)
    shape <- if (mild) "mild" else "stacked"
  } else if (is.character(action)) {
    if (include)
      idx <- 0L:m
    else idx <- 1L:m
    shape <- match.arg(action, c("all", "long", "broad", "repeated", "stacked"))
    shape <- if (shape == "all" || mild) "mild" else shape
  } else {
    stop("The input for the action argument is invalid.")
  }

  #Select created variables from matchit/weightit models to add
  if (is.mimids(data)) {
    modelvars <- c("weights", "subclass", "distance", "discard")
  }
  else modelvars <- "weights"

  modelvars <- intersect(modelvars, names(object$models[[2]]))

  #Do it
  mylist <- lapply(idx, function(j) {
    out <- mice::complete(data$object, j)
    for (v in modelvars) {
      out[[v]] <- if (j == 0) NA_real_ else object$models[[j]][[v]]
    }
    out
  })

  #Return the output
  if (shape == "stacked") {
    cmp <- do.call("rbind", mylist)
    if (!all) cmp <- cmp[cmp$weights > 0, ,drop = FALSE]
    return(cmp)
  }

  if (shape == "mild") {
    if (!all) {
      for (i in seq_along(mylist)) {
        mylist[[i]] <- mylist[[i]][mylist[[i]]$weights > 0, ,drop = FALSE]
      }
    }
    names(mylist) <- as.character(idx)
    class(mylist) <- "mild"
    return(mylist)
  }

  if (shape == "long") {
    cmp <- do.call("rbind", lapply(idx, function(i) data.frame(.imp = i, .id = seq_len(nrow(mylist[[i]])),
                                                               mylist[[i]])))
    if (is.integer(attr(mylist[[1]], "row.names")))
      row.names(cmp) <- seq_len(nrow(cmp))
    else row.names(cmp) <- as.character(seq_len(nrow(cmp)))

    if (!all) cmp <- cmp[cmp$weights > 0, ,drop = FALSE]

    return(cmp)
  }

  if (!all) stop(paste0("'all' cannot be set to FALSE if action = '", shape, "'."))
  cmp <- do.call("cbind", mylist)
  names(cmp) <- paste(rep.int(names(mylist[[1]]), m), rep.int(idx, rep.int(ncol(mylist[[1]]), length(idx))), sep = ".")
  if (shape == "broad")
    return(cmp)
  else {
    return(cmp[, order(rep.int(seq_len(ncol(mylist[[1]])), length(idx)))])
  }
}

#' @rdname complete
#'
#' @method complete wimids
#'
#' @export

complete.wimids <- complete.mimids
