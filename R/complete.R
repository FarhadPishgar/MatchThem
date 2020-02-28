#' @title Extracts Imputed Datasets
#'
#' @name complete
#'
#' @rdname complete
#'
#' @aliases complete complete.mimids complete.wimids
#'
#' @param data This argument specifies an object of the \code{mimids} or \code{wimids} class.
#' @param n This argument specifies the imputed dataset number, intended to extract its data, or an action. The input must be a positive integer or a keyword. The keywords include \code{"all"} (produces a \code{mild} object of the imputed datasets), \code{"long"} (produces a dataset with imputed datasets stacked vertically), and \code{"broad"} (produces a dataset with imputed datasets stacked horizontally). The default is \code{1}.
#' @param include This argument specifies whether the original data with the missing values should be included. The input must be a logical value. The default is \code{FALSE}.
#' @param mild This argument specifies whether the return value should be an object of \code{mild} class. Please note that setting \code{mild = TRUE} overrides \code{n} keywords \code{"long"}, \code{"broad"}, and \code{"repeated"}. The default is \code{FALSE}.
#' @param all This argument specifies whether to include observations with a zero estimated weight. The default is \code{TRUE}.
#' @param ... Additional arguments to be passed to the function.
#'
#' @description \code{complete()} function extracts data from an object of the \code{mimids} or \code{wimids} class.
#'
#' @details The datasets within the \code{mimids} or \code{wimids} class objects are extracted.
#'
#' @return This function returns the imputed dataset within \code{mimids} or \code{wimids} class objects.
#'
#' @seealso \code{\link[=mimids]{mimids}}
#' @seealso \code{\link[=wimids]{wimids}}
#'
#' @author Extracted from the \pkg{mice} package written by Stef van Buuren et al. with changes
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'
#' @importFrom mice complete
#'
#' @export complete
#'
#' @examples \donttest{#Loading the dataset
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
#' #Extracting the first imputed dataset
#' matched.dataset.1 <- complete(matched.datasets, n = 1)}

#' @method complete mimids
#'
#' @export

complete.mimids <- function(data, n = 1, include = FALSE, mild = FALSE, all = TRUE, ...) {

  #External function
  #S3 method

  #Based on: The mice::complete()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Some

  #' @export

  #Polishing variables
  object <- data
  action <- n
  m <- as.integer(data$object$m)

  #mimids and wimids
  if ((is.mimids(object)) || (is.wimids(object))) {
    #Shape
    if (is.numeric(action)) {
      action <- as.integer(action)
      idx <- action[action >= 0L & action <= m]
      if (include && all(idx != 0L))
        idx <- c(0L, idx)
      shape <- ifelse(mild, "mild", "stacked")
    } else if (is.character(action)) {
      if (include)
        idx <- 0L:m
      else idx <- 1L:m
      shape <- match.arg(action, c("all", "long", "broad", "repeated", "stacked"))
      shape <- ifelse(shape == "all" || mild, "mild", shape)
    } else {
      stop("The input for the n argument is invalid.")
    }

    #Do it
    mylist <- lapply(idx, function(j) {
      out <- data$datasets[[j + 1]]
      out <- out[!names(out) %in% c(".id", ".imp")]
      if (!all) out <- out[out$weights > 0, , drop = FALSE]
      out
    })

    #Return the output
    if (shape == "stacked") {
      return(do.call("rbind", mylist))
    }

    if (shape == "mild") {
      names(mylist) <- as.character(idx)
      class(mylist) <- c("mild", "list")
      return(mylist)
    }

    if (shape == "long") {
      cmp <- do.call("rbind", mylist)
      cmp <- data.frame(.imp = rep(idx, each = nrow(mylist[[1]])),
                        .id = rep.int(1L:nrow(mylist[[1]]), length(idx)), cmp)
      if (is.integer(attr(mylist[[1]], "row.names")))
        row.names(cmp) <- seq_len(nrow(cmp))
      else row.names(cmp) <- as.character(seq_len(nrow(cmp)))
      return(cmp)
    }

    cmp <- do.call("cbind", mylist)
    names(cmp) <- paste(rep.int(names(mylist[[1]]), m), rep.int(idx, rep.int(ncol(mylist[[1]]), length(idx))), sep = ".")
    if (shape == "broad")
      return(cmp)
    else {
      return(cmp[, order(rep.int(seq_len(ncol(mylist[[1]])), length(idx)))])
    }
  }
}

#' @rdname complete
#'
#' @method complete wimids
#'
#' @export

complete.wimids <- function(data, n = 1, include = FALSE, mild = FALSE, all = TRUE, ...) {

  #External function
  #S3 method

  #Based on: The mice::complete()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Some

  #' @export

  #Polishing variables
  object <- data
  action <- n
  m <- as.integer(data$object$m)

  #mimids and wimids
  if ((is.mimids(object)) || (is.wimids(object))) {
    #Shape
    if (is.numeric(action)) {
      action <- as.integer(action)
      idx <- action[action >= 0L & action <= m]
      if (include && all(idx != 0L))
        idx <- c(0L, idx)
      shape <- ifelse(mild, "mild", "stacked")
    } else if (is.character(action)) {
      if (include)
        idx <- 0L:m
      else idx <- 1L:m
      shape <- match.arg(action, c("all", "long", "broad", "repeated", "stacked"))
      shape <- ifelse(shape == "all" || mild, "mild", shape)
    } else {
      stop("The input for the n argument is invalid.")
    }

    #Do it
    mylist <- lapply(idx, function(j) {
      out <- data$datasets[[j + 1]]
      out <- out[!names(out) %in% c(".id", ".imp")]
      if (!all) out <- out[out$weights > 0, , drop = FALSE]
      out
    })

    #Return the output
    if (shape == "stacked") {
      return(do.call("rbind", mylist))
    }

    if (shape == "mild") {
      names(mylist) <- as.character(idx)
      class(mylist) <- c("mild", "list")
      return(mylist)
    }

    if (shape == "long") {
      cmp <- do.call("rbind", mylist)
      cmp <- data.frame(.imp = rep(idx, each = nrow(mylist[[1]])),
                        .id = rep.int(1L:nrow(mylist[[1]]), length(idx)), cmp)
      if (is.integer(attr(mylist[[1]], "row.names")))
        row.names(cmp) <- seq_len(nrow(cmp))
      else row.names(cmp) <- as.character(seq_len(nrow(cmp)))
      return(cmp)
    }

    cmp <- do.call("cbind", mylist)
    names(cmp) <- paste(rep.int(names(mylist[[1]]), m), rep.int(idx, rep.int(ncol(mylist[[1]]), length(idx))), sep = ".")
    if (shape == "broad")
      return(cmp)
    else {
      return(cmp[, order(rep.int(seq_len(ncol(mylist[[1]])), length(idx)))])
    }
  }
}
