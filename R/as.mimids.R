#' @title Create a `mimids` object
#'
#' @description Creates a `mimids` object from a list of `matchit` objects and an imputed dataset.
#'
#' @inheritParams matchthem
#' @param x A list of `matchit` objects, each the output of a call to [MatchIt::matchit()] on an imputed dataset.
#' @param ... Ignored.
#'
#' @return A [`mimids`] object.
#'
#' @details
#' The matched datasets are stored as though `matchthem()` was called with `approach = "within"`.
#'
#' @seealso [matchthem()], [`mimids`], [MatchIt::matchit()]
#'
#' @examples
#'
#' #Loading the dataset
#' data(osteoarthritis)
#'
#' #Multiply imputing the missing values
#' imputed.datasets <- mice::mice(osteoarthritis, m = 5,
#'                                printFlag = FALSE)
#'
#' #Matching the multiply imputed datasets manually
#' match.list <- lapply(1:5, function(i) {
#'   MatchIt::matchit(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                    mice::complete(imputed.datasets, i),
#'                    method = 'nearest')
#' })
#'
#' #Creating mimids object
#' matched.datasets <- as.mimids(match.list,
#'                               imputed.datasets)

#' @export
as.mimids <- function(x, ...) {
  UseMethod("as.mimids")
}

#' @exportS3Method as.mimids default
#' @rdname as.mimids
as.mimids.default <- function(x, datasets, ...) {
  #check x
  if (missing(x) || !is.list(x) ||
      !all(vapply(x, inherits, logical(1L), "matchit"))) {
    stop("`x` must be a list of 'matchit' objects, the output of calls to MatchIt::matchit().")
  }

  #Checking inputs format
  if(missing(datasets) || length(datasets) == 0) {stop("The input for the datasets must be specified.")}
  if(!inherits(datasets, "mids")  && !inherits(datasets, "amelia")) {stop("The input for the datasets must be an object of the 'mids' or 'amelia' class.")}

  #Compatibility with amelia objects
  if (inherits(datasets, "amelia")) {
    imp0 <- datasets$imputations[[1]]
    is.na(imp0) <- datasets$missMatrix
    imp0$.id <- 1:nrow(imp0)
    imp0$.imp <- 0

    implist <- vector("list", datasets$m + 1)
    implist[[1]] <- imp0

    for (i in 1:datasets$m) {
      imp <- datasets$imputations[[i]]
      imp$.id <- 1:nrow(imp0)
      imp$.imp <- i
      implist[[i+1]] <- imp
    }

    imp.datasets <- do.call(base::rbind, as.list(noquote(implist)))
    datasets <- mice::as.mids(imp.datasets)
  }

  if(!is.null(datasets$data$distance)) {stop("The input for the datasets shouldn't have a variable named 'distance'.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(!is.null(datasets$data$subclass)) {stop("The input for the datasets shouldn't have a variable named 'subclass'.")}
  if(!is.null(datasets$data$discarded)) {stop("The input for the datasets shouldn't have a variable named 'discarded'.")}

  if (length(x) != datasets$m) {
    stop("The length of `x` should be equal to the number of imputations")
  }

  for (i in seq_along(x)) {
    if (length(x[[i]]$treat) != length(x[[1]]$treat) ||
        length(x[[i]]$treat) != nrow(mice::complete(datasets, i))) {
      stop("The matchit objects in `x` do not appear to have been fit to the imputed datasets supplied to `datasets`")
    }
  }

  .remove_data_from_call <- function(z) {
    if (!is.null(z$call) && !is.null(z$call$data)) z$call$data <- NULL
    z
  }

  if (!all(vapply(x, function(m) {
    identical(.remove_data_from_call(m)$call,
              .remove_data_from_call(x[[1]])$call)
  }, logical(1L)))) {
    warning("Different matching specifications were used for each imputed dataset; this may not yield valid results when pooling estimates")
    called <- NULL
  }
  else {
    m.called <- match.call()
    called <- x[[1]]$call
    called[[1]] <- quote(matchthem)
    called[["data"]] <- m.called[["datasets"]]
    names(called)[names(called) == "data"] <- "datasets"
    called[["approach"]] <- "within"
  }

  output <- list(call = called,
                 object = datasets,
                 models = x,
                 approach = "within")
  class(output) <- "mimids"
  return(output)
}