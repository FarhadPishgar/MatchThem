#' @title Create a `wimids` object
#'
#' @description Creates a `wimids` object from a list of `weightit` objects and an imputed dataset.
#'
#' @inheritParams weightthem
#' @param x A list of `weightit` objects, each the output of a call to [WeightIt::weightit()] on an imputed dataset.
#' @param ... Ignored.
#'
#' @return A [`wimids`] object.
#'
#' @details
#' The weighted datasets are stored as though `weightthem()` was called with `approach = "within"`.
#'
#' @seealso [weightthem()], [`wimids`], [WeightIt::weightit()]
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
#' weight.list <- lapply(1:5, function(i) {
#'   WeightIt::weightit(OSP ~ AGE + SEX + BMI + RAC + SMK,
#'                      mice::complete(imputed.datasets, i),
#'                      method = 'glm',
#'                      estimand = 'ATT')
#' })
#'
#' #Creating wimids object
#' weighted.datasets <- as.wimids(weight.list,
#'                                imputed.datasets)

#' @export
as.wimids <- function(x, ...) {
  UseMethod("as.wimids")
}

#' @exportS3Method as.wimids default
#' @rdname as.wimids
as.wimids.default <- function(x, datasets, ...) {
  #check x
  if (missing(x) || !is.list(x) ||
      !all(vapply(x, inherits, logical(1L), "weightit"))) {
    stop("'x' must be a list of 'weightit' objects, the output of calls to WeightIt::watchit().")
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

  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}


  if (length(x) != datasets$m) {
    stop("The length of `x` should be equal to the number of imputations")
  }

  for (i in seq_along(x)) {
    if (length(x[[i]]$treat) != length(x[[1]]$treat) ||
        length(x[[i]]$treat) != nrow(mice::complete(datasets, i))) {
      stop("The weightit objects in `x` do not appear to have been fit to the imputed datasets supplied to `datasets`")
    }
  }

  .remove_data_from_call <- function(z) {
    if (!is.null(z$call) && !is.null(z$call$data)) z$call$data <- NULL
    z
  }

  if (!all(vapply(x, function(w) {
    identical(.remove_data_from_call(w)$call,
              .remove_data_from_call(x[[1]])$call)
  }, logical(1L)))) {
    warning("Different weighting specifications were used for each imputed dataset; this may not yield valid results when pooling estimates")
    called <- NULL
  }
  else {
    m.called <- match.call()
    called <- x[[1]]$call
    called[[1]] <- quote(weightthem)
    called[["data"]] <- m.called[["datasets"]]
    names(called)[names(called) == "data"] <- "datasets"
    called[["approach"]] <- "within"
  }

  output <- list(call = called,
                 object = datasets,
                 models = x,
                 approach = "within")
  class(output) <- "wimids"
  return(output)
}