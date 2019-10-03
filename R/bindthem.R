#' @title Binds Imputed Datasets and Dataframes
#'
#' @rdname bindthem
#'
#' @aliases bindthem
#'
#' @param datasets This argument specifies an object of the \code{mimids} or \code{wimids} class.
#' @param data This argument specifies a dataframe.
#'
#' @description The \code{bindthem()} function binds a dataframe to each imputed dataset of the \code{mimids} or \code{wimids} class objects in a row-wise fashion.
#'
#' @details This functions can be used similar to the \code{cbind()} function (from the \pkg{mice} package).
#'
#' @return This function returns an object of the \code{mimids} or \code{wimids} class after binding a dataframe to each imputed dataset of the inputted object.
#'
#' @seealso  \code{\link[=matchthem]{matchthem}}
#' @seealso  \code{\link[=weightthem]{weightthem}}
#' @seealso  \code{\link[=mergethem]{mergethem}}
#'
#' @author Farhad Pishgar
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
#'                          method = c("", "", "mean", "polyreg", "logreg", "logreg", "logreg"))
#'
#' #Matching the multiply imputed datasets
#' matched.datasets <- matchthem(OSP ~ AGE + SEX + BMI + RAC + SMK, imputed.datasets,
#'                               approach = 'within', method = 'nearest')
#'
#' #Binding a (sorted) dataframe to imputed datasets of the 'matched.datasets'
#' #(assume that 'data' is a dataset with information on new variables)
#' matched.datasets <- bindthem(matched.datasets, data)
#' }

bindthem <- function(datasets, data) {

  #External function

  #Importing functions
  #' @importFrom mice is.mids complete
  mice::is.mids
  mice::complete
  #' @export

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(is.null(data)) {stop("The input for the data must be specified.")}
  if(!mice::is.mids(datasets) & !is.mimids(datasets) & !is.wimids(datasets)) {stop("The input for the datasets must be an object of the 'mids', 'mimids', or 'wimids' class.")}
  if(!is.data.frame(data)) {stop("The input for the data must be a dataframe.")}

  if (mice::is.mids(datasets)) {
    #Polishing variables
    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- cbind(data.0, data)

    #Preparing the list
    datasetslist <- list(data.0)

    #Binding
    for (i in 1:datasets$m) {
      data.i <- mice::complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- cbind(data.i, data)
      datasetslist[[i+1]] <- data.i
    }

    #Returning output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    new.datasets <- as2.mids(new.datasets)
    return(new.datasets)
  }

  if (is.mimids(datasets)) {
    #Polishing variables
    modelslist <- datasets$models
    others <- datasets$others
    datasets <- datasets$object
    original <- datasets$original.object

    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- cbind(data.0, data)

    #Preparing the list
    datasetslist <- list(data.0)

    #Binding
    for (i in 1:datasets$m) {
      data.i <- mice::complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- cbind(data.i, data)
      datasetslist[[i+1]] <- data.i
    }

    #Prepating the output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    new.datasets <- as2.mids(new.datasets)

    #Returning output
    output <- list(object = new.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.object = original)
    class(output) <- "mimids"
    return(output)
  }

  if (is.wimids(datasets)) {
    #Polishing variables
    modelslist <- datasets$models
    others <- datasets$others
    datasets <- datasets$object
    original <- datasets$original.object

    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- cbind(data.0, data)

    #Preparing the list
    datasetslist <- list(data.0)

    #Binding
    for (i in 1:datasets$m) {
      data.i <- mice::complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- cbind(data.i, data)
      datasetslist[[i+1]] <- data.i
    }

    #Prepating the output
    new.datasets <- do.call("rbind", as.list(noquote(datasetslist)))
    new.datasets <- as2.mids(new.datasets)

    #Returning output
    output <- list(object = new.datasets,
                   models = modelslist,
                   others = others,
                   datasets = datasetslist,
                   original.object = original)
    class(output) <- "wimids"
    return(output)
  }
}
