#' @title Merges Imputed Datasets with Dataframes
#'
#' @rdname mergethem
#'
#' @aliases mergethem
#'
#' @param datasets This argument specifies an object of the \code{mimids} or \code{wimids} class.
#' @param data This argument specifies a dataframe.
#' @param by This argument specifies a variable name, present in both \code{datasets} and \code{data}.
#'
#' @description The \code{mergethem()} function merges a dataframe with each imputed dataset of the \code{mimids} or \code{wimids} class objects based on the variables passed to the function as \code{by}.
#'
#' @details This functions can be used similar to the \code{cbind()} function (from the \pkg{mice} package).
#'
#' @return This function returns an object of the \code{mimids} or \code{wimids} class after merging a dataframe with each imputed dataset of the inputted object.
#'
#' @seealso \code{\link[=matchthem]{matchthem}}
#' @seealso \code{\link[=weightthem]{weightthem}}
#' @seealso \code{\link[=bindthem]{bindthem}}
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
#' #Merging a (unsorted) dataframe with imputed datasets of the 'matched.datasets'
#' #(assume that 'data' is a dataset with information on new variables)
#' matched.datasets <- mergethem(matched.datasets, data, by = c("AGE", "SEX"))
#' }

mergethem <- function(datasets, data, by = "ID") {

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
  if(!is.data.frame(data)) {stop("The input for the data must be a data frame.")}

  if (mice::is.mids(datasets)) {
    #Polishing variables
    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- merge(data.0, data, by = by, all.x = TRUE, all.y = FALSE)

    #Preparing the list
    datasetslist <- list(data.0)

    #Merging
    for (i in 1:datasets$m) {
      data.i <- mice::complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, data, by = by, all.x = TRUE, all.y = FALSE)
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
    data.0 <- merge(data.0, data, by = by, all.x = TRUE, all.y = FALSE)

    #Preparing the list
    datasetslist <- list(data.0)

    #Merging
    for (i in 1:datasets$m) {
      data.i <- mice::complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, data, by = by, all.x = TRUE, all.y = FALSE)
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
    data.0 <- merge(data.0, data, by = by, all.x = TRUE, all.y = FALSE)

    #Preparing the list
    datasetslist <- list(data.0)

    #Binding
    for (i in 1:datasets$m) {
      data.i <- mice::complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, data, by = by, all.x = TRUE, all.y = FALSE)
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
