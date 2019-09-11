match2.data <- function(object, distance = "distance", weights = "weights",
                       environment = environment()) {

  #Internal function

  #Based on: The MatchIt::match.data()
  #URL: <https://cran.r-project.org/package=MatchIt>
  #URL: <https://github.com/kosukeimai/MatchIt>
  #URL: <https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf>
  #URL: <https://imai.fas.harvard.edu/research/files/matchit.pdf>
  #Authors: Daniel Ho et al.
  #Changes: Few

  #Polishing variables
  data <- eval(object$call$data, envir = environment)
  treat <- object$treat
  wt <- object$weights
  vars <- names(data)

  #Binding the distance variable
  if (distance %in% vars)
    stop("The input for the datasets shouldn't have a variable named as 'distance'.")
  else if (!is.null(object$distance)) {
    dta <- data.frame(cbind(data, object$distance))
    names(dta) <- c(names(data), distance)
    data <- dta
  }

  #Binding the weights variable
  if (weights %in% vars)
    stop("The input for the datasets shouldn't have a variable named as 'weights'.")
  else if (!is.null(object$weights)){
    dta <- data.frame(cbind(data, object$weights))
    names(dta) <- c(names(data), weights)
    data <- dta
  }

  #Returning output
  return(data[wt > 0,])
}
