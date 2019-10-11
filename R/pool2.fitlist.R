pool2.fitlist <- function (fitlist, dfcom = NULL) {

  #Internal function
  #S3 method

  #Based on: The mice:::pool.fitlist()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Few

  #Importing functions
  #' @importFrom dplyr mutate
  #' @importFrom dplyr group_by
  #' @importFrom dplyr summarize
  #' @importFrom dplyr select
  #' @importFrom dplyr %>%
  #' @importFrom mice getfit
  #' @importFrom stats sd
  #' @importFrom stats var
  dplyr::mutate
  dplyr::group_by
  dplyr::summarize
  dplyr::select
  dplyr::`%>%`
  mice::getfit
  stats::sd
  stats::var

  #Preparing the summary
  w <- summary(fitlist, type = "tidy", exponentiate = FALSE)

  #Combine y.level and term into term (for multinom)
  if ("y.level" %in% names(w)) w$term <- paste(w$y.level, w$term, sep = ":")

  #Address the problem with checking in an unusual way, just to keep the original codes of the mice package
  .data <- NULL
  b <- NULL
  df <- NULL
  m <- NULL
  param <- NULL
  riv <- NULL
  term <- NULL
  ubar <- NULL

  #Rubin's rules for scalar estimates
  output <- w %>%
    dplyr::mutate(param = rep_len(1L:length(unique(term)), length.out = dplyr::n())) %>%
    dplyr::group_by(param) %>%
    dplyr::summarize(m = dplyr::n(),
                     term = .data$term[1L],
                     qbar = mean(.data$estimate),
                     ubar = mean(.data$std.error ^ 2),
                     b = stats::var(.data$estimate),
                     t = ubar + (1 + 1 / m) * b,
                     dfcom = dfcom,
                     df = barnard2.rubin(m, b, t, dfcom),
                     riv = (1 + 1 / m) * b / ubar,
                     lambda = (1 + 1 / m) * b / t,
                     fmi = (riv + 2 / (df + 3)) / (riv + 1)) %>%
    dplyr::select(-m, -param)
  output <- data.frame(output[, -1L],
                       row.names = output$term)
  names(output)[1L] <- "estimate"

  #Return the output
  return(output)
}
