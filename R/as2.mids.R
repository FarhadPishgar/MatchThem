as2.mids <- function(long, where = NULL, .imp = ".imp", .id = ".id") {

  #Internal function
  #S3 method

  #Based on: The mice::as.mids()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Few

  #Importing functions
  #' @importFrom mice mice
  #' @importFrom stats na.omit
  mice::mice
  stats::na.omit

  if (is.numeric(.imp)) .imp <- names(long)[.imp]
  if (is.numeric(.id)) .id <- names(long)[.id]
  if (!.imp %in% names(long)) stop("Imputation index `.imp` not found")

  # no missings allowed in .imp
  imps <- unlist(long[, .imp], use.names = FALSE)
  if (anyNA(imps)) stop("Missing values in imputation index `.imp`")

  # number of records within .imp should be the same
  if (any(diff(table(imps))) != 0)
    stop("Unequal group sizes in imputation index `.imp`")

  # get original data part
  keep <- setdiff(names(long), stats::na.omit(c(.imp, .id)))
  data <- long[imps == 0, keep, drop = FALSE]
  n <- nrow(data)
  if (n == 0)
    stop("Original data not found.\n Use `complete(..., action = 'long', include = TRUE)` to save original data.")

  # determine m
  m <- length(unique(imps)) - 1

  # use mice to get info on data
  if (is.null(where)) where <- is.na(matrix(nrow = n, ncol = length(keep)))
  colnames(where) <- keep

  ini <- mice::mice(data, m = m, where = where, maxit = 0,
                    remove.collinear = FALSE, allow.na = TRUE)

  # store any .id as row names
  if (!is.na(.id))
    rownames(ini$data) <- unlist(long[imps == 0, .id], use.names = FALSE)

  # copy imputations from long into proper ini$imp elements
  names  <- names(ini$imp)
  for (i in seq_along(names)) {
    varname <- names[i]
    if(!is.null(ini$imp[[varname]])) {
      for(j in seq_len(m)) {
        idx <- imps == j & where[, varname]
        ini$imp[[varname]][j] <- long[idx, varname]
      }
    }
  }
  return(ini)
}
