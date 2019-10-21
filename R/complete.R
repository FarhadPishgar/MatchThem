# Extends mice::complete for wimids and mimids objects instead of using matchthem.data and weightthem.data.
# Does exactly the same thing but with added options (e.g., multiple datasets stacked, etc.)
# All arguments the same as mice::complete except for keepzeroweight. When FALSE,
# obesrvations with zero weights are removed from the dataset. This is necessary for svyglm()
# to not throw a warning (which can be ignored anyway, but we don't want users seeing it).

complete <- function(data, action = 1L, include = FALSE, mild = FALSE, ...) {
  UseMethod("complete")
}
complete.wimids <- function(data, action = 1L, include = FALSE, mild = FALSE, keepzeroweight = TRUE, ...) {
  m <- as.integer(data$object$m)

  if (is.numeric(action)) {
    action <- as.integer(action)
    idx <- action[action >= 0L & action <= m]
    if (include && all(idx != 0L))
      idx <- c(0L, idx)
    shape <- ifelse(mild, "mild", "stacked")
  }
  else if (is.character(action)) {
    if (include)
      idx <- 0L:m
    else idx <- 1L:m
    shape <- match.arg(action, c("all", "long", "broad",
                                 "repeated", "stacked"))
    shape <- ifelse(shape == "all" || mild, "mild", shape)
  }
  else stop("'action' not recognized")
  mylist <- lapply(idx, function(j) {
    out <- data$datasets[[j + 1]]
    out <- out[!names(out) %in% c(".id", ".imp")]
    if (!keepzeroweight) out <- out[out$weights > 0, , drop = FALSE]
    out
  })

  if (shape == "stacked")
    return(do.call("rbind", mylist))
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
  names(cmp) <- paste(rep.int(names(mylist[[1]]), m), rep.int(idx,
                                                            rep.int(ncol(mylist[[1]]), length(idx))), sep = ".")
  if (shape == "broad")
    return(cmp)
  else return(cmp[, order(rep.int(seq_len(ncol(mylist[[1]])),
                                  length(idx)))])
}
complete.mimids <- complete.wimids
complete.default <- mice::complete