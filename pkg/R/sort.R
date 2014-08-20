#' Sort data.frames easily with tie-braker columns
#' 
#' @param x a data.frame
#' @param decreasing Sort in- or decreasing?
#' @param by A vector of column indices into x. Sortng is on the first index with subsequent columns as tie-brakers.
#' @param ... currently unimplemented
#' @export
sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}

