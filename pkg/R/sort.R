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


#' Pseudolog
#'
#' @param x numeric.
#' @param base positive number.
#' 
#' @section Details:
#' The pseudolog function is defined as \eqn{asinh(0.5\cdot x)/log(base)}. It approaches \eqn{log(x)} for
#' large positive \eqn{x} and \eqn{-log(|x|)} for large negative \eqn{x}. Around zero it behaves linearly.
#' 
#' @export 
plog <- function(x,base=exp(1)){
  stopifnot(base > 0)
  asinh(0.5*x)/log(base)
}


