

#' The identity function
#' @param x any R object
#' @return x
#' @export
Id <- function(x) x


#' Rescale a numeric vector.
#' 
#' @param x \code{numeric}
#' @param domain Limits of the input range. If none is given, the min and max of R are chosen.
#' @param range Limits of the output range.
#' @param transform \code{function} to apply to x and the domain of x prior to rescaling (for example: \code{sqrt}).
#' @param ... values to pass to other methods.
#'
#' @export
#'
setGeneric('rescale',def=function(x, domain, range, ...) standardGeneric('rescale'))

#' @rdname rescale
setMethod('rescale',signature('numeric','numeric','numeric'), function(x,domain=range(x), range, transform=Id){
  x <- transform(x)
  dx <- diff(transform(domain))
  dy <- diff(range)
  range[1] + (x-domain[1]) * dy/dx  
})

#' @rdname rescale
setMethod('rescale',signature('numeric','matrix','numeric'), function(x, domain, range, transform=Id){
  x <- transform(x)
  dx <- transform(domain[,2]) - transform(domain[,1])
  dy <- diff(range)
  range[1] + (x - domain[,1]) * dy/dx
})

#' @rdname rescale
setMethod('rescale',signature('numeric','matrix','matrix'), function(x, domain, range, transform=Id){
  x <- transform(x)  
  dx <- transform(domain[,2]) - transform(domain[,1])
  dy <- range[,2] - range[,1]
  range[,1] + (x - domain[,1]) * dy/dx
})

#' @rdname rescale
setMethod('rescale',signature('numeric','numeric','matrix'),function(x, domain=range(x), range, transform=Id){
  x <- transform(x)
  dx <- diff(domain)
  dy <- range[,2] - range[,1]
  range[,1] + (x - domain[,1]) * dy/dx
})



#colorize.character <- function(x,palette=NULL,...){
#  colorize.factor(as.factor(x),palette=palette,...)
#}

#colorize.integer <- function(x,palette=NULL,...){
#  lev <- unique(x)
# n <- length(lev)
# i <- match(x,lev)
# if (is.null(palette)){
#   map <- brewer.pal(max(3,n),name="Dark2")[1:n]
# } else {
#   map <- palette
#  }
#  map[i]
#}
