#' Normalize a vector to range from 0 to 1
#'
#' @description This is a helper function to normalize vectors
#'
#' @param x A numeric vector

#' @return \code{x} ranging from 0 to 1
#' @examples
#' minmax(x)
#' @export

minmax<-function(x) {xnorm=(x-min(x, na.rm=T))/(max(x, na.rm = T)-min(x, na.rm = T))}


