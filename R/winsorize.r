
#' winsorize
#'
#' @param x numerical vector to winsorize
#' @param fixed.val
#' If NULL, performs winsorization using percentiles (set with prob)
#' If "iqr", winsorize using the interquartile (boxplot) criterion
#' If single numeric value, winsorize using this value as upper bound
#' @param prob Set the probability for the quantile function. Default is .99
#' @param show If TRUE (default), show how many cases were recoded to which value.
#'
#' @return A recoded vector with the upper extreme values replaced with less extreme values
#' @export
#'
#' @examples
#' #  Within data.table:
#' ess[, hhmmb.w:= winsorize(hhmmb, fixed.val="iqr", show=T)]
#'
winsorize <- function(x,
                      fixed.val = NULL,
                      prob = .99,
                      show = T) {
  if (is.null(fixed.val)) {
    maxval <- quantile(x, prob = prob, na.rm = T)
  } else if (is.numeric(fixed.val)) {
    maxval <- fixed.val
  } else if (fixed.val=="iqr") {
    maxval<- quantile(x, prob=.75,na.rm=T) + 1.5*IQR(x, na.rm = T)
  }
  if(show==T) {cat(sum(x>maxval, na.rm=T), "cases winsorized at value", maxval)}
  x <- ifelse(x > maxval, maxval, x)
  return(x)
}
