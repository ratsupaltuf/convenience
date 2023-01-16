#' Winsorize using the interquartile criterion
#'
#' @param x The numeric vector that is to be winsorized
#'
#' @return A numeric vector where values above Q75 + 1.5*IQR are replaced with that value
#' @export
#'
#' @examples
#' winsorize(df$eduyrs)
winsorize.box <- function(x) {
  val<- round(quantile(x, na.rm=T, probs = .75)+ 1.5*IQR(x, na.rm=T))
  x<- ifelse(x>val, val, x)
  return(x)
}
