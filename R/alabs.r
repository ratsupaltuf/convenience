#' Show values and labels of a labelled vector in table format
#'
#' @description
#' This is a helper function for the times you want to see values and labels of a labelled vector.
#'
#' @param x A labelled vector
#' @param kable Optional=should the output be processed by knitr::kable?
#' @return Value and labels of \code{x}
#' @examples
#' alabs(x, kable=F)
#'
#' @importFrom dplyr tibble
#' @importFrom knitr kable
#' @export

alabs<- function(x, kable=TRUE){
  tb<- tibble(labels=names(attributes(x)$labels), values=attributes(x)$labels)
  if(kable==T) {
  kable(tb)
  }  else {
    tb
  }
}
