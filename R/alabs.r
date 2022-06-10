#' Show values and labels of a labelled vector in table format
#'
#' @description
#'
#' @param x A labelled vector
#' @param kable Optional=should the output be processed by knitr::kable?
#' @return Value and labels of \code{x}
#' @examples
#' alabs(x, kable=F)

alabs<- function(x, kable=TRUE){
  tb<- tibble(labels=names(attributes(x)$labels), values=attributes(x)$labels)
  if(kable==T) {
  knitr::kable(tb)
  }  else {
    tb
  }
}
