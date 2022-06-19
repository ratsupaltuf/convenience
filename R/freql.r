#' Frequency table with value labels
#'
#' @description This is a shorthand for freq(as_label(x))
#'
#' @param x A labelled vector
#' @param cap A character specifying the title for the frequency table.
#' Defaul is the variable label
#' @param kable Optional=should the output be processed by knitr::kable?
#' @param d number of digits to be displayed
#' @return Frequency table as produced by sjlabelled::freq of \code{x}, but with labels
#' @examples
#' freql(x, y="Some title")
#' @importFrom summarytools freq
#' @importFrom knitr kable
#' @importFrom sjlabelled get_label
#' @export

freql <- function(x, cap=sjlabelled::get_label(x), kable=T, d=3, ...) {
  t<-summarytools::freq(as_label(x), round.digits = d)
  if (kable==T & !is.null(cap)) {
  knitr::kable(t, caption=cap)
    }else if (kable==T)
    {knitr::kable(t)
      } else return(t)

}
