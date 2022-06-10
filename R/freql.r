#' Frequency table with value labels
#'
#' @description This is a shorthand for freq(as_label(x))
#'
#' @param x A labelled vector
#' @param y A title for the frequency table
#' @param kable Optional=should the output be processed by knitr::kable?
#' @return Frequency table as produced by sjlabelled::freq of \code{x}, but with labels
#' @examples
#' freql(x, y="Some title")
#' @importFrom summarytools freq
#' @importFrom knitr kable
#' @importFrom sjlabelled get_label
#' @export

freql <- function(x, y="") {
  summarytools::freq(as_label(x)) %>%
    knitr::kable(caption=paste(y, "-", sjlabelled::get_label(x), sep=" "), digits=2)
}
