#' Show values and labels of a labelled vector in table format
#'
#' @description
#' This is a helper function for the times you want to see values and labels of a labelled vector.
#'
#' @param x A labelled vector
#' @param kable Should the output be processed by knitr::kable? Default is TRUE
#' @param extended Should extended information be displayed? Default is TRUE.
#' @return Value and labels of \code{x}. If extended==T, return Label, class and unique values.
#' @examples
#' alabs(x, kable=F)
#'
#' @importFrom dplyr tibble
#' @importFrom knitr kable
#' @export

alabs<- function(x, kable=TRUE, extended=T){
  tb<- tibble(labels=names(attributes(x)$labels), values=attributes(x)$labels)

  if(extended==T) {
  lab<- attributes(x)$label
  cl<- class(x)
  u<- unique(x)

  if(kable==T) {

    cat(lab, "\n",
        "Class:",cl, "\n",
        "Unique values:", u, "\n",
        "\n"
    )
    print(knitr::kable(tb, caption="Labels"))

  }  else {
    cat(lab, "\n",
        "Class:",cl, "\n",
        "Unique values:", u, "\n",
        "\n"
    )
    print(tb)
  }
  } else if (kable==T) {

  kable(tb)
  }  else {
    tb
  }
}
