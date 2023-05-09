#' Calculate the percentage of missing values in a vector or a list of vectors
#'
#' This function calculates the percentage of missing values in a vector or a list of vectors.
#'
#' @usage mis(x)
#'
#' @param x A vector or a list of vectors
#'
#' @return A numeric value or a vector of numeric values representing the percentage of missing values
#'
#' @examples
#' mis(c(1,2,NA,4,NA)) # returns 40
#' mis(list(c(1,2,NA), c(NA,3,4), c(NA,NA,NA))) # returns c(33.33, 66.67, 100)
#'
#' @export
mis <- function(x) {
  if(is.list(x)) {
    lapply(x, function(y) {
      round(sum(is.na(y)) / length(y) * 100, 3)
    })
  } else {
    round(sum(is.na(x)) / length(x) * 100, 3)
  }
}




