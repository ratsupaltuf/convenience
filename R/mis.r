#' Calculate the percentage of missing values in a vector
#'
#' This function takes a vector as input and calculates the percentage of missing values in it.
#'
#' @param x A vector of numeric or character values
#'
#' @return A numeric value representing the percentage of missing values in the input vector
#'
#' @examples
#' mis(c(1, NA, 3, NA, 5))
#' mis(c("a", "b", "", NA, "d"))
#'
#' @export
mis <- function(x) {
  round(sum(is.na(x)) / length(x) * 100, 3)
}





