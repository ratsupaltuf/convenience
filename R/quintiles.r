#' Divide a numeric vector into quintiles based on provided probabilities
#'
#' This function takes a numeric vector and divides it into quintiles based on
#' the specified probabilities. It is a convenience function for the POLAR ISSP harmonization.
#'
#' @param x A numeric vector.
#' @param prob A numeric vector specifying the probabilities for dividing into
#'             quintiles.
#' @return A factor representing the quintile membership for each element in
#'         the input vector.
#' @details The function uses the \code{quantile} function to calculate the
#'          quantiles and then uses the \code{cut} function to categorize the
#'          elements into quintiles.
#' @examples
#' x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' quintiles(x, 0.2)
#'
#' @export
quintiles <- function(x, prob) {
  pq <- quantile(x, probs = seq(0, 1, prob), na.rm = TRUE)
  if (any(duplicated(pq))) {
    message("duplicated cutoffs, hard coding required")
    return(x)
  } else {
    x <- cut(x, breaks = pq, labels = FALSE, include.lowest = TRUE, right = TRUE)
    return(x)
  }
}
