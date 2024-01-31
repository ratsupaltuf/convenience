#' Calculate the Weighted Median of a Numeric Vector
#'
#' This function calculates the weighted median of a numeric vector 'x' using
#' corresponding weights 'w'.
#'
#' @param x Numeric vector for which the weighted median is calculated.
#' @param w Weights corresponding to the elements of 'x'.
#' @param na.rm Logical. If TRUE, missing values in 'x' and 'w' will be removed
#'              before calculation (default: TRUE).
#' @return Weighted median of the input vector 'x'.
#' @details The function sorts 'x' in ascending order and calculates the
#'          cumulative sum of weights 'w'. It then identifies the index where
#'          the cumulative sum exceeds half of the total weight, returning the
#'          corresponding value in 'x' as the weighted median.
#' @examples
#' # Calculate the weighted median with default parameters
#' result <- weighted.median(c(1, 2, 3), c(0.2, 0.5, 0.3))
#'
#' # Calculate the weighted median with missing values removed
#' result_na_rm <- weighted.median(c(1, NA, 3), c(0.2, NA, 0.3), na.rm = TRUE)
#'
#' @export
weighted.median <- function(x, w, na.rm=T) {
  if(na.rm==T) {
    # Remove missing values
    non_missing <- !is.na(x) & !is.na(w)
    x <- x[non_missing]
    w <- w[non_missing]
  }
  if (length(x) != length(w)) {
    stop("Error: The lengths of 'x' and 'w' must be the same. Try removing NAs")
  }

  sorted_indices <- order(x)
  sorted_x <- x[sorted_indices]
  sorted_w <- w[sorted_indices]

  cumulative_weights <- cumsum(sorted_w)
  half_weight <- sum(sorted_w) / 2

  idx <- which(cumulative_weights >= half_weight)[1]

  return(sorted_x[idx])
}
