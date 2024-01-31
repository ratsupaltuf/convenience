#' Top-bottom coding for income variables
#'
#' This function limits the values in a numeric vector to a specified range
#' based on the chosen type of range adjustment.
#'
#' @param x Numeric vector to be adjusted.
#' @param type Type of range adjustment (1 or 2).
#'   - Type 1: Adjust based on mean and median, see details
#'   - Type 2: Adjust based on quantiles and interquartile range.
#' @param is.log Logical. If TRUE, the input vector is assumed to be log-transformed.
#'               Default is FALSE.
#' @return Numeric vector with values adjusted to the specified range.
#' @details The function adjusts the values in the input vector 'x' based on the
#'          chosen type of range adjustment. For Type 1, it adjusts values based
#'          on the mean and median, i.e. the standard LIS approach.
#'          Here, the bottom code is equal to a hundredth of the mean
#'          income and the top income is equal to ten times the median income.
#'
#'          For Type 2, it adjusts values based on
#'          quantiles and interquartile range of the logged distribution, following Neugschwender (2020).
#'          If 'is.log' is TRUE, the function
#'          handles the input vector as if it's log-transformed.
#'
#' @references
#' Neugschwender, J. (2020). Top and Bottom Coding at LIS. LIS Technical Working Paper No. 9. https://www.lisdatacenter.org/wps/techwps/9.pdf
#' @examples
#' df10[!is.na(rinc_imp), rinc_imp_tbs:=topbottom(rinc_imp), by=iso2c]
#' df10[!is.na(rinc_imp), rinc_imp_tbn:=topbottom(rinc_imp, type=2, is.log=F), by=iso2c]
#'
#' @export
topbottom <- function(x, type=1, is.log=FALSE) {

  original_x <- x # Keep the original vector
  non_zero_x <- x[x != 0 & !is.na(x)]  # Exclude zero & NA values for calculations

  if (length(non_zero_x) <= 1) {
    stop("Vector contains no non-missing positive values")
  }

  # start with if type 1
  if(type==1) {
    bottom <- mean(non_zero_x, na.rm=TRUE) / 100
    top <- median(non_zero_x, na.rm=TRUE) * 10

  } else if (type==2){
    # else if type two

    if (!is.log) {
      # if not logged, then log
      non_zero_x <- log(non_zero_x)
      bottom <- exp(quantile(non_zero_x, probs=0.25, na.rm=TRUE) - 3 * IQR(non_zero_x, na.rm=TRUE))
      top <- exp(quantile(non_zero_x, probs=0.75, na.rm=TRUE) + 3 * IQR(non_zero_x, na.rm=TRUE))
    } else if(is.log) {

      # else if already logged, continue as is
      bottom <- quantile(non_zero_x, probs=0.25, na.rm=TRUE) - 3 * IQR(non_zero_x, na.rm=TRUE)
      top <- quantile(non_zero_x, probs=0.75, na.rm=TRUE) + 3 * IQR(non_zero_x, na.rm=TRUE)
    }
  }

  # continue loop
  original_x[original_x<bottom] <- bottom
  original_x[original_x>top] <- top
  #return original
  return(original_x)

}
