#' Adjust Individual Incomes by a specified factor
#'
#' This convenience function adjusts individual incomes in a numeric vector 'x' by a
#' specified factor 'by'.
#'
#' @param x Numeric vector representing individual incomes.
#' @param by Numeric factor by which to adjust the incomes.
#' @param digits The number of decimal places to return.
#' @return Numeric vector with adjusted individual incomes.
#' @details The function divides each element in the input vector 'x' by the
#'          specified factor 'by'. The result is rounded to three decimal places by default.
#'          The function is used for harmonizing incomes in the ISSP harmonization
#'           for PPP adjustment and LIS-equivalization.
#' @examples
#' # convert to monthly income
#' df1[annual=="annual", (vars_adjust):= lapply(.SD, adjust_inc, by=12), .SDcols=vars_adjust]
#'
#' @export
adjust_inc <- function(x, by, digits=3) {
  round(as.double(x/by), digits)
}
