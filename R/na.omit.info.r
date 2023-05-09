#' na.omit.info: Remove Missing Observations and Show Summary
#'
#' This function removes rows with missing observations from a data frame and
#' displays a summary table showing the number of missing observations and
#' the percentage of missing values deleted by column.
#'
#' @param x A data frame containing the data to be cleaned.
#'
#' @return A cleaned data frame with missing observations removed.
#' @examples
#' # Example data frame
#' data <- data.frame(A = c(1, 2, NA, 4, 5),
#'                    B = c(NA, 2, 3, 4, NA),
#'                    C = c(1, NA, NA, 4, 5))
#'
#' # Use na.omit.info function
#' cleaned_data <- na.omit.info(data)
#'
#' @export
