#'@title na.omit.info: Remove Missing Observations and Show Summary
#'
#' @description This function removes rows with missing observations from a data frame and
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
#'
na.omit.info <- function(x) {
total_obs <- nrow(x)
na_counts <- colSums(is.na(x))

x_clean <- na.omit(x)
total_obs_clean <- nrow(x_clean)

deleted_obs <- total_obs - total_obs_clean
message(deleted_obs, " observations deleted in total")

# Calculate percentage of missing values by column
missing_percentage <- (na_counts / total_obs) * 100

# Create a table of missing observations by variable
missing_table <- data.frame(Variable = names(na_counts),
                            MissingObservations = na_counts,
                            MissingPercentage = missing_percentage,
                            stringsAsFactors = FALSE)

# Round the percentage to two decimal places
missing_table$MissingPercentage <- round(missing_table$MissingPercentage, 2)

# Print the table to the console
print(missing_table)

return(x_clean)
}

