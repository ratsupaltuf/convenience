#' Cross-table of countries and years in a data frame
#'
#' This function generates a cross-table of countries and years based on the input data frame.
#' The table includes sample sizes for each combination of country and year, as well as totals for each year and country.
#'
#' @param data The input data frame.
#' @param country The column name representing countries in the data frame.
#' @param year The column name representing years in the data frame.
#' @param replace.na A vector of values to replace NA (missing) values in the table. If set to \code{NULL}, NA values will not be replaced.
#' @param kable A logical value indicating whether to format the table using the \code{kable} function from the \code{knitr} package.
#' If set to \code{TRUE}, the table will be formatted; otherwise, the raw data frame will be returned.
#'
#' @return The function returns a data frame containing the cross-table of countries and years.
#' If \code{kable} is set to \code{TRUE}, the result will be formatted using the \code{kable} function.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' desc_sample(data = your_data_frame, country = "iso2c", year = "year", replace.na = "-", kable = TRUE)
#' }
#'
#' @details
#' - The function first calculates the sample size per year and country using the \code{group_by} and \code{summarise} functions from the \code{dplyr} package.
#' - It then spreads the data to create a wide-format table.
#' - Totals for each row and column are calculated and added to the table.
#' - The function provides an option to replace NA values in the table with a specified vector of replacement values (\code{replace.na} parameter).
#' - The function also calculates and appends the minimum and maximum number of countries for each year.
#' - If \code{kable} is set to \code{TRUE}, the resulting data frame is formatted using the \code{kable} function.
#'
#' @note Make sure to have the necessary packages (\code{dplyr} and \code{knitr}) installed and loaded in your R environment before using this function.
#'
#'
#' @seealso
#' \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[knitr]{kable}}
#'
#'
#' @author Simon Bienstman
#'
#' @export
desc_sample<- function(data, country, year, replace.na=NULL, kable=T){
  #Table showing sample size per year and country,
  N1 <- data %>% group_by(country, year) %>% summarise("N"=n())
  N1 <- N1 %>% spread(year, N)
  N1$'Total' <- c(rowSums(N1[,-1], na.rm = T))
  t<- as.list(colSums(N1[,-1], na.rm = T))
  t<-append("Total (Subjects)", as.list(t))

  names(t)<- c("Year", unique(data[[year]]), "Total")
  names(N1) <- c("Year", unique(data[[year]]), "Total")
  N1 <- rbind(N1,t)
  N1$Total <- as.numeric(as.character(N1$Total))
  #min and max countries
  N2 <- data %>% mutate(year=as.numeric(year)) %>% group_by(year) %>% summarise(N= length(unique(country)))
  N2 <- N2 %>% spread(year, N)
  Total <- length(unique(data[[country]]))
  b <- c("Total (Countries)")
  N2 <- cbind(b, N2,Total)
  names(N2) <- c("Year", sort(unique(data[[year]])), "Total")

  N1 <- bind_rows(N1, N2)

  if(!is.null(replace.na)) {
    N1 <- apply(N1,2, as.character)
    N1[is.na(N1)] <- replace.na
  }

  if(kable==T) {

    N1 <-  N1 %>% knitr::kable()
  }
  return(N1)
}
