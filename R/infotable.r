
#' Extract information on income brackets from variable labels in the ISSP dataset
#'
#' This function takes a variable from the iSSP dataset, extracts information on
#' income brackets from its labels, and returns a data frame containing details
#' about the income brackets. It serves as the base function for the wave-specific functions (e.g., infotable_2005)
#'
#' @param variable The input variable from the iSSP dataset.
#' @param print Logical. If TRUE, the resulting table will be printed.
#' @return A data frame containing information on income brackets extracted from
#'         the variable labels.
#' @details The function uses the \code{alabs} function to generate a frequency
#'          table for the input variable, removes labels with certain NA patterns,
#'          and extracts numerical values from the remaining labels to form
#'          income brackets. The resulting data frame includes information on
#'          the lower and upper bounds of the brackets.
#' @examples
#' # Example with a variable 'income_variable' from the iSSP dataset
#' data <- infotable(income_variable, print = TRUE)
#'
#' @export
infotable<- function(variable, print=F) {

  # Generate a frequency table for the input variable and remove any labels that contain certain NA patterns.
  income<- alabs(variable, F, F)
  na_pattern <- "^NA|NAP|other countries|no income|Refuse|Refused|Don't know|DK|uncertain|no answer|can't choose|Can't say|Country specific variable not applicable for this country|Not in paid work|No own income, not in paid work"
  nunique <- length(unique(variable))
  na_labels <- grep(na_pattern, income$labels, ignore.case = T, value = TRUE)
  na_table <- income %>% filter(labels %in% na_labels)%>% mutate(na_value="NA_value")
  income <- income  %>% mutate(categorical=ifelse(length(labels)>=nunique, 1,0))

  # Extract numerical values from remaining labels and store them as a list of ranges.
  incomes_num<- income %>%  filter(  !(labels %in% na_labels)) %>%
    mutate(string_clean=stringr::str_replace_all(labels, "[,.\\s+]", "")) %>%
    mutate(num=str_extract_all(string_clean, "[0-9]+([.,][0-9]+)?"))
  num<- incomes_num$num
  numlist<- list()
  for (i in 1:length(num)) {
    x<- num[[i]]
    if(i==1 & length(x)<2)
      x<- c("1", x)
    if(i==length(num) & length(x)<2)
      x<- c(x, "-9999")
    x<-stringr::str_replace_all(x, "[,.]", "")
    x<- as.numeric(x)
    numlist[[i]] <- list(lower=x[1], upper=x[2])
  }

  # Convert the list of ranges into a data frame.
  df<- rbindlist(numlist)
  data<- cbind(incomes_num, df)

  # Combine the NA table with the numeric table, and add a column indicating whether each label is categorical or not.
  data<-rbindlist(list(data, na_table), fill=T)
  data$categorical <- na.omit(unique(data$categorical))

  # Print the resulting table if requested by the user, and return the table as a data frame.
  if(print==T) {print(data)}
  return(data)
}
