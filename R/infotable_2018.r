
#' Extract information on income brackets from variable labels in the ISSP dataset,
#' used in years 2018, 2019
#'
#' This function takes a variable from the iSSP dataset, extracts information on
#' income brackets from its labels, and returns a data frame containing details
#' about the income brackets.
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
#' vars<- names(df05)[grep( "_INC", names(df05))]
#'
#'
#' infotable_df<- purrr::imap(df05[,..vars], function(.x, .z) {
#'   print(.z)
#'   df<-infotable_2005(.x, F)
#'   df$var<- .z
#'   #df$C_ALPHAN_2 <- substr(df$var, start = 1, stop = 2)
#'   return(df)}
#' )
#' infotable_df<-rbindlist(infotable_df)
#' infotable_df[, num:=as.character(num)]
#' infotable_df[, iso2c := substr(var, start = 1, stop = 2)]
#' infotable_df[inf_add, on="iso2c", `:=`(metric=i.metric, annual=i.interval, net=i.gross_net)]
#'
#' @export
infotable_2018<- function(variable, print=F) {
  income<- alabs(variable, F, F)
  na_pattern<-"^NA|NAP|other countries|no income|Refuse|Refused|Don't know|uncertain|no answer|can't choose|Can't say"

  nunique <- length(unique(variable))
  na_labels <- grep(na_pattern, income$labels, ignore.case = T, value = TRUE)
  na_table <- income %>% filter(labels %in% na_labels)%>% mutate(na_value="NA_value")
  income <- income  %>% mutate(categorical=ifelse(length(labels)>=nunique, 1,0))

  incomes_num<- income %>%  filter(  !(labels %in% na_labels)) %>%
    # mutate(labels = ifelse(
    #   str_detect(labels, "CZK"), str_replace(labels, ".*\\/", ""),
    #   str_replace(labels, "^[^.]*\\.", ""))) %>%

    #mutate(string_clean = str_replace(labels, "^[^.]*\\.", "")) %>%
    mutate(string_clean = labels) %>%
    mutate(string_clean = ifelse(
      str_detect(string_clean, "^[^.]*\\."), str_replace(string_clean, "^[^.]*\\.", ""),
      string_clean)) %>%

    mutate(string_clean=stringr::str_replace_all(string_clean, "[,.\\s+]", "")) %>%
    mutate(num=
             str_extract_all(string_clean, "[0-9]+([.,][0-9]+)?"))

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
    # return(x)
    #print(i)
    numlist[[i]] <- list(lower=x[1], upper=x[2])
  }
  numlist
  df<- rbindlist(numlist)
  df
  data<- cbind(incomes_num, df)
  data<-rbindlist(list(data, na_table), fill=T)
  data$categorical <- na.omit(unique(data$categorical))
  if(print==T) {print(data)}

  return(data)
}
