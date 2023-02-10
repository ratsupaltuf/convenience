
#' Code income quintiles
#'
#' @param x Numeric (labelled) Vector
#' @param lower Numeric. All values below "lower" are coded as NA. Defaults to 0.
#' @param upper Logical. If TRUE (default), the vector contains unclassified NA values at the upper tail that are to be recoded
#' @param prob Argument passed to `quantile()` and the result to `cut()`. Default is .2
#' @param show Logical. If TRUE (default), show the last five values and corresponding values and which whether they were coded as NA
#'
#' @return A numerical vector with values recoded their quantile of the original distribution, specified by prob.
#'   If show==T, prints info on which of the labels and values at the
#'   tail were NA coded to the console.
#'
#' @description The function codes NA values for the (labelled) numeric vector
#'   `x` and recodes `x` to specified quantiles. Users can give lower bounds for
#'   NA codes manually. If upper==F, no upper bound value is coded as NA. If upper==T, upper bounds are automatically detected as those values
#'   at the tail with a probability of >.3 and all values above that bound are NA coded.
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' Example from ISSP: Recoding of country-specific household income variables
#' HH income (HHINCOME)
#' df10<-df10 %>% mutate(
#'   across(ends_with("_INC"),
#'          inc_quintiles,
#'          .names = "quint_{.col}")
#' )
#'
#' quintcols<-names(df10)[grep("quint", names(df10))]
#' df10[, hhinc:=do.call(pmin, c(.SD, na.rm=TRUE)), .SDcols=quintcols]
#' df10[, c(quintcols):=NULL]

inc_quintiles<- function(x, lower=0, upper=T, prob=.2, show=T) {
  if(show==T) {
    # if you want to print the labels of the last five values
    t.show<-tail(tibble(labels = names(attributes(x)$labels), values = attributes(x)$labels))
  }

  x<- remove_all_labels(as_numeric(x))
  if(upper==F) {
    x<-ifelse(x>=lower, x, NA)
  } else if(upper==T) {
    # take the five largest values, if one of them has a probability of 0.1 or higher, this
    # must be an NA value. Assuming that all values following the first NA value
    # are NA's, this is the cut-off for NA coding.
    # Then get its name (i.e. the numerical value) and save
    t<- prop.table(table(x))*100
    upper <-as.numeric(names(tail(t)[tail(t)>0.3]))[1]
    x<-ifelse(x>=0 & x<upper, x, NA)}

  pq<- quantile(x, probs=seq(0,1,prob), na.rm=T)
  x<- cut(x, breaks=pq, labels=F, include.lowest=T, right=T)
  if(show==T) {
    # Additional check: print the last five labels indicate which were NA coded
    t.show$Code <- ifelse(t.show$values>=upper, "NA", "-")
    print(t.show)
  }
  return(x)
}
