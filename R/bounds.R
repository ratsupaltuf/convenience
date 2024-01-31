#' Generate Lower and Upper Bounds for Income Brackets
#'
#' This function generates lower and upper bounds for income brackets based on
#' the input vector 'x'. Convenience function used in the ISSP income harmonization.
#'
#' @param x Numeric vector representing midpoints or values for income brackets.
#' @param return Character. Determines what to return.
#'   - "lower": Return only the lower bounds.
#'   - "upper": Return only the upper bounds.
#'   - "both": Return both lower and upper bounds (default).
#' @param low Logical. If TRUE, assumes the lowest value is likely the upper
#'            limit of the lowest bracket. Default is FALSE.
#' @return Depending on the 'return' parameter:
#'   - If 'return' is "lower", a numeric vector of lower bounds.
#'   - If 'return' is "upper", a numeric vector of upper bounds.
#'   - If 'return' is "both", a list containing both lower and upper bounds.
#' @details For categorical variables where the interview cards could not be obtained, we
# impute the bounds assuming that values are indeed midpoints, i.e. there is
# equal distance between the coded value and the underlying upper and lower
# bound.
#' The function calculates lower and upper bounds for income brackets
#'          based on the midpoints or values provided in the input vector 'x'.
#'          It handles the case where the lowest value is likely the upper limit
#'          of the lowest bracket if 'low' is TRUE.
#'
#' @examples
#' # ISSP 2002: NL, AT, AU, GB, US -> no interview card available -> use function
#'
#' df02[iso2c %in% c("AT", "AU", "US") & rinc!=0, c("lower_rinc", "upper_rinc"):= bounds(rinc), by=iso2c]
#' df02[iso2c == "GB", c("lower_rinc", "upper_rinc"):= bounds(rinc, low=T), by=iso2c]
#' df02[rinc==0, `:=`(lower= NA, upper=NA)]
#' df02[lower_rinc>upper_rinc & upper_rinc!= -9999, unique(iso2c)]
#' df02[iso2c=="GB" & rinc==1500, lower_rinc:=1]
#'
#' @export
bounds <- function(x, return="both", low=F) {
  if(low==T) { # if lowest value is likely the upper limit of lowest bracket
    incomes <- data.frame(midpoints=x)
    midpoints <- sort(unique(x))
    differences <- diff(c(0, midpoints))


    g<- data.frame(midpoints, differences, upper= midpoints + differences/2)
    g$upper[1] <- g$midpoints[1]
    g<- g %>% mutate(lower= midpoints- (midpoints-lag(midpoints))/2,
                     upper= midpoints+ (lead(midpoints)-midpoints)/2)

    # most likely, last code is "more than", and thus the upper limit of the last category
    g<-g %>% mutate(upper= ifelse(midpoints==g$midpoints[length(g$midpoints)-1],g$midpoints[length(g$midpoints)], upper)) %>%
      mutate(lower= ifelse(midpoints==g$midpoints[length(g$midpoints)],g$midpoints[length(g$midpoints)]+1, lower)) %>%
      mutate(upper=ifelse(midpoints==g$midpoints[length(g$midpoints)],-9999, upper))
    g<- g %>% mutate(across(c("lower", "upper"), round))
    final<- left_join(incomes, g, by = "midpoints",  relationship="many-to-one")

  } else {


    incomes <- data.frame(midpoints=x)
    midpoints <- sort(unique(x))
    differences <- diff(c(0, midpoints))

    g<- data.frame(midpoints, differences, upper= midpoints + differences/2)
    g<- g %>% mutate(lower=lag(upper) + 1) %>% select(midpoints, lower, upper)
    g$lower[1] <- 1
    # most likely, last code is "more than", and thus the upper limit of the last category
    g<-g %>% mutate(upper= ifelse(midpoints==g$midpoints[length(g$midpoints)-1],g$midpoints[length(g$midpoints)], upper)) %>%
      mutate(lower= ifelse(midpoints==g$midpoints[length(g$midpoints)],g$midpoints[length(g$midpoints)]+1, lower)) %>%
      mutate(upper=ifelse(midpoints==g$midpoints[length(g$midpoints)],-9999, upper))
    g<- g %>% mutate(across(c("lower", "upper"), round))
    final<- left_join(incomes, g, by = "midpoints",  relationship="many-to-one")


  }

  if (return=="lower") {
    return(
      final$lower) } else if (return== "upper") {
        return (final$upper)
      } else
        return(
          list(final$lower, final$upper)
        )
}
