#' Compute social class based on dominance principle
#'
#' This function assigns social class based on the dominance principle.
#' The function takes three input variables: \code{resp}, \code{partner}, and \code{higher}.
#' \code{resp} and \code{partner} represent the respondent and partner, respectively. If the respondent's class is missing, it takes the partner's class.
#' If either \code{resp} or \code{partner} is a factor variable, it is converted to numeric before computation.
#' The resulting variable is assigned labels based on the labels of the \code{resp} variable.
#' The \code{higher} parameter determines the interpretation of class values; if \code{higher} is FALSE (default), lower values indicate higher class. If \code{higher} is TRUE, higher values indicate higher class.
#'
#' @param resp A vector containing data for the respondent.
#' @param partner A vector containing data for the partner.
#' @param higher A logical value indicating whether higher values represent higher social class (default is FALSE).
#'
#' @return A vector containing the social class variable.
#'
#' @examples
#' dominance(c(1,2,3), c(2,1,NA), FALSE)
#' dominance(factor(c("low","high","medium")), factor(c("high","low",NA)), TRUE)
#' dominance(c(1,2,3), c(2,1,NA), FALSE) %>% set_labels(c("Lower class", "Middle class", "Upper class"))
#'
#' @importFrom sjlabelled get_labels as_numeric set_labels
#' @importFrom dplyr case_when
#' @export
dominance<- function(resp, partner, higher=F) {
  # Assign social class based on dominance principle
  # if higher==F, lower values equal higher class
  # If respondent class is missing it takes partner class
  # Extract labels of resp variable
  labs<- sjlabelled::get_labels(resp)

  # Convert factors to numeric
  if(is.factor(resp)) {resp <- sjlabelled:: as_numeric(resp)}
  if(is.factor(partner)) {partner <- sjlabelled::as_numeric(partner)}
  # Assign values to hhclass variable based on conditions
if(higher==F) {
    hhclass<- case_when(
    is.na(resp) & is.na(partner) ~ NA_real_,
    is.na(resp) & !is.na(partner) ~ partner,
    !is.na(resp) & is.na(partner) ~ resp,
    resp <= partner ~ resp,
    TRUE ~ partner # default condition for cases that don't meet any of the above
  )
} else if(higher==T) {
  hhclass<- case_when(
    is.na(resp) & is.na(partner) ~ NA_real_,
    is.na(resp) & !is.na(partner) ~ partner,
    !is.na(resp) & is.na(partner) ~ resp,
    resp >= partner ~ resp,
    TRUE ~ partner # default condition for cases that don't meet any of the above
  )
}
  # Assign labels to hhclass variable
  hhclass <- set_labels(hhclass, labels=labs)

  return(hhclass)
}
