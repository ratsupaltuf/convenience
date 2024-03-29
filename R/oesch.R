#' Code Oesch class schema
#'
#'
#'
#' @param isco ISCO codes (preferably 4-digits)
#' @param isco08 ISCO 08 or 88 codes?
#' @param self.employed Numeric vector with 0=not self-employed and 1=self-employed
#' @param n.employees Numeric vector with the number of employees. Will be recoded to 0/<=10/>10
#' @param n.classes Numeric value indicating desired degree of differentiation of class schema
#'
#' @description This function codes the Oesch class schema based on respondent's occupation, employment status, and number of employees
#'
#' @references
#' https://github.com/DiogoFerrari/occupar -> The function is largely adapted from Ferrari's code.
#'
#' https://people.unil.ch/danieloesch/scripts/ -> The body of the function (the class coding) is from Oesch's code.
#'
#' Oesch, D. (2006). Coming to grips with a changing class structure: An analysis of employment stratification in Britain, Germany, Sweden and Switzerland. International Sociology, 21(2), 263–288. https://doi.org/10.1177/0268580906061379
#'
#' @return The Oesch class code
#' @author Simon Bienstman
#'
#' @examples oesch(isco=data$isco, isco08=TRUE, self.employed=data$selfem, n.employees=data$n_employees, n.classes=5)
#' @note
#'
#' For function input, we need to properly code NA in n.employees before calling function
#' Those who are working for family business are coded as self-employed without employees in Oesch schema
#' -> need to fix that as well before function call
#' those working for family business have no empl. anyways -> ess[emplrel==3, table(emplno)]
#'
#' freq(ess$emplrel)
#'
#' ess[, self.employed:=0]
#'
#'
#' ess[emplrel==2 | emplrel==3, self.employed:=1]
#'
#' tail(freq(ess$emplno))
#'
#' ess[, n.employees:=ifelse(is.na(emplno) | emplno>66665,0, emplno)]
#'
#' freq(ess$self.employed)
#'
#' summary(ess$n.employees)
#'
#' For input variables, see above.
#'
#' @importFrom sjlabelled set_label set_labels
#' @export

oesch <- function(isco, isco08=TRUE, self.employed=NULL, n.employees=NULL, n.classes=16){
  ## check number of categories allowed
  ## ----------------------------------
  if (!(n.classes %in% c(16,8,5))) {
    stop("\n\nNumber of classes allowed to compute Oesch class scheme are 16, 8, or 5.\n\n")
  }

  if (is.null(self.employed)) {
    stop("\n\nThe parameters emplrel must be non-NULL to compute Oesch.\n\n")
  }

  if (is.null(n.employees)) {
    # Following Oesch for Partner's class ESS 6-9
    message("\n\nIf n.employees is NULL, pragmatically assign '0' and produce a reduced class schema.\n\n")
    n.employees <- rep(0, length(isco))
  }

    ###########################################
  # Compute employment status categories
  ###########################################

  self.employed[self.employed != 1 | is.na(self.employed)] <- 0
  n.employees[is.na(n.employees)] <- 0


  selfem_mainjob <- rep(NA, length(isco))
  selfem_mainjob[self.employed == 1 & n.employees > 9]        <- 4  ####  self-employed 10+ employees
  selfem_mainjob[self.employed == 1 & n.employees %in% 1:9]   <- 3  ####  small employers <10
  selfem_mainjob[self.employed == 1 & n.employees == 0]       <- 2  ####  self-employed, no employees
  selfem_mainjob[self.employed != 1]                          <- 1  ####  employee





  d<- data.frame(isco_mainjob=isco, selfem_mainjob)
  d$isco_mainjob[is.na(d$isco_mainjob)] <- -9


  if (isco08==F) {



  if (any(isco[(!is.na(isco)) & isco != 10] %>% nchar == 4)) {


# Create Oesch class schema for respondents ISCO88 4-digit ------


    d$class16 <- -9

    # Large employers (1)

    d$class16[d$selfem_mainjob == 4] <- 1

    # Self-employed professionals (2)

    d$class16[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2000 & d$isco_mainjob <= 2229] <- 2
    d$class16[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2300 & d$isco_mainjob <= 2470] <- 2

    # Small business owners with employees (3)

    d$class16[d$selfem_mainjob == 3 & d$isco_mainjob >= 1000 & d$isco_mainjob <= 1999] <- 3
    d$class16[d$selfem_mainjob == 3 & d$isco_mainjob >= 3000 & d$isco_mainjob <= 9333] <- 3
    d$class16[d$selfem_mainjob == 3 & d$isco_mainjob == 2230] <- 3

    # Small business owners without employees (4)

    d$class16[d$selfem_mainjob == 2 & d$isco_mainjob >= 1000 & d$isco_mainjob <= 1999] <- 4
    d$class16[d$selfem_mainjob == 2 & d$isco_mainjob >= 3000 & d$isco_mainjob <= 9333] <- 4
    d$class16[d$selfem_mainjob == 2 & d$isco_mainjob == 2230] <- 4

    # Technical experts (5)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2100 &  d$isco_mainjob <= 2213] <- 5

    # Technicians (6)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3100 &  d$isco_mainjob <= 3152] <- 6
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3210 &  d$isco_mainjob <= 3213] <- 6
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 3434] <- 6

    # Skilled manual (7)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 6000 &  d$isco_mainjob <= 7442] <- 7
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 8310 &  d$isco_mainjob <= 8312] <- 7
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 8324 &  d$isco_mainjob <= 8330] <- 7
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 8332 &  d$isco_mainjob <= 8340] <- 7

    # Low-skilled manual (8)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 8000 &  d$isco_mainjob <= 8300] <- 8
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 8320 &  d$isco_mainjob <= 8321] <- 8
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 8331] <- 8
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 9153 &  d$isco_mainjob <= 9333] <- 8

    # Higher-grade managers and administrators (9)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 1000 &  d$isco_mainjob <= 1239] <- 9
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2400 &  d$isco_mainjob <= 2429] <- 9
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 2441] <- 9
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 2470] <- 9

    # Lower-grade managers and administrators (10)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 1300 &  d$isco_mainjob <= 1319] <- 10
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3400 &  d$isco_mainjob <= 3433] <- 10
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3440 &  d$isco_mainjob <= 3450] <- 10

    # Skilled clerks (11)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 4000 &  d$isco_mainjob <= 4112] <- 11
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 4114 &  d$isco_mainjob <= 4210] <- 11
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 4212 &  d$isco_mainjob <= 4222] <- 11

    # Unskilled clerks (12)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 4113] <- 12
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 4211] <- 12
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 4223] <- 12

    # Socio-cultural professionals (13)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2220 &  d$isco_mainjob <= 2229] <- 13
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2300 &  d$isco_mainjob <= 2320] <- 13
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2340 &  d$isco_mainjob <= 2359] <- 13
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2430 &  d$isco_mainjob <= 2440] <- 13
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2442 &  d$isco_mainjob <= 2443] <- 13
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 2445] <- 13
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 2451] <- 13
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 2460] <- 13

    # Socio-cultural semi-professionals (14)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 2230] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2330 &  d$isco_mainjob <= 2332] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 2444] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2446 &  d$isco_mainjob <= 2450] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 2452 &  d$isco_mainjob <= 2455] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 3200] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3220 &  d$isco_mainjob <= 3224] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 3226] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3229 &  d$isco_mainjob <= 3340] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3460 &  d$isco_mainjob <= 3472] <- 14
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 3480] <- 14

    # Skilled service (15)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 3225] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3227 &  d$isco_mainjob <= 3228] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 3473 &  d$isco_mainjob <= 3475] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 5000 &  d$isco_mainjob <= 5113] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 5122] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 5131 &  d$isco_mainjob <= 5132] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 5140 &  d$isco_mainjob <= 5141] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 5143] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 5160 &  d$isco_mainjob <= 5220] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 8323] <- 15

    # Low-skilled service (16)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 5120 &  d$isco_mainjob <= 5121] <- 16
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 5123 &  d$isco_mainjob <= 5130] <- 16
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 5133 &  d$isco_mainjob <= 5139] <- 16
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 5142] <- 16
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 5149] <- 16
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 5230] <- 16
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 8322] <- 16
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 9100 &  d$isco_mainjob <= 9152] <- 16

  } else if (any(isco[(!is.na(isco)) & isco != 10] %>% nchar %in% c(2,3))) {


    if (any(isco[(!is.na(isco)) & isco != 10] %>% nchar ==3)) {
      isco = isco %>% stringr::str_replace(string=., pattern=".$", replacement="")
    }


# Create Oesch class schema for respondents ISCO88 2-digit --------


    d$class16_r<- -9

    # Large employers (1)

    d$class16[d$selfem_mainjob == 4] <- 1

    # Self-employed professionals (2)

    d$class16[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 20 & d$isco_mainjob <= 24] <- 2

    # Small business owners with employees (3)

    d$class16[d$selfem_mainjob == 3 & d$isco_mainjob >= 10 & d$isco_mainjob <= 13] <- 3
    d$class16[d$selfem_mainjob == 3 & d$isco_mainjob >= 30 & d$isco_mainjob <= 93] <- 3
    #ivs$class16_p[ivs$selfem_partner == 3 & ivs$isco_partner == 2230] <- 3
    #ISCO2230 is Nursing and midwifery professionals - not possible for 2-digit - thus covered under Class 2

    # Small business owners without employees (4)

    d$class16[d$selfem_mainjob == 2 & d$isco_mainjob >= 10 & d$isco_mainjob <= 19]<-4
    d$class16[d$selfem_mainjob == 2 & d$isco_mainjob >= 30 & d$isco_mainjob <= 93]<- 4
    #ivs$class16_p[ivs$selfem_partner == 2 & ivs$isco_partner == 2230] <- 4
    #ISCO2230 is Nursing and midwifery professionals - not possible for 2-digit - thus covered under Class 2

    # Technical experts (5)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 20 & d$isco_mainjob <= 22 ]<- 5
    #teaching professionals (ISCO 23) get code 13

    # Technicians (6)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 30 & d$isco_mainjob <= 31]<-6
    # ISCO 30 (technicians and associate professionals are now included as technicians)
    #ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 3210 & ivs$isco_partner <= 3213] <- 6 #ISCO 32 is c14
    #ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 3434] <- 6 #other associate professionals get c10


    # Skilled manual (7)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 60 & d$isco_mainjob <= 75] <- 7
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 8310 & ivs$isco_partner <= 8312] <- 7
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 8324 & ivs$isco_partner <= 8330] <- 7
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 8332 & ivs$isco_partner <= 8340] <- 7

    # Low-skilled manual (8)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 80 & d$isco_mainjob <= 84] <- 8
    # includes ISCO 84 semiskilled nfs
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 8320 & ivs$isco_partner <= 8321] <- 8
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 8331] <- 8
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 92 & d$isco_mainjob <= 93]<- 8

    # Higher-grade managers and administrators (9)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 10 & d$isco_mainjob <= 12] <- 9
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 24] <-9
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 2441] <- 9
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 2470] <- 9

    # Lower-grade managers and administrators (10)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 13]<- 10
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 34]<- 10

    # Skilled clerks (11)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 40 & d$isco_mainjob <= 42] <- 11
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 4114 & ivs$isco_partner <= 4210] <- 11
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 4212 & ivs$isco_partner <= 4222] <- 11

    # Unskilled clerks (12)

    #not possible for 2-digit ISCO
    #
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 4113] <- 12
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 4211] <- 12
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 4223] <- 12

    # Socio-cultural professionals (13)

    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob == 23] <-13
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 2300 & ivs$isco_partner <= 2320] <- 13
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 2340 & ivs$isco_partner <= 2359] <- 13
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 2430 & ivs$isco_partner <= 2440] <- 13
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 2442 & ivs$isco_partner <= 2443] <- 13
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 2445] <- 13
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 2451] <- 13
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 2460] <- 13

    # Socio-cultural semi-professionals (14)

    d$class16[d$selfem_mainjob == 1 & (d$isco_mainjob == 32 | d$isco_mainjob==33)] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 2330 & ivs$isco_partner <= 2332] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 2444] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 2446 & ivs$isco_partner <= 2450] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 2452 & ivs$isco_partner <= 2455] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 3200] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 3220 & ivs$isco_partner <= 3224] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 3226] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 3229 & ivs$isco_partner <= 3340] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 3460 & ivs$isco_partner <= 3472] <- 14
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 3480] <- 14

    # Skilled service (15)

    #ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 3225] <- 15
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 50 & d$isco_mainjob <= 52] <- 15
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 3473 & ivs$isco_partner <= 3475] <- 15
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 5000 & ivs$isco_partner <= 5113] <- 15
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 5122] <- 15
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 5131 & ivs$isco_partner <= 5132] <- 15
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 5140 & ivs$isco_partner <= 5141] <- 15
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 5143] <- 15
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 5160 & ivs$isco_partner <= 5220] <- 15
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 8323] <- 15

    # Low-skilled service (16)

    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 5120 & ivs$isco_partner <= 5121] <- 16
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 5123 & ivs$isco_partner <= 5130] <- 16
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner >= 5133 & ivs$isco_partner <= 5139] <- 16
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 5142] <- 16
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 5149] <- 16
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 5230] <- 16
    # ivs$class16_p[ivs$selfem_partner == 1 & ivs$isco_partner == 8322] <- 16
    d$class16[d$selfem_mainjob == 1 & d$isco_mainjob >= 90 & d$isco_mainjob <= 91] <- 16

  }

  }


  if (isco08==T) {


    if (any(isco[(!is.na(isco)) & isco != 10] %>% nchar == 4)) {


# Create Oesch class schema for respondents ISCO 08 ----


      d$class16_r <- -9

      # Large employers (1)

      d$class16_r[d$selfem_mainjob == 4] <- 1

      # Self-employed professionals (2)

      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2000 & d$isco_mainjob <= 2162] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2164 & d$isco_mainjob <= 2165] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2200 & d$isco_mainjob <= 2212] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob == 2250] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2261 & d$isco_mainjob <= 2262] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2300 & d$isco_mainjob <= 2330] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2350 & d$isco_mainjob <= 2352] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2359 & d$isco_mainjob <= 2432] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2500 & d$isco_mainjob <= 2619] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob == 2621] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2630 & d$isco_mainjob <= 2634] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2636 & d$isco_mainjob <= 2640] <- 2
      d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2642 & d$isco_mainjob <= 2643] <- 2

      # Small business owners with employees (3)

      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 1000 & d$isco_mainjob <= 1439] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2163] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2166] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2220 & d$isco_mainjob <= 2240] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2260] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2263 & d$isco_mainjob <= 2269] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2340 & d$isco_mainjob <= 2342] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2353 & d$isco_mainjob <= 2356] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2433 & d$isco_mainjob <= 2434] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2620] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2622] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2635] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2641] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2650 & d$isco_mainjob <= 2659] <- 3
      d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 3000 & d$isco_mainjob <= 9629] <- 3

      # Small business owners without employees (4)

      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 1000 & d$isco_mainjob <= 1439] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2163] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2166] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2220 & d$isco_mainjob <= 2240] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2260] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2263 & d$isco_mainjob <= 2269] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2340 & d$isco_mainjob <= 2342] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2353 & d$isco_mainjob <= 2356] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2433 & d$isco_mainjob <= 2434] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2620] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2622] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2635] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2641] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2650 & d$isco_mainjob <= 2659] <- 4
      d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 3000 & d$isco_mainjob <= 9629] <- 4

      # Technical experts (5)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2100 &  d$isco_mainjob <= 2162] <- 5
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2164 &  d$isco_mainjob <= 2165] <- 5
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2500 &  d$isco_mainjob <= 2529] <- 5

      # Technicians (6)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3100 &  d$isco_mainjob <= 3155] <- 6
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3210 &  d$isco_mainjob <= 3214] <- 6
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3252] <- 6
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3500 &  d$isco_mainjob <= 3522] <- 6

      # Skilled manual (7)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 6000 &  d$isco_mainjob <= 7549] <- 7
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8310 &  d$isco_mainjob <= 8312] <- 7
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8330] <- 7
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8332 &  d$isco_mainjob <= 8340] <- 7
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8342 &  d$isco_mainjob <= 8344] <- 7

      # Low-skilled manual (8)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8000 &  d$isco_mainjob <= 8300] <- 8
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8320 &  d$isco_mainjob <= 8321] <- 8
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8341] <- 8
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8350] <- 8
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9200 &  d$isco_mainjob <= 9334] <- 8
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9600 &  d$isco_mainjob <= 9620] <- 8
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9622 &  d$isco_mainjob <= 9629] <- 8

      # Higher-grade managers and administrators (9)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 1000 &  d$isco_mainjob <= 1300] <- 9
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 1320 &  d$isco_mainjob <= 1349] <- 9
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2400 &  d$isco_mainjob <= 2432] <- 9
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2610 &  d$isco_mainjob <= 2619] <- 9
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2631] <- 9
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 100 &  d$isco_mainjob <= 110] <- 9

      # Lower-grade managers and administrators (10)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 1310 &  d$isco_mainjob <= 1312] <- 10
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 1400 &  d$isco_mainjob <= 1439] <- 10
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2433 &  d$isco_mainjob <= 2434] <- 10
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3300 &  d$isco_mainjob <= 3339] <- 10
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3343] <- 10
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3350 &  d$isco_mainjob <= 3359] <- 10
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3411] <- 10
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5221] <- 10
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 200 &  d$isco_mainjob <= 210] <- 10

      # Skilled clerks (11)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3340 &  d$isco_mainjob <= 3342] <- 11
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3344] <- 11
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 4000 &  d$isco_mainjob <= 4131] <- 11
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 4200 &  d$isco_mainjob <= 4221] <- 11
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 4224 &  d$isco_mainjob <= 4413] <- 11
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 4415 &  d$isco_mainjob <= 4419] <- 11

      # Unskilled clerks (12)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 4132] <- 12
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 4222] <- 12
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 4223] <- 12
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5230] <- 12
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 9621] <- 12

      # Socio-cultural professionals (13)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2200 &  d$isco_mainjob <= 2212] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2250] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2261 &  d$isco_mainjob <= 2262] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2300 &  d$isco_mainjob <= 2330] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2350 &  d$isco_mainjob <= 2352] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2359] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2600] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2621] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2630] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2632 &  d$isco_mainjob <= 2634] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2636 &  d$isco_mainjob <= 2640] <- 13
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2642 &  d$isco_mainjob <= 2643] <- 13

      # Socio-cultural semi-professionals (14)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2163] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2166] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2220 &  d$isco_mainjob <= 2240] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2260] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2263 &  d$isco_mainjob <= 2269] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2340 &  d$isco_mainjob <= 2342] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2353 &  d$isco_mainjob <= 2356] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2620] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2622] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2635] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2641] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2650 &  d$isco_mainjob <= 2659] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3200] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3220 &  d$isco_mainjob <= 3230] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3250] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3253 &  d$isco_mainjob <= 3257] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3259] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3400 &  d$isco_mainjob <= 3410] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3412 &  d$isco_mainjob <= 3413] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3430 &  d$isco_mainjob <= 3433] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3435] <- 14
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 4414] <- 14

      # Skilled service (15)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3240] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3251] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3258] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3420 &  d$isco_mainjob <= 3423] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3434] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5000 &  d$isco_mainjob <= 5120] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5140 &  d$isco_mainjob <= 5142] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5163] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5165] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5200] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5220] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5222 &  d$isco_mainjob <= 5223] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5241 &  d$isco_mainjob <= 5242] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5300 &  d$isco_mainjob <= 5321] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5400 &  d$isco_mainjob <= 5413] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5419] <- 15
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8331] <- 15

      # Low-skilled service (16)

      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5130 &  d$isco_mainjob <= 5132] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5150 &  d$isco_mainjob <= 5162] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5164] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5169] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5210 &  d$isco_mainjob <= 5212] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5240] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5243 &  d$isco_mainjob <= 5249] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5322 &  d$isco_mainjob <= 5329] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5414] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8322] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9100 &  d$isco_mainjob <= 9129] <- 16
      d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9400 &  d$isco_mainjob <= 9520] <- 16


    } else {
      stop("\n\n Oesch currently available only for 4-digit ISCO 08.\n\n Try converding to ISCO88 using occupar::isco08to88()")
    }
  }





  ###################################
  # Construct classes and assign class labels
  ##################################


  class16_labels<- c("Large employers" = 1,
                     "Self-employed professionals" = 2,
                     "Small business owners with employees" = 3,
                     "Small business owners without employees" = 4,
                     "Technical experts" = 5,
                     "Technicians" = 6,
                     "Skilled manual" = 7,
                     "Low-skilled manual" = 8,
                     "Higher-grade managers and administrators" = 9,
                     "Lower-grade managers and administrators" = 10,
                     "Skilled clerks" = 11,
                     "Unskilled clerks" = 12,
                     "Socio-cultural professionals" = 13,
                     "Socio-cultural semi-professionals" = 14,
                     "Skilled service" = 15,
                     "Low-skilled service" = 16)

  class8_labels<-c("Self-employed professionals and large employers" = 1,
                   "Small business owners" = 2,
                   "Technical (semi-)professionals" = 3,
                   "Production workers" = 4,
                   "(Associate) managers" = 5,
                   "Clerks" = 6,
                   "Socio-cultural (semi-)professionals" = 7,
                   "Service workers" = 8)

  class5_labels <-c("Higher-grade service class" = 1,
                    "Lower-grade service class" = 2,
                    "Small business owners" = 3,
                    "Skilled workers" = 4,
                    "Unskilled workers" = 5)


  d$class16[d$class16 == -9] <- NA
  d$class16 <- sjlabelled::set_labels(d$class16,labels=class16_labels)
  sjlabelled::set_label(d$class16) <- "Respondent's Oesch class position - 16 classes"

  if(n.classes==16) {
    return(d$class16)
  } else if (n.classes==8) {

    d$class8 <- NA
    d$class8[d$class16 <= 2] <- 1
    d$class8[d$class16 == 3 | d$class16 == 4] <- 2
    d$class8[d$class16 == 5 | d$class16 == 6] <- 3
    d$class8[d$class16 == 7 | d$class16 == 8] <- 4
    d$class8[d$class16 == 9 | d$class16 == 10] <- 5
    d$class8[d$class16 == 11 | d$class16 == 12] <- 6
    d$class8[d$class16 == 13 | d$class16 == 14] <- 7
    d$class8[d$class16 == 15 | d$class16 == 16] <- 8
    d$class8 <- sjlabelled::set_labels(d$class8, labels=class8_labels)
    sjlabelled::set_label(d$class8) <- "Respondent's Oesch class position - 8 classes"


    return(d$class8)

  } else  {

    d$class5 <- NA
    d$class5[d$class16 <= 2 | d$class16 == 5 | d$class16 == 9 | d$class16 == 13] <- 1
    d$class5[d$class16 == 6 | d$class16 == 10 | d$class16 == 14] <- 2
    d$class5[d$class16 == 3 | d$class16 == 4] <- 3
    d$class5[d$class16 == 7 | d$class16 == 11 | d$class16 == 15] <- 4
    d$class5[d$class16 == 8 | d$class16 == 12 | d$class16 == 16] <- 5

    d$class5<- sjlabelled::set_labels(d$class5, labels=class5_labels)
    sjlabelled::set_label(d$class5) <- "Respondent's Oesch class position - 5 classes"

    return(d$class5)
  }


}
