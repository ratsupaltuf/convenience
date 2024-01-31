#' Simulate Individual Incomes within income brackets
#'
#' This function simulates individual incomes based on specified parameters,
#' including minimum income (`imin`), maximum income (`imax`), standard deviation
#' (`sigma`), and mean (`cons`).
#'
#' @param imin Minimum income value (default: euro_lo).
#' @param imax Maximum income value (default: euro_hi).
#' @param sigma Standard deviation of the income distribution (default: sigma).
#' @param cons Mean of the income distribution (default: cons).
#' @return Simulated individual income value.
#' @details The function generates random numbers and simulates individual incomes
#'          based on a normal distribution with the specified mean and standard
#'          deviation. It ensures that the simulated income falls within the
#'          specified range (defined by `imin` and `imax`). If `imax` is `NA`,
#'          the uppermost income bracket is simulated using a beta distribution.
#' @examples
#' # Income imputation (example from harmonization script)
#'
#' start_all <- Sys.time()
#' set.seed(1704)
#'
#' # # For interval regression: If upper limit highest income category is not
#' # # specified, set NA
#' df1[categorical==1 & upper==-9999, upper:=NA]
#' df1[categorical==1,interval:=paste(lower, upper, sep="-")]
#' df1[categorical==1,lower_log:=log(lower)]
#' df1[categorical==1,upper_log:=log(upper)]
#' df1[categorical==1,interval_log:=paste(lower_log, upper_log, sep="-")]
#' df1[categorical==1,cround:=paste0(iso3c, year)] #cntry-year identifier
#' df1[categorical==1, rowpos:= .I] # create variable containing rownumber
#' # First, get regression coefficients (constant and sigma) by country and year, store as variables
#' # in data.table
#' start_time_regress <- Sys.time()
#' for (cy in unique(df1$cround)) {
#'   dat <- df1[cround==cy]
#'   mi <- dat[["lower_log"]]
#'   ma <- dat[["upper_log"]]
#'   Y <- Surv(mi, ma, type = "interval2")
#'   m <- survreg(Y ~ 1,data=dat, dist = "gaussian", na.action = "na.exclude")
#'   df1[cround==cy, cons:= as.numeric(m$icoef["(Intercept)"])]
#'   df1[cround==cy, sigma := exp(as.numeric(m$icoef["Log(scale)"]))]
#'   df1[cround==cy, iter:= as.numeric(m$iter)]
#' }
#' end_time_regress <- Sys.time()
#' end_time_regress-start_time_regress
#' rm(dat, m, ma, mi, Y, cy)
#' #Check which did not converge:
#' df1[iter>=11, .(unique(iso3c), unique(year))]
#' #cat[cntry=="UA" & essround==2, table(income, useNA = "ifany")]
#' descr(df1$iter)
#' df1[, iter:= NULL]
#'
#' # Impute incomes
#' start<- Sys.time()
#' df1[categorical==1 & !is.na(income_values),
#'     inc_imp := impinc(
#'       imin = lower_log,
#'       imax = upper_log,
#'       sigma = sigma,
#'       cons = cons
#'     ), by = rowpos]
#' end<- Sys.time()
#' end-start
#'
#' df1[, ln_inc_imp:=inc_imp]
#' df1[, inc_imp:=exp(inc_imp)]
#'
#' # Checks: values outside of original income brackets?
#' df1[inc_imp <lower | inc_imp>upper, .N]
#' df1[inc_imp <lower | inc_imp>upper, .(income_values, interval, inc_imp)]
#'
#' descr(df1$inc_imp)
#' df1[inc_imp==max(df1$inc_imp, na.rm=T), .(num, lower, upper, interval, inc_imp)]
#'
#' #### Merge incomes
#'
#' df1[categorical==1, hhinc_imp:=inc_imp]
#' df1[categorical==0, hhinc_imp:=hhinc]
#' @export
impinc<- function(imin=euro_lo, imax=euro_hi, sigma=sigma, cons=cons) {
  x<-runif(1000)
  v <- qnorm(x, mean=cons, sd=sigma)
  e<-v[((imin < v) & (v <= imax) | ((is.na(imax)) & imin < v))]

  if (length(e)>0) {
    return(e[1])
  } else {
    x<-runif(100000)
    v <- qnorm(x, mean=cons, sd=sigma)
    e<-v[((imin < v) & (v <= imax) | ((is.na(imax)) & imin < v))]
  }
  if (length(e)>0) {
    return(e[1]) } else if (is.na(imax)) {
      # if imax is NA, it is the upper most income bracket, simulate beta
      # distribution for that
      upper_limit <- 2*imin
      lower_limit <- imin
      return(rbeta(1, shape1=2, shape2=17) * (upper_limit - lower_limit) + lower_limit)

    } else {
      v<-rnorm(1000) *(imax - imin) + imin
      e<-v[((imin < v) & (v <= imax) | ((is.na(imax)) & imin < v))]
      return(e[1])
    }

}
