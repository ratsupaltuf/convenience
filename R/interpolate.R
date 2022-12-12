#' Interpolate between missing values within data.table
#'
#' @description This is a convenience function based on zoo's replace functions. It was constructed for use within grouped data.tables
#' @param var The variable for which missing values should be replaced
#' @param index The numeric index variable that has an underlying time dimension that will be used for the interpolation.
#' @param type A numeric value between 1 and 4, indicating the interpolation method
#' @param ... Further arguments passed to zoo functions, e.g., maxgap or xout. See the zoo functions for further arguments.
#'
#' @details
#' If type = 1, interpolation uses a natural spline function
#' If type = 2, interpolation uses a periodic spline function
#' If type = 3, interpolation uses an approximation function with rule 2 as argument, i.e. carrying extreme observations forward/backward.
#' If type = 4, interpolation uses last observation carried forward. Maximum gap (for example, in years) can be passed on with maxgap.
#' @return A vector of the same length as 'var' in which NA values have been replaced. Use in data.tables to recode directly by reference.
#' @export
#'
#' @seealso spline, na.spline, na.approx, na.locf
#' @examples
#' # with interpolate(), replace NA with last observation carried forward, maximum 4 years before
#' agg<-issp[, .("Nissp"=.N), by=c("iso3c", "year")]
#' swiid<- merge.data.table(agg, swiid[, .(iso3c, year, gini_disp, gini_mkt, rel_red)], by=c("iso3c", "year"), all=T)
#' swiid[!is.na(Nissp) & is.na(gini_disp), .(iso3c, year)]
#' swiid[, gini_original:=gini_disp]
#' swiid[, gini_disp:=interpolate(gini_disp, year, 4, maxgap=4), by=iso3c]
#' swiid[, gini_mkt:=interpolate(gini_mkt, year, 4, maxgap=4), by=iso3c]
#' issp <- merge.data.table(issp, swiid[, .(iso3c, year, gini_disp, gini_mkt, rel_red)], by=c("iso3c", "year"), all.x=T)

interpolate<-function(var, index, type=3, ...) {
  z<- zoo::zoo(var, order.by = index)
  if(type==1) {
    # natural spline
    nat<- zoo::na.spline(z, na.rm=F, method = c("natural"))
    nat <- as.numeric(nat)
    return(nat) } else if(type==2) {
      # periodic spline
      return(as.numeric(zoo::na.spline(z, na.rm=F, method = c("periodic")))
      ) } else if(type==3) {
        # linear approximation, extremes carried forward
        return(as.numeric(zoo::na.approx(z, rule=2) ))
      } else if(type==4) {
        # last observation carried forward
        return(as.numeric(zoo::na.locf(z) ))
      }
}
