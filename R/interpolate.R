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
