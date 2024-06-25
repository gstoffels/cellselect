#' Tool for selecting a transformation power
#'
#' Function that computes the Medcouple (MC), a robust measure of skewness,  for a range of power transformations.
#'
#' @param dframe Data frame with N rows and 4 variables (OBS_ID, x, y, MINMAX_CLUST)
#' @param xy_coord Use xy_coord = 2 for x-axis variable and xy_coord = 3 for  y-axis variable
#' @param pstart Minimum power
#' @param pstop Maximum power
#' @return Data frame of powers and corresponding MC values

#' @export

power.select <- function(dframe, xy_var,pstart,pstop) {

  istop=10*(pstop - pstart) # last iteration
  totiter=10*(pstop - pstart) +1 # number of iterations
  MC <- rep(NA,totiter) # initial medcouple (MC) vector with NAs

  # x-axis variable; values clustered at min/max are removed
  if (xy_var == 2) var_loop <- subset(dframe[,2],dframe[,4]==0)

  # y-axis variable; values clustered at min/max are removed
  else if (xy_var == 3) var_loop <- subset(dframe[,3],dframe[,4]==0)

  else stop("xy_coord not valid. Use 2 for x-coord and 3 for y-coord.")

  # MC value for power transformations from pstart to pstop increasing by 0.1
  for (i in 0:istop){
    xtrans <- var_loop^(pstart+i/10)
    MC[i+1] <- robustbase::mc(xtrans)
  }

  power <- seq(pstart,pstop,0.1) # grid of power values

  # data frame of power values and corresponding mc values
  as.data.frame(cbind(power,MC))

}
