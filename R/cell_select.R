#' Select cells
#'
#' This function selects cells in regions of interest.
#'
#' @param data A data frame with N rows and 4 variables (OBS_ID, x, y, MINMAX_CLUST)
#' @param px Optimal power for transforming x-coordinate data
#' @param py Optimal power for transforming y-coordinate data
#' @param sect.size Sector size in degrees
#' @param p.select Percentage of non-gated cells to be selected in each region
#' @return A data frame with N rows and 9 variables (OBS_ID, x, y, MINMAX_CLUST, tx, ty, sector, sector.sel, center.sel)
#' @return tx: power transformed x
#' @return ty: power transformed y
#' @return sector: sector indicator (1 to 8)
#' @return sector.sel: selected within a sector (0=no, 1=yes)
#' @return center.sel: selected in the center region (0=no, 1=yes)

#' @export


################################################################
## MAIN FUNCTION that selects cells of interest

cell.select <- function(data,px,py,sect.size,p.select) {

  xtrans <- data[,2]^(px)
  ytrans <- data[,3]^(py)
  tx <- (xtrans - median(xtrans))/(0.74*IQR(xtrans))
  ty <- (ytrans - median(ytrans))/(0.74*IQR(ytrans))

  ntot<-length(data[,2])

  if (sect.size > 22.5) {stop("This angle is too large and will produce overlapping regions")}

  df0 <- cbind(data[,1:4],tx,ty) # data frame with added tx and ty

  df1 <- regions.marker(df0,ntot,sect.size) # dta frame augmented with sector and dist. to origin

  df2 <- selection.regions(df1,p.select,ntot) # dta frame augmented with sector.sel and center.sel

  subset(df2, select = -c(rank.dist, rank.dist.sector, dist2origin)) # reduced output dta frame

}

