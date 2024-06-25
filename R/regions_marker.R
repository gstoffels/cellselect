################################################################
## Secondary function that identifies regions of interest

regions.marker <- function(df0,ntot,sect.size) {

  deltarad=sect.size*pi/180 # sector size in radians
  diagrad=pi/4
  x <- NULL
  y <- NULL
  sector <- rep(NA,ntot)
  dist2origin <- rep(NA,ntot)

  for (i in 1:ntot){

    x_orig <- df0[i,2] # original x
    y_orig <- df0[i,3] # original y

    x <- df0[i,5] # tx
    y <- df0[i,6] # ty

    dist2origin[i] = sqrt(x^2 + y^2) # distance to origin

    if (y > x*tan(diagrad - deltarad) && y < x*tan(diagrad + deltarad)){ # sectors
      sector[i] = 2
    } # Northeast sector

    else if (y > -x*tan(diagrad - deltarad) &&
             y < -x*tan(diagrad + deltarad)){
      sector[i] = 4
    } # Northwest sector
    else if (y > x*tan(diagrad + deltarad) &&
             y < x*tan(diagrad - deltarad)){
      sector[i] = 6
    } # Southwest sector

    else if (y > -x*tan(diagrad + deltarad) &&
             y < -x*tan(diagrad - deltarad)){
      sector[i] = 8
    } # Southeast sector

    else if (y < x*tan(pi + deltarad) &&
             y > x*tan(pi - deltarad)){
      sector[i]= 1
    } # East sector

    else if  (y > -x*tan(pi - deltarad) &&
              y < -x*tan(pi + deltarad)){
      sector[i] = 5
    } # West sector

    else if ( (x > 0 && y > x*tan(2*diagrad - deltarad)) ||
              (x < 0 && y > -x*tan(2*diagrad - deltarad)) || (x==0 && y>0) ){
      sector[i] = 3
    } # North sector

    else if ((x > 0 && y < x*tan(2*diagrad + deltarad)) ||
             (x < 0 && y < -x*tan(2*diagrad + deltarad)) || (x==0 && y<0)){
      sector[i] = 7
    } # South sector

    else {
      sector[i] = 0
    } # all other regions
  }

  cbind(df0,sector,dist2origin) # data frame with 3 added vars

}
