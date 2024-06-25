#######################################################################################
## Secondary function that selects cells of interest in each sector and in center region

selection.regions <- function(df1,p.select,ntot) {

  df1.noclust <- subset(df1,MINMAX_CLUST==0) # subset of non-clustered observations

  n.noclust <- nrow(df1.noclust) # number of non-clustered observations

  n.select <- round(p.select*n.noclust) # number of non-clustered obs to select per region

  aggregate(df1.noclust$dist2origin,list(df1.noclust$sector),FUN=function(x) {
    n <- length(x)
    if ((n < n.select) && (sector!=0)){stop("Number of selected cells larger than number of cells in that region")}
  }
  )

  set.seed(1234)

  # ranks for dist2origin (non-clustered obs)
  rank.dist <- ave(df1$dist2origin,df1$MINMAX_CLUST,FUN=function(x) rank(x,ties.method="random"))

  # ranks for dist2origin by sector (non-clustered obs)
  rank.dist.sector <- ave(df1$dist2origin,df1$MINMAX_CLUST,df1$sector,FUN=function(x) rank(-x,ties.method="random"))

  df.temp <- cbind(df1,rank.dist,rank.dist.sector)

  ## Creating the variables selected and center
  sct <- NULL # sector
  clustered <- NULL # clustered
  rnk <- NULL # rank for distance to origin
  rnk.sector <- NULL # reversed rank for distance to origin by sector
  sector.sel <- rep(0,ntot) # initializing the sector selected indicator
  center.sel <- rep(0,ntot) # initializing the center selected indicator

  for (i in 1:ntot){

    sct <- df.temp[i,7] # sector
    clustered <- df.temp[i,4] # clustered
    rnk <- df.temp[i,9] # rank for distance to origin
    rnk.sector <- df.temp[i,10] # reversed rank for distance to origin by sector

    # selected cells that do not include gated obs
    if (sct !=0 && rnk.sector <= n.select && clustered==0) {sector.sel[i] = 1}

    if (rnk <= n.select && clustered==0) {
      sector.sel[i] = 0
      center.sel[i] = 1
    }
  }

  cbind(df.temp,sector.sel,center.sel)
}
