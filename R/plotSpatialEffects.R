## Ligia P. Bras (Sept 2006)

## Function that shows the row and column effects (calculated by the Bscore method or the spatial normalization methods) for a given range of plates ('plates'), and in a given channel ('channel').
## The spatial offsets within the selected channel 'channel' are transformed by subtracting their minimum value, and dividing by their amplitude (max - min values), in order to confine them to the range [0,1].


plotSpatialEffects = function(object, channel=1, plates) {

  if(!inherits(object, "cellHTS")) stop("'object' should be of class 'cellHTS'.")

  rowcol <- plateEffects(object)[["rowcol"]]
  d <- dim(Data(object))
  nrReplicates <- d[2]
  nrPlates <- max(plate(object))

 ## Check if rowcol.effects are not empty in the 'cellHTS' object
  if(length(rowcol)==0)
    stop("No information in slot 'rowcol.effects'! Please normalize 'object' using 'normalizePlates' with parameter 'method' = 'Bscore' or 'loess' or 'locfit', and 'save.model=TRUE'.")

  if(channel > dim(rowcol)[3])
    stop("Choose a correct channel number using argument 'channel'!")  

  if (missing(plates)) {
    plates = 1:nrPlates
  } else  {
    if(!is(plates, "vector") | !all(plates %in% 1:nrPlates))
     stop(sprintf("\n 'plates' should be a vector of integers between 1 and %s giving the ID number of the plates to display.", nrPlates))
  }

  myMax = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, max(x), as.numeric(NA))
  }
  myMin = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, min(x), as.numeric(NA))
  }

  nPlates = length(plates)

  pushViewport(viewport(layout = grid.layout(nrReplicates, nPlates))) 
  selx = array(rowcol[,,channel], dim=c(prod(pdim(object)), nrPlates, nrReplicates))
  # set range of sel to [0,1]
  selx = (selx-myMin(selx))/(myMax(selx)-myMin(selx))

  for (r in 1:nrReplicates) 
    for (p in 1:nPlates) {
      wp = plates[p]
  #xrange = range(aux, na.rm=TRUE) 
      sel = selx[,wp,r]
      pushViewport(viewport(layout.pos.row=r, layout.pos.col=p))
      plotPlate(as.numeric(t(sel)), nrow=pdim(object)["nrow"], ncol=pdim(object)["ncol"], na.action="xout",main=sprintf("Row + Column offsets, Plate %d, Replicate %d, Channel %s",wp, r, channel), col=rev(brewer.pal(9, "RdBu")), cex.main=0.8, cex.lab=1.1, add=TRUE, xrange=c(0,1))
      popViewport()
  } 
  popViewport()
}
