## Ligia Brás (September 2006)

## Function that shows the row and column effects (calculated by the Bscore method or the spatial normalization methods) for a given range of plates ('plates'), and in a given channel ('channel').
## The spatial offsets within the selected channel 'channel' are transformed by subtracting their minimum value, and dividing by their amplitude (max - min values), in order to confine them to the range [-1,1].


plotSpatialEffects = function(object, channel=1, plates) {

  if(!inherits(object, "cellHTS")) stop("'object' should be of class 'cellHTS'.")

 ## Check if rowcol.effects are not empty in the 'cellHTS' object
  if(all(object@rowcol.effects==0))
    stop("No information in slot 'rowcol.effects'! Please normalize 'object' using 'normalizePlates' with parameter 'method' = 'Bscore' or 'loess' or 'locfit', and 'save.model=TRUE'.")

  if(channel > dim(object@rowcol.effects)[4])
    stop("Choose a correct channel number using 'channel'!")  

  if (missing(plates)) {
    plates = 1:dim(object@xraw)[2]
  } else  {
    if(!is(plates, "vector") | !all(plates %in% 1:dim(object@xraw)[2]))
     stop(sprintf("\n 'plates' should be a vector of integers between 1 and %s \n
     giving the ID number of the plates to display", dim(object@xraw)[2]))
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

  pushViewport(viewport(layout = grid.layout(dim(object@xraw)[3], nPlates))) 
  selx = object@rowcol.effects[,,,channel]
  # set range of sel to [-1,1]
  selx = (selx-myMin(selx))/(myMax(selx)-myMin(selx))

  for (r in 1:dim(object@xraw)[3]) 
    for (p in 1:nPlates) {
      wp = plates[p]
  #xrange = range(aux, na.rm=TRUE) 
      sel = selx[,wp,r]
      pushViewport(viewport(layout.pos.row=r, layout.pos.col=p))
      plotPlate(as.numeric(t(sel)), nrow=object@pdim["nrow"], ncol=object@pdim["ncol"], na.action="xout",main=sprintf("Row + Column offsets, Plate %d, Replicate %d, Channel %s",wp, r, channel), col=rev(brewer.pal(9, "RdBu")), cex.main=0.8, cex.lab=1.1, add=TRUE) #,  xrange=xrange)
      popViewport()
  } 
  popViewport()
}
