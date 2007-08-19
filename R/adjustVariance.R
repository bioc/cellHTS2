##=============================================================
## adjust data variance
## Arguments:
## object - cellHTS object
## type - type of variance adjustment. 
##        Options are: 
##               - "byPlate" (per-plate variance scaling)
##               - "byBatch" (per batch of plates variance scaling) (default)
##               - "byExperiment" (per experiment variance scaling) 
## Variance adjustment is performed separately to each replicate and channel.


adjustVariance <- function(object, type="byBatch") { 

#  if(!(type %in% c("byPlate", "byBatch", "byExperiment"))) stop(sprintf("Undefined value %s for 'type'.", type))

  ## use the array stored in slot 'xnorm'
   xnorm <- normdata(object) 
   d <- dim(xnorm)
   nrWpP <- d[1]
   nrPlates <- d[2]
   nrReplicates <- d[3]

   samps <- (wellAnno(object)=="sample")


  if(type=="byBatch") {
  # adjust by the plate-wide mad

   nrBatches = max(object@batch) 
   plates = 1:nrPlates
   platesPerBatch = split(plates, object@batch)

   for(b in 1:nrBatches){
       pb <- platesPerBatch[[b]]
       spp <- samps[(1+nrWpP*(min(pb)-1)):(nrWpP*max(pb))]
       xnorm[,pb,,] <- apply(xnorm[,pb,,, drop=FALSE], 3:4, function(z) z/mad(z[spp], na.rm=TRUE))
   }

  } else {
    if(type=="byExperiment") {
      # adjust by the experiment-wide mad
      xnorm[] <- apply(xnorm, 3:4, function(z) z/mad(z[samps], na.rm=TRUE)) 
    } else {
  ## type="byPlate"
  ## adjust by the plate-wide mad
  for (p in 1:nrPlates) {
    spp <- samps[(1:nrWpP)+nrWpP*(p-1)]
    xnorm[,p,,] <- apply(xnorm[,p,,,drop=FALSE], 3:4, function(z) z/mad(z[spp], na.rm=TRUE))
  }
}
}

normdata(object) <- xnorm
return(object)
}