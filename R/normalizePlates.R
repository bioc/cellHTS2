## ----------------------------------------------------------------------------
## Functions for data normalization and summarization
## ----------------------------------------------------------------------------

## All of these functions use the content of "assayData" slot of the NChannelSet-derived cellHTS object

## =============================================================================
## 	 	------- Per-plate normalization of raw data ---------
## modified by Ligia Bras, AUG 2007
## modified by Ligia Bras, NOV 2007
## 
## object - cellHTS instance
## scale - argument to define whether the data are in "additive" or "multiplicative" scale
## log - log transform the data? TRUE or FALSE . Note: cannot be TRUE if scale="additive".
## method - define the plate adjustment: "median", "mean", "shorth" (subtract or divide, depending on "scale" argument)
##                                       "POC", "negatives", "NPI"
##                                       "Bscore" (but without variance adjustment of the residuals. This can be done in a subsequent step)
## varianceAdjust - argument to specify which variance adjustment to perform. Options are: "byExperiment", ""byBatch" or "byPlate". 
## posControls- optional. Required if a controls-based normalization method is chosen. Defaults to "pos".
## negControls - optional. Required if a controls-based normalization method is chosen. Defaults to "neg"
##
## Function workflow: 
##   1. Log transformation (optional)
##   2. Plate adjustment using the chosen method
##   3. Variance adjustment (optional)
## =============================================================================

normalizePlates = function(object, scale="additive", log = FALSE, method="median", varianceAdjust="none", posControls, negControls,...) {

  if(!inherits(object, "cellHTS")) stop("'object' should be of class 'cellHTS'.")
  ## Check the status of the 'cellHTS' object
  if(!state(object)[["configured"]])
    stop("Please configure 'object' (using the function 'configure') before normalization.")

  ## Check the conformity between the scale of the data and the chosen preprocessing
  if(scale=="additive" & log)
    stop("For data on the 'additive' scale, please do not set 'log=TRUE'. ",
         "Please have a look at the documentation of the 'scale' and 'log' options ",
         "of the 'normalizePlates' function.") 

  if(!(varianceAdjust %in% c("none", "byPlate", "byBatch", "byExperiment"))) 
         stop(sprintf("Undefined value %s for 'varianceAdjust'.", varianceAdjust))

 ## Check consistency for posControls and negControls (if provided)
  nrChannel = length(ls(assayData(object)))

  if(!missing(posControls)) {
    ## check
    checkControls(posControls, nrChannel, "posControls")
  } else { 
    posControls <- as.vector(rep("^pos$", nrChannel))
  }

  if(!missing(negControls)){
    ## check
    checkControls(y=negControls, len=nrChannel, name="negControls")
  } else {
    negControls=as.vector(rep("^neg$", nrChannel))
  }

## 1. Log transformation: 
  oldRawData <- Data(object)
  if(log){
      Data(object) <- log2(oldRawData)
      scale = "additive"
  }

## 2. Plate-by-plate adjustment:
 allowedFunctions  = c("mean", "median", "shorth", "negatives", "POC", "NPI", "Bscore", "loess", "locfit")
# override assayData with the new data 
 object <- switch(method,
   "mean" = perPlateScaling(object, scale, method),
   "median" = perPlateScaling(object, scale, method),
   "shorth" = perPlateScaling(object, scale, method),
   "negatives" = perPlateScaling(object, scale, method, negControls),
   "POC" = controlsBasedNormalization(object, method, posControls, negControls),
   "NPI" = controlsBasedNormalization(object, method, posControls, negControls),
   "Bscore" = Bscore(object, ...),
   "loess" = spatialNormalization(object, model="loess", ...), 
   "locfit" = spatialNormalization(object, model="locfit", ...), 
   stop(sprintf("Invalid value '%s' for argument 'method'.\n Allowed values are: %s.", 
            method, paste(allowedFunctions, collapse=", ")))
  )

## 3. Variance adjustment (optional):
  if(varianceAdjust!="none") object <- adjustVariance(object, method=varianceAdjust)

  object@state[["normalized"]] = TRUE
  validObject(object)
  return(object)
}
 

## =============================================================================
## 		-------- Channel summarization  -------
## Function that combines plate-corrected intensities from dual-channel screens.
## Modified by LPB, AUG 2007
## Modified by LPB, NOV 2007 - plate normalization before 
## =============================================================================
summarizeChannels = function(object,
    fun = function(r1, r2, thresh) ifelse(r1>thresh, r2/r1, as.numeric(NA))) {
    #funargs = list(thresh=quantile(r1, probs=0.1, na.rm=TRUE)),

# 05.11.2007 - I've commented the next 2 lines, since we no longer require 'object' to contain per-plate corrected channels prior to channels summarization 
#  if(!state(object)[["normalized"]])
#    stop("Please normalize the data in each channel in a plate-by-plate basis using 'normalizePlates').")

  if(length(channelNames(object))!= 2)
    stop("Currently this function is implemented only for dual-channel data.")
 
  xnorm <- Data(object) 

  ## The argument 'fun' allows using different normalizations, and also to define
  ## the numerator/denominator for the ratio (i.e. R1/R2 or R2/R1)
  xnorm <- array(
     do.call("fun", list(r1=xnorm[,,1], r2=xnorm[,,2])),
            dim=c(dim(xnorm)[1:2], "Channels"=1))

  ## store the summarized data in 'assayData' slot:
   # 1) remove channel 2:
  chNames <- assayDataElementNames(object) 
  assayDataElement(object, chNames[2]) <- NULL
   # 2) replace the contents of the (single) remaining channel by the new summarized values:
  Data(object) <- xnorm
  ## just to ensure that the object will pass the validity checks:
  if(!is.null(batch(object))) {
    bb=batch(object) 
    ## see if it differs across channels. If so, reset it to an empty matrix given that we've lost one channel. Otherwise, just keep one channel.
    bbt=apply(bb, 1:2, function(i) length(unique(i)))
    if(any(bbt>1)) object@batch <- new("cellHTS")@batch else batch(object) <- bb[,,1, drop=FALSE]
  }
  validObject(object)
  return(object)
}
