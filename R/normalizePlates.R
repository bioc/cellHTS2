## ----------------------------------------------------------------------------
## Functions for data normalization and summarization
## ----------------------------------------------------------------------------

## =============================================================================
## 	 	------- Per-plate normalization of raw data ---------
## Uses the contents of slot 'xraw'
## modified by Ligia Bras, AUG 2007
## 
## 
## object - cellHTS instance
## scale - argument to define whether the data are in "additive" or "multiplicative" scale
## log - log transform the data? TRUE or FALSE . Note: cannot be TRUE if scale="additive".
## method - define the plate adjustment: "median", "mean", "shorth" (subtract or divide, depending on "scale" argument)
##                                       "POC", "negatives2, "NPI"
##                                       "Bscore" (but without variance adjustment of the residuals. This can be done in a subsequent step)
## varianceAdjust - argument to specify which variance adjustment to perform. Options are: "byExperiment", ""byBatch" or "byPlate". 
## posControls- optional. Required if a controls-based normalization method is chosen. Defaults to "pos".
## negControls - optional. Required if a controls-based normalization method is chosen. Defaults to "neg"
##
## Function workflow: 
##   1. Log transformation (if asked)
##   2. Plate adjustment using the chosen method
##   3. Variance adjustment 
## =============================================================================

normalizePlates = function(object, scale="additive", log = FALSE, method="median", varianceAdjust="byBatch", posControls, negControls,...) {

  if(!inherits(object, "cellHTS")) stop("'object' should be of class 'cellHTS'.")
  ## Check the status of the 'cellHTS' object
  if(!state(object)["configured"])
    stop("Please configure 'object' (using the function 'configure') before normalization.")

  ## Check the conformity between the scale of the data and the chosen preprocessing
  if(scale=="additive" & log) stop("Please set 'log' to FALSE, since data are in 'additive' scale!") 

  if(!(varianceAdjust %in% c("byPlate", "byBatch", "byExperiment"))) 
         stop(sprintf("Undefined value %s for 'varianceAdjust'.", varianceAdjust))

 ## Check consistency for posControls and negControls (if provided)
   nrChannel = dim(rawdata(object))[4]

  if(!missing(posControls)) {
    ## check
    if (!is(posControls, "vector") | length(posControls)!=nrChannel | mode(posControls)!="character") 
      stop(sprintf("'posControls' should be a vector of regular expressions with length %d", nrChannel))
  } else { 
    posControls <- as.vector(rep("^pos$", nrChannel))
  }

  if(!missing(negControls)) {
    ## check
    if (!is(negControls, "vector") | length(negControls)!=nrChannel | mode(negControls)!="character") 
      stop(sprintf("'negControls' should be a vector of regular expressions with length %d", nrChannel))
  } else {
    negControls=as.vector(rep("^neg$", nrChannel))
  }

## 1. Log transformation: 
  oldRawData <- rawdata(object)
  if(log){
      rawdata(object) <- log2(oldRawData)
      scale = "additive"
  }

## 2. Plate-by-plate adjustment:
 allowedFunctions  = c("mean", "median", "shorth", "negatives", "POC", "NPI", "Bscore", "loess", "locfit")
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
   stop(sprintf("Invalid value '%s' for argument 'method'.\n Allowed valures are: %s.", 
            method, paste(allowedFunctions, collapse=", ")))
  )

## 3. Variance adjustment:
## (applied to slot 'xnorm')
  object <- adjustVariance(object, type=varianceAdjust)

  rawdata(object) <- oldRawData

  object@state["normalized"] = TRUE
  validObject(object)
  return(object)
}
 

## =============================================================================
## 		-------- Channel summarization  -------
## Function that combines plate-corrected intensities from dual-channel screens.
## Modified by LPB, AUG 2007
## =============================================================================
summarizeChannels = function(object,
    fun = function(r1, r2, thresh) ifelse(r1>thresh, r2/r1, as.numeric(NA)),...) {
    #funargs = list(thresh=quantile(r1, probs=0.1, na.rm=TRUE)),

  if(!state(object)["normalized"])
    stop("Please normalize the data in each channel in a plate-by-plate basis using 'normalizePlates').")

  if(dim(normdata(object))[4] != 2)
    stop("Currently this function is implemented only for dual-channel data.")

  ## The argument 'fun' allows using different normalizations, and also to define
  ## the numerator/denominator for the ratio (i.e. R1/R2 or R2/R1)
  normdata(object) <- array(
     do.call("fun", list(r1=normdata(object)[,,,1], r2=normdata(object)[,,,2])),
#append(list(r1=object@xnorm[,,,1], r2=object@xnorm[,,,2]), funargs)),
            dim=c(dim(normdata(object))[1:3], 1))
  return(object)
}



## =============================================================================
##            ----- Per-plate normalization of xnorm data -----
## Per-plate normalization after channels summarization
## Ligia Bras, AUG 2007
## This function acts on xnorm slot, and calls normalizePlates.
## =============================================================================

normalizePlatesAgain = function(object, scale="additive", log = FALSE, method="median", varianceAdjust="byBatch", posControls, negControls,...) {

  if(!state(object)["normalized"])
    stop("Only raw data is availabe! This function should be called as a further normalization step for already pre-processed data! Please call 'normalizePlates' and 'summarizeChannels' first.")

oldraw <- rawdata(object)
# replace xraw by xnorm
rawdata(object)<-normdata(object)

# call 'normalizePlates' on object with 'xraw' replcaed by 'xnorm'
object <- normalizePlates(object, scale, log, method, varianceAdjust, ...)

# restore original raw data in 'xraw' slot
rawdata(object) <- oldraw
validObject(object)
return(object)
}

##================================================================================