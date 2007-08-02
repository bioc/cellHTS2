## Per-plate normalization 
## modified by Ligia Bras, AUG 2007
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


normalizePlates = function(object, scale="additive", log = FALSE, method="median", varianceAdjust="byBatch", posControls, negControls,...) {

  ## Check the status of the 'cellHTS' object
  if(!object@state["configured"])
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
 