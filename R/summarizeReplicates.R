## ======================================================================
## Replicates summarization
## ======================================================================
summarizeReplicates = function(object, zscore="+", summary="min") {

  if(!state(object)[["normalized"]])
    stop("Please normalize 'object' (using for example the function 'normalizePlates') before calling this function.")

  if(dim(Data(object))[3]!=1)
    stop("Currently this function is implemented only for single-color data.")

  ## 1) Using "zscore" argument, determine the z-score for each
  ## replicate, so that the selected summary has the same meaning
  ## independently of the type of the assay:

  sg = switch(zscore,
    "+" = 1,
    "-" = -1,
    stop(sprintf("Invalid value '%s' for argument 'zscore'", zscore)))

  samps <- (wellAnno(object)=="sample")
  xnorm <- Data(object)
  xnorm[] = apply(xnorm, 2:3, function(v) sg*(v-median(v[samps], na.rm=TRUE))/mad(v[samps], na.rm=TRUE))


  ## 2) Summarize between scored replicates:
  ## we need these wrappers because the behavior of max(x, na.rm=TRUE) if all
  ##   elements of x are NA is to return -Inf, which is not what we want.
  myMax = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, max(x), as.numeric(NA))
  }
  myMin = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, min(x), as.numeric(NA))
  }

  myFurthestFromZero = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, x[abs(x)==max(abs(x))][1], as.numeric(NA))
  }

  myClosestToZero = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, x[abs(x)==min(abs(x))][1], as.numeric(NA))
  }

  ## Root mean square: square root of the mean squared value of the replicates
  myRMS = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, sqrt(sum(x^2)/length(x)), as.numeric(NA))
  }


  ## 2) Summarize between replicates:
  mx <- xnorm[,,1] 
  if(dim(xnorm)[2]==1) mx = matrix(mx)

  score <- switch(summary,
    mean = rowMeans(mx, na.rm=TRUE),
    max  = apply(mx, 1, myMax),
    min  = apply(mx, 1, myMin),
    rms = apply(mx, 1, myRMS),
    closestToZero = apply(mx, 1, myClosestToZero),
    furthestFromZero = apply(mx, 1, myFurthestFromZero),
    stop(sprintf("Invalid value '%s' for argument 'summary'", summary)))


  ## Store the scores in 'assayData' slot. Since now there's a single sample (replicate) we need to construct a new cellHTS object.
  z <- object[, 1] # construct a cellHTS object just with the first sample
  assayData(z) <- assayDataNew("score"=matrix(score, dimnames=list(featureNames(object), 1)))

#   z <- new("cellHTS", 
#     assayData = assayDataNew("score"=matrix(score)),
#     phenoData = phenoData(object)[1],
#     featureData = featureData(object),
#     state = state(object), 
#     experimentData = object@experimentData,
#     plateList = object@plateList,
#     intensityFiles = object@intensityFiles,
#     plateConf = object@plateConf,
#     screenLog = object@screenLog,
#     screenDesc = object@screenDesc,
#     annotation = object@annotation
#   )


  ## batch slot: see if the batch differs across samples. If so, reset it to an empty array since now we only have one sample. Otherwise just keep one sample.

  ## just to ensure that the object will pass the validity checks:
  if(!is.null(batch(object))) {
    bb=batch(object) 
    ## see if it differs across samples:
    bbt=apply(bb, c(1,3), function(i) length(unique(i)))
    z@batch <- if(any(bbt>1))  new("cellHTS")@batch else bb[,1,1, drop=FALSE]
  }

  z@state[["scored"]] <- TRUE
  validObject(z)
  return(z)
}


##------------------------------------------------
## Sigmoidal transformation of the z-score values
##------------------------------------------------

## Function that applies a sigmoidal transformation with parameters z0 and lambda to the z-score values stored in a cellHTS object. The obtained results are called 'calls'. The transformation is given by:
##	1/(1+exp(-(z-z0)*lambda))
## This maps the z-score values to the interval [0,1], and is intended to expand the scale of z-scores with intermediate values and shrink the ones showing extreme values, therefore making the difference between intermediate phenotypes larger.


## x - scored cellHTS object
## z0 - centre of the sigmoidal transformation
## lambda - parameter that controls the smoothness of the transition from low values 
## to higher values (the higher this value, more steeper is this transition). Should be > 0 (but usually it makes more sense to use a value >=1)

scores2calls <- function(x, z0, lambda){

 ## check whether 'x' contains scored values:
   if(!state(x)[["scored"]]) stop(sprintf("'x' should be a 'cellHTS' object containing scored data!\nPlease check its preprocessing state: %s", paste(names(state(x)), "=", state(x), collapse=", ")))

  if(dim(Data(x))[3]!=1)
    stop("Currently this function is implemented only for single-color data.")

  if(!all(is.numeric(lambda) & is.numeric(z0))) stop("'z0' and 'lambda' should be numeric values.")

  if(!all(length(lambda)==1L & length(z0)==1L)) stop("'z0' and 'lambda' should be numeric values of length one.")

  if(lambda <=0) stop("'lambda' should be a positive value!")


  trsf = function(z) 1/(1+exp(-(z-z0)*lambda))
  z <- trsf(Data(x))
  assayData(x) <- assayDataNew("call"=matrix(z, dimnames=list(featureNames(x), 1)))
  return(x)
}



