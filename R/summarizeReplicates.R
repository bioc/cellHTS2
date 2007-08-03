## ======================================================================
## Replicates summarization
## ======================================================================
summarizeReplicates=function(object, zscore="+", summary="min") {

  if(!state(object)["normalized"])
    stop("Please normalize 'object' (using for example the function 'normalizePlates') before calling this function.")

  if(dim(normdata(object))[4]!=1)
    stop("Currently this function is implemented only for single-color data.")


  ## 1) Using "zscore" argument, determine the z-score for each
  ## replicate, so that the selected summary has the same meaning
  ## independently of the type of the assay:

  sg = switch(zscore,
    "+" = 1,
    "-" = -1,
    stop(sprintf("Invalid value '%s' for argument 'zscore'", zscore)))

  samps <- (wellAnno(object)=="sample")
  normdata(object)[] = apply(normdata(object), 3:4, function(v) sg*(v-median(v[samps], na.rm=TRUE))/mad(v[samps], na.rm=TRUE))

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
  mx <- getReplicatesMatrix(normdata(x), channel=1, na.rm=FALSE)
  scores(object) <- switch(summary,
    mean = rowMeans(mx, na.rm=TRUE),
    max  = apply(mx, 1, myMax),
    min  = apply(mx, 1, myMin),
    rms = apply(mx, 1, myRMS),
    closestToZero = apply(mx, 1, myClosestToZero),
    furthestFromZero = apply(mx, 1, myFurthestFromZero),
    stop(sprintf("Invalid value '%s' for argument 'summary'", summary)))

  object@state["scored"] = TRUE
  validObject(object)
  return(object)
}
