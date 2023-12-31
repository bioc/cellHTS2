scoreReplicates <- function(object, sign="+", method="zscore", ...)
{
    methodArgs <- list(...)
    ## 1) Score each replicate using the selected method:
    xnorm <- if(method=="none") Data(object) else do.call(paste("scoreReplicates", method, sep="By"),
                args=c(list(object), methodArgs))
    ## Store the scores in 'assayData' slot.

    ## 2) Use "sign" to make the meaning of the replicates summarization
    ## independent of the type of the assay
    sg <- switch(sign,
                 "+" = 1,
                 "-" = -1,
                 stop(sprintf("Invalid value '%s' for argument 'sign'", sign)))

    Data(object) <- sg*xnorm
    validObject(object)
    object@processingInfo[["scored"]] <- method
    return(object)
}

scoreReplicatesByzscore <- function(object)
{
    xnorm <- Data(object)
    samps <- (wellAnno(object)=="sample")
    xnorm[] <- apply(xnorm, 2:3, function(v) (v-median(v[samps], na.rm=TRUE))/mad(v[samps], na.rm=TRUE))
    return(xnorm)
}

scoreReplicatesByNPI <- function(object, posControls, negControls)
{
    xnorm <- Data(object)
    d <- dim(xnorm)
    nrSamples <- d[2]
    nrChannels <- d[3]
    wAnno <- as.character(wellAnno(object))

    ## Check consistency for posControls and negControls (if provided)
    if(!missing(posControls))
    {
        checkControls(posControls, nrChannels, "posControls")
    }
    else
    {
        posControls <- as.vector(rep("^pos$", nrChannels))
    }

    if(!missing(negControls))
    {
        checkControls(y=negControls, len=nrChannels, name="negControls")
    }
    else
    {
        negControls=as.vector(rep("^neg$", nrChannels))
    }

    for(ch in 1:nrChannels)
    {
        if(!(emptyOrNA(posControls[ch]))) pos <- findControls(posControls[ch], wAnno) else pos <- integer(0)
        if(!(emptyOrNA(negControls[ch]))) neg <- findControls(negControls[ch], wAnno)  else neg <- integer(0)
        if (!length(pos) | !length(neg))
            stop(sprintf("No positive or/and negative controls were found in channel %d! Please use a ",
                         "different normalization function.", ch))

	for(r in 1:nrSamples)
            if(!all(is.na(xnorm[, r, ch])) )
            {
                if(all(is.na(xnorm[pos,r,ch])) | all(is.na(xnorm[neg,r,ch])))
                    stop(sprintf(paste("No values for positive or/and negative controls were found in",
                                       "replicate %d, channel %d! Please use a different normalization function."),
                                 replicate, channel))

            xnorm[,r,ch] <- (mean(xnorm[pos,r,ch], na.rm=TRUE) - xnorm[,r,ch])/(mean(xnorm[pos,r,ch], na.rm=TRUE) -
                                                   mean(xnorm[neg, r, ch], na.rm=TRUE))
            }
    }

    return(xnorm)
}



## ======================================================================
## Replicates summarization
## ======================================================================
summarizeReplicates <- function(object, summary="min")
{
    ## Summarize between scored replicates:
    ## we need these wrappers because the behavior of max(x, na.rm=TRUE) if all
    ## elements of x are NA is to return -Inf, which is not what we want.
    myMax <- function(x)
    {
        x <- x[!is.na(x)]
        ifelse(length(x)>=1, max(x), as.numeric(NA))
    }

    myMin <- function(x)
    {
        x <- x[!is.na(x)]
        ifelse(length(x)>=1, min(x), as.numeric(NA))
    }

    myFurthestFromZero <- function(x)
    {
        x <- x[!is.na(x)]
        ifelse(length(x)>=1, x[abs(x)==max(abs(x))][1], as.numeric(NA))
    }

    myClosestToZero <- function(x)
    {
        x <- x[!is.na(x)]
        ifelse(length(x)>=1, x[abs(x)==min(abs(x))][1], as.numeric(NA))
    }

    ## Root mean square: square root of the mean squared value of the replicates
    myRMS <- function(x) {
        x <- x[!is.na(x)]
        ifelse(length(x)>=1, sqrt(sum(x^2)/length(x)), as.numeric(NA))
    }

    ## Summarize between replicates:
    xnorm <- Data(object)

    if(dim(xnorm)[2]>1)
    { # we don't need to do anything in case we don't have replicates!

         score <- switch(summary,
                        mean = if(dim(xnorm)[3]==1) rowMeans(xnorm[,,1], na.rm=TRUE) else apply(xnorm, c(1,3), mean, na.rm=TRUE),
                        median = if(dim(xnorm)[3]==1) rowMedians(xnorm[,,1], na.rm=TRUE) else apply(xnorm, c(1,3), median, na.rm=TRUE),
                        median = apply(xnorm, c(1,3), median, na.rm=TRUE),
                        max  = apply(xnorm, c(1,3), myMax),
                        min  = apply(xnorm, c(1,3), myMin),
                        rms = apply(xnorm, c(1,3), myRMS),
                        closestToZero = apply(xnorm, c(1,3), myClosestToZero),
                        furthestFromZero = apply(xnorm, c(1,3), myFurthestFromZero),
                        stop(sprintf("Invalid value '%s' for argument 'summary'", summary)))
         # ensure proper dimensionality of score
         dim(score)=c(dim(xnorm)[1], Channels=dim(xnorm)[3])
	 dimnames(score)=list(Features=featureNames(object), Channels=channelNames(object))
         ## Store the scores in 'assayData' slot. Since now there's a single sample (replicate) we
         ## need to construct a new cellHTS object.
         z <- object[, 1] # construct a cellHTS object just with the first sample
         matrices = sapply(channelNames(object), function(nm) matrix(score[,nm], dimnames=list(featureNames(object), 1)), simplify=FALSE)
         assayData(z) = do.call(assayDataNew, matrices)
    }
    else
    {
        z <- object
    }
    ## NB - the state "scored" of the cellHTS object is only changed to TRUE after data scoring
    ## and replicate summarization.
    z@state[["scored"]] <- TRUE
    z@processingInfo[["summarized"]] <-  summary
    validObject(z)
    return(z)
}



##------------------------------------------------
## Sigmoidal transformation of the z-score values
##------------------------------------------------
## Function that applies a sigmoidal transformation with parameters z0 and lambda to the z-score values
## stored in a cellHTS object. The obtained results are called 'calls'. The transformation is given by:
##	1/(1+exp(-(z-z0)*lambda))
## This maps the z-score values to the interval [0,1], and is intended to expand the scale of z-scores
## with intermediate values and shrink the ones showing extreme values, therefore making the difference
## between intermediate phenotypes larger.

## x - scored cellHTS object
## z0 - centre of the sigmoidal transformation
## lambda - parameter that controls the smoothness of the transition from low values
## to higher values (the higher this value, more steeper is this transition). Should be > 0 (but usually
## it makes more sense to use a value >=1)

scores2calls <- function(x, z0, lambda)
{
    ## check whether 'x' contains scored values:
    if(!state(x)[["scored"]])
        stop(sprintf(paste("'x' should be a 'cellHTS' object containing scored data!\nPlease check",
                     "its preprocessing state: %s"), paste(names(state(x)), "=", state(x), collapse=", ")))

    if(dim(Data(x))[3]!=1)
        stop("Currently this function is implemented only for single-color data.")

    if(!all(is.numeric(lambda) & is.numeric(z0)))
        stop("'z0' and 'lambda' should be numeric values.")

    if(!all(length(lambda)==1L & length(z0)==1L))
        stop("'z0' and 'lambda' should be numeric values of length one.")

    if(lambda <=0)
        stop("'lambda' should be a positive value!")


    trsf <- function(z) 1/(1+exp(-(z-z0)*lambda))
    z <- trsf(Data(x))
    assayData(x) <- assayDataNew("call"=matrix(z, dimnames=list(featureNames(x), 1)))
    return(x)
}



