## Functions for plate-by-plate normalization:
## perPlateScaling
## controlsBasedNormalization

## ===========================================================
## Auxiliary functions

funOperator=function(a,b, op) {
op = get(op, mode="function")
op(a,b)
}

POC = function(a, pos,...){
100*a/mean(a[pos], na.rm=TRUE)
}

NPI = function(a, pos, neg){
(mean(a[pos], na.rm=TRUE) - a)/(mean(a[pos], na.rm=TRUE) - mean(a[neg], na.rm=TRUE))
}
## ===========================================================

## 		----	perPlateScaling ------
##
## For each plate, calculates the ratio between each measurement and
## a certain plate intensity statistic (e.g. for scaleByPlateMean,
## the average intensity over the wells containing 'sample' or 'negative' controls).
##
## if data are in "additive" scale
## subtract by the per-plate correction factor
## instead of dividing by it.
##
## stats - character of length 1 giving the name of the plate intensity statistic to calculate. Options are: "mean", "median", "shorth" and "negatives"

perPlateScaling <- function(object, scale, stats="median", negControls){

  if(stats %in% c("mean", "median", "negatives")) funArgs = list(na.rm=TRUE) else funArgs = list(na.rm=TRUE, tie.action="min")

  if(scale=="additive") op="-" else op="/"

  if(stats=="negatives") statFun <- median else statFun <- get(stats, mode="function")

  normdata(object) <- rawdata(object)

  nrWpP <- dim(rawdata(object))[1]
  nrPlates <- dim(rawdata(object))[2]
  nrReplicates <- dim(rawdata(object))[3]
  nrChannels <- dim(rawdata(object))[4]

  for(p in 1:nrPlates) {
    wAnno = as.character(wellAnno(object)[(1:nrWpP)+nrWpP*(p-1)])
    inds <- (wAnno=="sample")
    for(ch in 1:nrChannels){
        if(stats %in% "negatives"){
          if (!(negControls[ch] %in% c(NA, ""))) inds = regexpr(negControls[ch], wAnno, perl=TRUE)>0 else inds=FALSE
          if((stats %in% c("negatives", "NPI")) & !sum(inds)) stop(sprintf("No negative controls were found in plate %s, channel %d! Please, use a different plate normalization method.", p, ch))
        }

        for(r in 1:nrReplicates) normdata(object)[,p,r,ch] = funOperator(rawdata(object)[, p, r, ch], do.call(statFun, c(list(x=rawdata(object)[inds, p, r, ch]), funArgs)), op)
        }
       }
  return(object)
}


## =========================================================================
##               ----- controlsBasedNormalization -----
## 
## General function that allows the following controls-based plate normalizations:
##       'POC' - Percent of control - determines the percentage of control, as the ratio between the 
## raw measurement and the mean of the measurements in the positive controls in an antagonist assay.
##
##       'NPI' - Normalized Percent Inhibition: for each plate, subtracts each measurement from the mean of the positive controls, and divides the result by the difference between the mean of positive and negative controls (plate dynamic range), in an antagonist assay.

controlsBasedNormalization <- function(object, method, posControls, negControls){

  normdata(object) <- rawdata(object)

  d <- dim(rawdata(object))
  nrWpP <- d[1]
  nrPlates <- d[2]
  nrReplicates <- d[3]
  nrChannels <- d[4]

  fun <- get(method, mode="function")

  for(p in 1:nrPlates) {
    wAnno <- as.character(wellAnno(object)[(1:nrWpP)+nrWpP*(p-1)])

      for(ch in 1:nrChannels) {
        if (!(posControls[ch] %in% c(NA, ""))) pos <- regexpr(posControls[ch], wAnno, perl=TRUE)>0  else pos <- FALSE
        if (!(negControls[ch] %in% c(NA, ""))) neg <- regexpr(negControls[ch], wAnno, perl=TRUE)>0  else neg <- FALSE
        if (method == "POC") {
           if (!sum(pos)) stop(sprintf("No positive controls were found in plate %s, channel %d! Please, use a different plate normalization method.", p, ch)) 
        } else {
	   if (sum(pos)==0 | sum(neg)==0) stop(sprintf("No positive or/and negative controls were found in plate %s, channel %d! Please, use a different normalization function.", p, ch))
        }

	for(r in 1:nrReplicates)
        normdata(object)[, p, r, ch] = fun(rawdata(object)[, p, r, ch], pos, neg)
     } 
  }
  return(object)
}


##-------- Bscore method (without plate variance adjustment) --------------------------
## B score: The residual (rijp) of the measurement for row i and column j on the p-th plate is obtained
## by fitting a two-way median polish:

## rijp = yijp - yijp_hat = yijp - (miu_hat + Rip_hat + Cjp_hat)

## For each plate p, the adjusted MADp is obtained from the rijp's.
## The variance adjustment step (Bscore = rijp/MADp) is omitted.


Bscore <- function(object, save.model=FALSE) {
  if(!inherits(object, "cellHTS")) stop("'object' should be of class 'cellHTS'.")
## acts on slot 'xraw'
  normdata(object) <- rawdata(object)
  xdat <- normdata(object)

  if(save.model) {
     object@rowcol.effects <- array(as.numeric(NA), dim=dim(xdat))
     object@overall.effects <- array(as.numeric(NA), dim=c(1, dim(xdat)[2:4]))
  }


  d <- dim(rawdata(object))
  nrWpP <- d[1]
  nrPlates <- d[2]
  nrReplicates <- d[3]
  nrChannels <- d[4]

  for(p in 1:nrPlates) {
    # use only sample wells for the fit:
    samples = (wellAnno(object)[(1:nrWpP)+nrWpP*(p-1)]=="sample")

    for(r in 1:nrReplicates)
      for(ch in 1:nrChannels) {
#       y must be a numeric matrix with "plate rows" in rows and "plate columns" in columns:
        y <- ysamp <- xdat[, p, r, ch]
        if(!all(is.na(y))) {
        ysamp[!samples]=NA
        ysamp <- matrix(ysamp,
            ncol=object@pdim["ncol"], nrow=object@pdim["nrow"], byrow=TRUE)
        y = matrix(y,
            ncol=object@pdim["ncol"], nrow=object@pdim["nrow"], byrow=TRUE)
        m = medpolish(ysamp, eps = 1e-5, maxiter = 200, trace.iter=!TRUE, na.rm = TRUE)

## apply the model to all the plate wells and obtain the residuals rijp
## replace NA by zero:
  isNArow = is.na(m$row)
  isNAcol = is.na(m$col)
  isNA = outer(isNArow, isNAcol, "*")
  m$row[isNArow]=0
  m$col[isNAcol]=0
  rowcol = outer(m$row, m$col, "+")

  res = y - (m$overall + rowcol) 

# if the effect is NA in both column and row elements, restore the NA value:
  if (sum(isNA)) rowcol[as.logical(isNA)] = NA
    #res is a matrix plate row * plate column
  normdata(object)[, p, r, ch] <- as.vector(t(res))

  if (save.model) {
      object@rowcol.effects[,p,r,ch] <- as.vector(t(rowcol))
      #residuals[,p,r,ch] = as.vector(t(m$residuals)) ## DON'T USE m$residuals, otherwise we'll have more NA 
      object@overall.effects[,p,r,ch]<-m$overall
   }
  } 
} 
}

object@state["normalized"] = TRUE
validObject(object)
return(object)
}


##                 ------- spatialNormalization ---------
##
## Fit a polynomial surface within each plate to the plate corrected intensities using local fit.
## uses a second degree polynomial (local quadratic fit)
##
## Inputs:
##### x -  cellHTS object
##### model  - fit the polynomial surface using robust "locfit" or "loess". The default is "locfit".
##### smoothPar - the parameter which controls the degree of smoothing (corresponds to 'span' argument of loess, or to the parameter 'nn' of 'lp' of locfit). The default is smoothPar = 0.6
##### save.model - should the fitted values be saved? Default=FALSE. If TRUE, the fitted values are stored in the slot 'rowcol.effects'. 
## -------------------------------------------------------------

spatialNormalization <- function(object, model="locfit", smoothPar=0.6, save.model=FALSE){
   
  if(!inherits(object, "cellHTS")) stop("'object' should be of 'cellHTS' class.")


  ## acts on slot 'xraw'
  normdata(object) <- rawdata(object)

  if (model=="locfit") require("locfit")


  d <- dim(rawdata(object))
  nrWpP <- d[1]
  nrPlates <- d[2]
  nrReplicates <- d[3]
  nrChannels <- d[4]

  rowcol.effects <- array(as.numeric(NA), dim=dim(rawdata(object)))
  position <- 1:prod(object@pdim)

## Map the position in the plates into a (x,y) coordinates of a cartesian system having its origin at the centre of the plate
  row <- 1 +(position-1) %/% object@pdim["ncol"]
  col <- 1 + (position-1) %% object@pdim["ncol"]
  centre <- 0.5 + c(object@pdim["ncol"]/2, object@pdim["nrow"]/2) 
  xpos <- col - centre[1]
  ypos <- centre[2] - row

  for(p in 1:nrPlates) {
    # use only sample wells for the fit:
    samples <- (wellAnno(object)[(1:nrWpP)+nrWpP*(p-1)]=="sample")

    for(r in 1:nrReplicates)
      for(ch in 1:nrChannels){
        y <- ysamp <- rawdata(object)[, p, r, ch]
        if(!all(is.na(y))) {
          ysamp[!samples] <- NA
          y <- yf <- data.frame(y=y, xpos=xpos, ypos=ypos)
          yf$xpos <- factor(xpos)
          yf$ypos <- factor(ypos)
          ysamp <- yfsamp <- data.frame(y=ysamp, xpos=xpos, ypos=ypos)
          yfsamp$xpos <- factor(xpos)
          yfsamp$ypos <- factor(ypos)

          ## Correct for spatial effects inside the plate using robust local fit
          posNAs <- is.na(ysamp$y)
          ysamp_complete <- ysamp[!posNAs,]
          yfsamp_complete <- yfsamp[!posNAs,]
          m = switch(model,
            "loess" = loess(y ~ xpos + ypos, data=ysamp_complete, normalize=TRUE, span=smoothPar,
                          control = loess.control(iterations=40)),
            "locfit" = locfit(y ~ lp(xpos, ypos, nn=smoothPar, scale=TRUE),
                          data = yfsamp_complete, lfproc=locfit.robust),
            stop(sprintf("Invalid value '%s' for argument 'model'", model))
          )

        # apply the model to all the plate wells and obtain the residuals rijp
        predx <- switch(model,
          "loess" <- predict(m, newdata=y),
          "locfit" <- predict(m, newdata=yf))

        #replace predicted NA by 0 to avoid having extra NA entries in xn:
        isNA <- is.na(predx)
        predx[isNA] <- 0  # safe, because we are going to perform a subtraction
        normdata(object)[,p,r,ch] <- y$y - predx
        # put back to NAs
        predx[isNA] <- NA  

        rowcol.effects[,p,r,ch] = predx
        }#if all is.na

        }#for channel
    }#for replicate

  if (save.model) {
     object@rowcol.effects <- rowcol.effects
     ## reset overall.effects to default of 'cellHTS' class to avoid problems in validity:
     object@overall.effects <-new("cellHTS")@overall.effects
   }

  object@state["normalized"] = TRUE
  validObject(object)
  return(object)
} #end function


