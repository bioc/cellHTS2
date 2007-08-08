QMexperiment = function(x, path, con, posControls, negControls, isTwoWay=FALSE, namePos) {

  nrbxp = 1+state(x)["normalized"]
  nrCh = ifelse(state(x)["normalized"], dim(normdata(x))[4], dim(x@xraw)[4])
  nrPlate = dim(rawdata(x))[2]
  wellAnno = as.character(x@wellAnno)

  actCtrls <- inhCtrls <- posCtrls <- negCtrls <- vector("list", length=nrCh)

  if(state(x)["configured"]) {


    if(isTwoWay){
      aux <- c(1:nrCh)[!emptyOrNA(posControls$act)]
      if(any(aux)) actCtrls[aux] <- lapply(aux, function(i)  as.numeric(findControls(posControls$act[i], wellAnno)))
      aux <- c(1:nrCh)[!emptyOrNA(posControls$inh)]
      if(any(aux)) inhCtrls[aux] <- lapply(aux, function(i) as.numeric(findControls(posControls$inh[i], wellAnno)))

    }else{## OneWay

      aux <- c(1:nrCh)[!emptyOrNA(posControls)]
      if(length(aux)) {
        posCtrls[aux]<- lapply(aux, function(i) {
        wa <- findControls(posControls[i], wellAnno)
        posCtrls[[i]] <- split(wa, wellAnno[wa])
        if(!all(names(posCtrls[[i]]) == namePos)) posCtrls[[i]] = posCtrls[[i]][order(names(posCtrls[[i]]))]
        posCtrls[[i]]
      })
      } #posControls
  }## else if isTwoWay

  #negative controls
  aux <- c(1:nrCh)[(!emptyOrNA(negControls))]
  if(length(aux)) negCtrls[aux] <- lapply(aux, 
        function(i) as.numeric(findControls(negControls[i], wellAnno)) )

 }## if configured 


  nrPos = sapply(actCtrls, length) + sapply(inhCtrls, length) + sapply(posCtrls, function(w) length(unlist(w)))
  nrNeg = sapply(negCtrls, length)

  ## Checks whether the number of channels has changed (e.g. normalized data)
  hasLessCh=FALSE
  if (state(x)["normalized"]) hasLessCh = dim(rawdata(x))[4] > dim(normdata(x))[4]

  ## Create a dataframe for the plots of each channel
  plotTable = data.frame(matrix(data = NA, nrow = 0, ncol = nrCh + 1))
  names(plotTable) = c("", paste("Channel", 1:nrCh, sep=" "))

  for(ch in 1:nrCh) {
    count = 0
    for (r in 1:(dim(rawdata(x))[3])) {
      makePlot(path, con=con, name=sprintf("boxplot_%d_%d", r, ch), w=5*(nrbxp-hasLessCh),
               h=5, fun = function() {
                 par(mfrow=c(1, (nrbxp-hasLessCh)), mai=c(1, 1, 0.01, 0.02))
                 if (!hasLessCh) {
                   ## to deal with cases where nrPlate=1
                   if (nrPlate==1)
                     xbp = matrix(rawdata(x)[,,r,ch])
                   else
                     xbp = rawdata(x)[,,r,ch]
                   boxplotwithNA(xbp, col(xbp), col="pink", outline=FALSE, main="", xlab="plate",
                                 ylab="raw intensity", batch=x@batch)
                 }
                 if(state(x)["normalized"]) {
                   ## to deal with cases where nrPlate=1
                   if (nrPlate==1)
                     xbp = matrix(normdata(x)[,,r,ch])
                   else
                     xbp = normdata(x)[,,r,ch]
                   boxplotwithNA(xbp, col(xbp), col="lightblue", outline=FALSE, main="", xlab="plate",
                                 ylab="normalized intensity", batch=x@batch)
                 }
               }, print=FALSE)

      if(ch ==1) {
        if(state(x)["normalized"] & !hasLessCh) 
          plotTable[count + 1, 1] = sprintf("<H3 align=left>Replicate %d </H3><em>%s</em><br>\n",
                     r, "Left: raw, right: normalized")
        else
          plotTable[count + 1, 1] = sprintf("<H3 align=left>Replicate %d</H3>", r)}

      plotTable[count + 1, ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n",
                 sprintf("boxplot_%d_%d.pdf", r, ch), sprintf("boxplot_%d_%d.png", r, ch)) 
      count = count + 1 

      if( ( nrPos[ch]>0) & (nrNeg[ch]>0) & (state(x)["normalized"]) ) {
        if (nrPlate==1)
          xbp = matrix(normdata(x)[,,r,ch])
        else
          xbp = normdata(x)[,,r,ch]

        xact= xbp[actCtrls[[ch]]]
        xinh= xbp[inhCtrls[[ch]]]
        #xpos = xbp[unlist(posCtrls[[ch]])]
        xpos = lapply(posCtrls[[ch]], function(d) xbp[d])
        xneg = xbp[negCtrls[[ch]]]
        if (!all(is.na(c(xact, xinh, unlist(xpos), xneg)))) {

          makePlot(path, con=con,
                   name=sprintf("Controls_%d_%d", r, ch), w=5*nrbxp, h=5, fun = function() {
                     par(mfrow=c(1, nrbxp), mai=c(par("mai")[1:2], 0.01, 0.02))
                     nrWell = prod(x@pdim)
                     plt = rep(1:nrPlate,each=nrWell)
                     #ppos = plt[unlist(posCtrls[[ch]])]
                     ppos = lapply(posCtrls[[ch]], function(d) plt[d]) 
                     pneg = plt[negCtrls[[ch]]]
                     pact = plt[actCtrls[[ch]]]
                     pinh = plt[inhCtrls[[ch]]]
                     ## Note: the Z'-factor will be determined considering the median and mad,
                     ## instead of the mean and standard deviation
                     drAct = abs(median(xact, na.rm=TRUE) - median(xneg, na.rm=TRUE))
                     drInh = abs(median(xinh, na.rm=TRUE) - median(xneg, na.rm=TRUE))
                     ssdAct = mad(xact, na.rm=TRUE) + mad(xneg, na.rm=TRUE)
                     ssdInh = mad(xinh, na.rm=TRUE) + mad(xneg, na.rm=TRUE)
                     zfacAct = 1-3*ssdAct/drAct
                     zfacInh = 1-3*ssdInh/drInh
                     zfacPos = lapply(xpos, function(d) 1-3*(mad(d, na.rm=TRUE) + mad(xneg, na.rm=TRUE))/(abs(median(d, na.rm=TRUE) - median(xneg, na.rm=TRUE))))
                     xvals = ppos
                     xvals$neg = pneg
                     xvals$inh = pinh
                     xvals$act = pact
                     yvals = xpos
                     yvals$neg = xneg
                     yvals$inh=xinh
                     yvals$act=xact
                     controlsplot(xvals, yvals, main="", batch=x@batch)

                     ## density function needs at least 2 points
                     ## dealing with the case where we have a single positive or negative
                     ## control well, and a single plate, so that a single measurement
                     ## is available in either xpos or xneg or both.

                     #xpos = xpos[!is.na(xpos)]
                     #xneg=xneg[!is.na(xneg)]
                     #xact=xact[!is.na(xact)]
                     #xinh=xinh[!is.na(xinh)]
                     yvals=lapply(yvals, function(d) d[!is.na(d)])
                     yvals.len = lapply(yvals, function(d) length(unlist(d))>1)
                     if (yvals.len$neg & any(yvals.len[names(yvals)!="neg"])) {
                       zfacs = zfacPos
                       zfacs$act = zfacAct
                       zfacs$inh = zfacInh
                       densityplot(values=yvals,
                                   zfacs, main="")
                     }
                   }, print=FALSE)

 
          plotTable[count + 1, 1] = "<CENTER></CENTER>"
          plotTable[count + 1, ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n",
                     sprintf("Controls_%d_%d.pdf", r, ch), sprintf("Controls_%d_%d.png", r, ch)) 

        } else {## if !all NA
          plotTable[count + 1, 1] = "<CENTER></CENTER>"
          plotTable[count + 1, ch+1] = "<CENTER><i>Values for 'pos' and 'neg' controls are missing.</i></CENTER>\n"
        }## else !allNA
      } else {## if nrPos nrNeg
        plotTable[count + 1, 1] = "<CENTER></CENTER>"
        plotTable[count + 1, ch+1] = "<CENTER><i>No controls ('pos' and 'neg') were found and/or 'x' is not normalized yet.</i></CENTER>\n"
      }## else nrPos nrNeg

      count = count + 1
    } ## for r
  } ## for ch
  return(plotTable) 

} ## QMexperiment



## --------------------------------------------------
boxplotwithNA <- function(x, fac, batch,...) {
  sel = apply(x,2,function(x) all(is.na(x)))
  bc <- rep(1, ncol(x))
  bc[sel] <- NA
  xsp = split(x, fac)
  bp <- boxplot(xsp, plot=FALSE)
  border <- IQR(x, na.rm=TRUE)/10
  lowerLim <- min(bp$stats[1,], na.rm=TRUE)-border
  upperLim <- max(bp$stats[5,], na.rm=TRUE)+border
  boxplot(xsp, ..., ylim=c(lowerLim, upperLim), border=bc)
  if(ncol(x)==1) axis(1, 1)

  bdiff = diff(batch)
  if(sum(bdiff)>0) {
    ind = 1:length(batch)
    abline(v=ind[as.logical(bdiff)]+0.5, lty=1)
  } 

}


## --------------------------------------------------
densityplot <- function(values, zfacs, ...) {
  dens <- list()
  ymax <- xmax <- xmin <- numeric(0)

  len.x = length(values)
  cols <- c(neg="#2040FF", act="#E41A1C", inh="#FFFF00")  
  if (len.x>3) {
    Lab.pal = colorRampPalette(c("darkred",  "red", "orange"), space="Lab")(len.x-3)
    names(Lab.pal) = names(values)[! (names(values) %in% names(cols))] 
    cols <- append(cols, Lab.pal)
    if ("pos" %in% names(cols) & len.x==4) cols["pos"]="#E41A1C"
  }
  sel <- sapply(values, length)
  values <- values[sel>1]
  zfacs <- zfacs[!is.na(zfacs)]
  for(i in 1:length(values)){
    theDens <- density(values[[i]], na.rm=TRUE, adjust=4)
    ymax <- max(ymax, theDens$y)
    xmax <- max(xmax, theDens$x)
    xmin <- min(xmin, theDens$x)
    dens[[i]] <- theDens
    names(dens)[i] <- names(values)[i]
  }
  #cols <- c(pos="red", neg="blue", act="red", inh="yellow")
  plot(dens[[1]], xlim = c(xmin, xmax), ylim=c(0, ymax*1.2), col=cols[names(dens)[1]], yaxt="n",ylab="",
       xlab="normalized intensity", ...)
  for(i in 2:length(dens))
    lines(dens[[i]], col=cols[names(dens)[i]])
  legend("top", legend=paste("'", names(dens), "' controls", sep=""), pch=16, col=cols[names(dens)],
         bg="white", cex=0.7, title = paste(sprintf("Z'-factor (%s) = %g", names(zfacs), round(unlist(zfacs),2)),
                                collapse=" "), horiz=TRUE, pt.cex=0.5, bty="n") 
}


## --------------------------------------------------
controlsplot <- function(xvals, yvals, batch, ...) {
  ylim <- range(unlist(yvals), na.rm=TRUE)
#   if (prod(ylim)<0) {
#     ylim <- sign(ylim)*1.25*abs(ylim)
#   } else {
#     if (ylim[1]<0) ylim = c(1.25, 0.7)*ylim else ylim = c(0.85, 1.4)*ylim
#   }

#  ylim = c(1-sign(ylim[1])*0.25, 1+sign(ylim[2])*0.25)*ylim 
  inc = 0.2*diff(ylim)
  ylim = ylim+c(-inc, inc)
  cols <- c(neg="#2040FF", act="#E41A1C", inh="#FFFF00") 
  len.x = length(xvals)
  if (len.x > 3) {
    Lab.pal = colorRampPalette(c("darkred",  "red", "orange"), space="Lab")(len.x-3)
    names(Lab.pal) = names(xvals)[! (names(xvals) %in% names(cols))] 
    cols <- append(cols, Lab.pal)
    if ("pos" %in% names(cols) & len.x==4) cols["pos"]="#E41A1C"
  }

  sel <- sapply(xvals, length)
  xvals <- xvals[sel!=0]
  yvals <- yvals[sel!=0]
  stopifnot(names(xvals) == names(yvals))

#  cols <- c(pos="red", neg="blue", act="red", inh="yellow")
  plot(xvals[[1]], yvals[[1]], pch=16, cex=0.5, ylim=ylim, xlab="plate", ylab="normalized intensity",
       col=cols[names(xvals)[1]], xaxt="n", ...)
  legend("top",legend=paste("'", names(xvals), "' controls", sep=""), col=cols[names(xvals)],
         horiz=TRUE, pch=16, pt.cex=0.5, bg="white", cex=0.7, bty="n")
  xall = split(unlist(yvals), unlist(xvals))
  xall = xall[!sapply(xall, function(f) all(is.na(f)))]
  xalls = data.frame(lapply(xall, function (k) range(k, na.rm=TRUE)))
  segments(as.numeric(names(xall)), as.matrix(xalls)[1,], as.numeric(names(xall)),
           as.matrix(xalls)[2,], lty=3)
  for(i in 2:length(xvals))
    points(xvals[[i]], yvals[[i]], pch=16, cex=0.5, col=cols[names(xvals)[i]])
  mp = max(unlist(xvals))
  if ((mp-1)%/%20)
    by=10
  else
    by=ifelse((mp-1)%/%10, 5, 1) 
  axis(1, at = c(1, seq(0,mp,by=by)[-1]), labels = TRUE)

  batch=batch[unique(unlist(xvals))] 
  bdiff=diff(batch)
  if(sum(bdiff)>0) {
    ind = unique(unlist(xvals))
    abline(v=ind[as.logical(bdiff)]+0.5, lty=1)
  } 
  return(list(xvals, yvals))
}


