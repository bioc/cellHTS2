
myImageMap <- function(object, tags, imgname) {

  if(!is.matrix(object)||ncol(object)!=4)
    stop("'object' must be a matrix with 4 columns.")

  for(i in seq(along=tags))
    if(length(tags[[i]])!=nrow(object))
      stop(paste("'tags[[", i, "]] must have as many elements as 'object' has rows (",
                 nrow(object),").", sep=""))

  mapname <- paste("map", gsub(" |/|#", "_", imgname), sep="_")
  out <- paste("<IMG SRC=\"", imgname, "\" USEMAP=#", mapname, " BORDER=2>", 
                         "<MAP NAME=\"", mapname, "\">", sep="")
  for(i in 1:nrow(object)) {
    out = paste(out, "<AREA SHAPE=\"rect\" COORDS=\"", paste(object[i,], collapse=","),
                "\"", sep="")
    for(t in seq(along=tags))
      out = paste(out, " ", names(tags)[t], "=\"", tags[[t]][i], "\"", sep="")
    out = paste(out, "\">", sep="")
  }
}


#"# ---------------------------------------------------------------------------
QMbyPlate <- function(x, wellAnno, pdim, name, basePath, subPath, geneAnno,
                      plotPlateArgs, brks, finalWellAnno, posControls, 
                 	  negControls, isTwoWay=FALSE, namePos) {

  fn  = file.path(subPath, "index.html")
  con = file(file.path(basePath, fn), "w")
  on.exit(close(con))

  writeheader(paste("Quality report for", name), 1, con)

  stopifnot(dim(x)[2]==1)
  stopifnot(dim(x)[1]==length(wellAnno))

  nrWells = prod(pdim)
  nrChannel = dim(x)[4]
  maxRep = dim(x)[3]
  ## which of the replicate plates has not just all NA values
  whHasData = list()
  for (ch in 1:nrChannel)
    whHasData[[ch]] = which(apply(x[,,,ch,drop=FALSE], 3, function(xx) !all(is.na(xx))))

  nrRepCh = sapply(whHasData, length)

  ## Checks whether the number of channels has changed (e.g. normalized data)
  hasLessCh = any(dim(finalWellAnno)!=dim(x))


  ## Define colors and comment on them; Handle controls.
  ## to avoid having the legend for 'pos' when we have 'inhibitors' and 'activators' or vice-versa
  if(isTwoWay) {
     wellTypeColor=c(neg="#2040FF", act="#E41A1C", inh="#FFFF00", sample="#000000", controls="#43FF00",
       other="#999999", empty="#FA00FF", flagged="#000000") 
     actCtrls <- inhCtrls <- vector("list", length=nrChannel)
     aux <- c(1:nrChannel)[!emptyOrNA(posControls$act)]
     if(any(aux)) actCtrls[aux] <- lapply(aux, function(i) as.numeric(findControls(posControls$act[i], wellAnno))) #needs to be like this because of the case of length(aux)=1
     aux <- c(1:nrChannel)[!emptyOrNA(posControls$inh)]
     if(any(aux)) inhCtrls[aux] <- lapply(aux, function(i) as.numeric(findControls(posControls$inh[i], wellAnno)))

  } else {  #oneWay
     wellTypeColor=c(pos="#E41A1C", neg="#2040FF", sample="#000000", controls="#43FF00", other="#999999", empty="#FA00FF", flagged="#000000")
     posCtrls <- vector("list", length=nrChannel)
     posCtrls <- lapply(posCtrls, function(z) { 
       z = vector("list", length=length(namePos)) 
       names(z) = namePos
       return(z) })

     aux <- c(1:nrChannel)[!emptyOrNA(posControls)]
     if(any(aux)) posCtrls[aux] <- lapply(aux, 
      function(i) {
        wa <- findControls(posControls[i], wellAnno)
        wa <- split(wa, wellAnno[wa])
        posCtrls[[i]][match(names(wa), tolower(namePos))] <- wa
        posCtrls[[i]]
      }
     )
 }

  mt = match(wellAnno, names(wellTypeColor))
  samples <- which(mt==which(names(wellTypeColor)=="sample"))

# negative controls:
  negCtrls <- vector("list", length=nrChannel)
  aux <- c(1:nrChannel)[!emptyOrNA(negControls)]
  if(any(aux)) negCtrls[aux] <- lapply(aux, function(i) as.numeric(findControls(negControls[i], wellAnno)))




  ## calculate quality metrics
  for (ch in 1:nrChannel) {
    nrRep = nrRepCh[ch]
    qm = data.frame(metric=I(character(0)), value=numeric(0), comment=I(character(0)))
    count = 0
    ## see the data scale for determining the dynamic range
    ## positive scale: go to log-scale, average and take difference, then re-exponentiate
    ##                 this assumes that the data are on multplicative scale.
    ## positive and negative scale: this may happen when we have scored the replicates 
    ##                 separately and saved the results in x$xnorm; determine the difference 
    ##                 between the aritmetic mean between pos and negative controls.

    allPositives = ifelse(all(is.na(x[,,,ch])), TRUE, prod(range(x[,,,ch], na.rm=TRUE))>0)


    if(isTwoWay){
      ## 1.a) Dynamic range (neg / activators)
      if(length(actCtrls[[ch]])>0 && length(negCtrls[[ch]])>0) {
        if (allPositives) 
          drAct = apply(x[,,,ch, drop=FALSE], 3, function(v)
            mean(log(v[actCtrls[[ch]]]), na.rm=TRUE) - mean(log(v[negCtrls[[ch]]]), na.rm=TRUE))
         else 
          drAct = apply(x[,,,ch, drop=FALSE], 3, function(v)
            mean(v[actCtrls[[ch]]], na.rm=TRUE)- mean(v[negCtrls[[ch]]], na.rm=TRUE))

         drAct[is.na(drAct)]=as.numeric(NA)

      ## consider also the dynamic range for each individual replicate
      for (r in 1:maxRep) {
       if (r %in% whHasData[[ch]]) { 
           if (is.na(drAct[r])) 
             commR = I("No available values for one of the controls") 
           else 
             commR = I("")
           if (allPositives) qrval = round(exp(drAct[r]), 2) else qrval = round(abs(drAct[r]), 2)
           qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range Activators (replicate %s)",r)), 
             value=qrval, comment=commR)) 
       } else {## if r %in% whHasData[ch]
        qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range Activators (replicate %s)",r)), 
          value=NA, comment=I(sprintf("Replicate %s is missing", r))))
       }## else r %in% whHasData[ch]
     }## for r

    if (allPositives) 
      drAct = round(exp(mean(drAct, na.rm=TRUE)), 2) 
    else 
      drAct = round(abs(mean(drAct, na.rm=TRUE)), 2)

      if (is.na(drAct)) { 
	    drAct = as.numeric(NA) 
	    commAct="No available values for one of the controls in all replicates" 
      } else {
       commAct = "" 
      }

    } else { ##if length(actCtrls)
       drAct <- as.numeric(NA)
       commAct <- "No controls ('activators' and 'neg') were found."
	   for (u in 1:maxRep) 
             qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range Activators (replicate %d)",u)), 
               value=drAct, comment=I(commAct)))
    } ##else if length(actCtrls)


      ## 1.b) Dynamic range (neg / inhibitors)
      if(length(inhCtrls[[ch]])>0 && length(negCtrls[[ch]])>0) {
        if (allPositives) 
          drInh = apply(x[,,,ch, drop=FALSE], 3, function(v)
           mean(log(v[negCtrls[[ch]]]), na.rm=TRUE) - mean(log(v[inhCtrls[[ch]]]), na.rm=TRUE))
         else 
          drInh = apply(x[,,,ch, drop=FALSE], 3, function(v)
           mean(v[negCtrls[[ch]]], na.rm=TRUE)- mean(v[inhCtrls[[ch]]], na.rm=TRUE))

        drInh[is.na(drInh)] = as.numeric(NA)

      ## consider also the dynamic range for each individual replicate
      for (r in 1:maxRep) {
       if (r %in% whHasData[[ch]]) { 
           if (is.na(drInh[r])) 
             commR = I("No available values for one of the controls") 
           else 
             commR = I("")
           if (allPositives) qrval = round(exp(drInh[r]), 2) else qrval = round(abs(drInh[r]), 2)
           qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range Inhibitors (replicate %s)",r)), 
             value=qrval, comment=commR)) 
       } else {## if r %in% whHasData[ch]
        qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range Inhibitors (replicate %s)",r)), 
          value=NA, comment=I(sprintf("Replicate %s is missing", r))))
       }## else r %in% whHasData[ch]
     }## for r

    if (allPositives) 
      drInh = round(exp(mean(drInh, na.rm=TRUE)), 2)
    else
      drInh = round(abs(mean(drInh, na.rm=TRUE)), 2)

      if (is.na(drInh)) { 
	    drInh = as.numeric(NA) 
	    commInh="No available values for one of the controls in all replicates" 
      } else {
       commInh = "" 
      }

    } else { ##if length(inhCtrls)
       drInh <- as.numeric(NA)
       commInh <- "No controls ('inhibitors' and 'neg') were found."
	   for (u in 1:maxRep) 
             qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range Inhibitors (replicate %d)",u)), 
               value=drInh, comment=I(commInh)))
    } ##else if length(inhCtrls)

 qm = rbind(qm, data.frame(metric=I("Dynamic range (Activators)"),
    value=drAct, comment=I(commAct)))
 qm = rbind(qm, data.frame(metric=I("Dynamic range (Inhibitors)"),
    value=drInh, comment=I(commInh)))


    } else {
      ##---------------------------------------
      ##  One Way
      ##----------------------------------------
      if (!is.null(posCtrls[[ch]])) {
        ## for each different positive control:
        for (pname in names(posCtrls[[ch]])) {
          if (pname=="pos" & length(posCtrls[[ch]])==1) pn="" else pn = sprintf("'%s'",pname)

          ## 1. Dynamic range (neg / pos controls)
          if(length(posCtrls[[ch]][[pname]])>0 && length(negCtrls[[ch]])>0) {

            ## see the data scale.
            ## positive scale: go to log-scale, average and take difference, then re-exponentiate
            ##                 this assumes that the data are on multplicative scale.
            ## positive and negative scale: this may happen when we have scored the replicates 
            ##                 separately and saved the results in x$xnorm; determine the difference 
            ##                 between the arithmetic mean between pos and negative controls.

            currentPos = posCtrls[[ch]][[pname]]
            dr = if(allPositives) {
              apply(x[,,,ch, drop=FALSE], 3, function(v)
                    exp(abs(mean(log(v[currentPos]), na.rm=TRUE) - mean(log(v[negCtrls[[ch]]]), na.rm=TRUE))))
            } else {
              apply(x[,,,ch, drop=FALSE], 3, function(v)
                    abs(mean(    v[currentPos] , na.rm=TRUE) - mean(    v[negCtrls[[ch]]] , na.rm=TRUE)))
            }
            dr = round(dr, 2)

            ## consider also the dynamic range for each individual replicate
            for (r in 1:maxRep) {
              if (r %in% whHasData[[ch]]) { 
                commR = I(if (is.na(dr[r])) "No available values for one of the controls" else "")
                qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range %s (replicate %s)",pn, r)),
                  value=dr[r], comment=commR)) 
              } else {
                qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range %s (replicate %s)",pn, r)),
                  value=NA, comment=I(sprintf("Replicate %s is missing", r))))
              }
            } ## for r

            dr = if(allPositives) 
              round(exp(mean(log(dr), na.rm=TRUE)), 2) 
            else 
              round(mean(dr, na.rm=TRUE), 2)
 
            if(is.na(dr)) { 
              comm="No available values for one of the controls in all replicates" 
            } else {
              comm = "" 
            } 

          } else { ## if length pos & neg
            dr = as.numeric(NA)
            comm = "No controls ('pos' and/or 'neg') were found."
            for (u in 1:maxRep) 
              qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range %s (replicate %d)",pn,u)), value=dr, comment=I(comm)))
          } ## else length pos & neg

          qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range %s", pn)), 
            value=dr, comment=I(comm)))
        } ## for names(posCtrls[[ch]])


      } else {
        ## if !is.null(posCtrls[[ch]])
        for (pname in namePos) {
          if (pname=="pos" & length(posCtrls[[ch]])==1) pn="" else pn = sprintf("'%s'",pname)
          dr = as.numeric(NA)
          comm = "No controls ('pos' and 'neg') were found."
          for (u in 1:maxRep) 
            qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range %s (replicate %d)",pn,u)), value=dr, comment=I(comm)))
          qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range %s", pn)), 
            value=dr, comment=I(comm)))
        }## for namePos

      }## else !is.null(posCtrls[[ch]])
    }## else isTwoWay

    ## 2. Correlation coefficient (just for samples wells)
    if (nrRep==2) {
      cc = round(cor(x[samples,,whHasData[[ch]],ch], x[samples,,whHasData[[ch]],ch], use="complete.obs", method="spearman")[1,2], 2)
      comm = ""
     } else {
       cc = as.numeric(NA)
       comm = sprintf("%d replicate(s)", nrRep)
    }
   qm = rbind(qm, data.frame(metric=I("Spearman rank correlation"), value=cc, comment=I(comm)))

   if (exists("qmplate")) qmplate = cbind(qmplate, qm) else qmplate = qm

   ## summary of the quality metrics in 'qm' to be returned by this function:
   if (!exists("qmsummary")) {
     qmsummary=list()
     length(qmsummary) = nrChannel
     names(qmsummary) = sprintf("Channel %d", 1:nrChannel)
   }

    qmsummary[[ch]] = qm$value
    names(qmsummary[[sprintf("Channel %d", ch)]]) = qm$metric 
  } # for ch 

  writeHTMLtable(qmplate, con=con, center=TRUE, extra=sprintf("Channel %d", 1:nrChannel))

  ## color legend for each channel 
  ## For the original configuration plate corrected by the screen log information:
  wellCount = data.frame(matrix(NA, ncol = nrChannel, nrow = 2))
  names(wellCount) = sprintf("Channel %d", 1:nrChannel)
  mtt <- vector("list", length = nrChannel)
  iF <- which(names(wellTypeColor)=="flagged")
  iE <- which(names(wellTypeColor)=="empty")
  iO <- which(names(wellTypeColor)=="other")
  iC <- which(names(wellTypeColor)=="controls")
  iP <- which(names(wellTypeColor)=="pos")
  iN <- which(names(wellTypeColor)=="neg")
  iA <- which(names(wellTypeColor)=="act")
  iI <- which(names(wellTypeColor)=="inh")

  if (hasLessCh & nrChannel==1) {
    ## The color code must take into account the common entries between channels and replicates 

    mtt[[1]] = mt
    fwa = matrix(finalWellAnno, ncol = prod(dim(finalWellAnno)[3:4]))
    mtrep = apply(fwa, 2, function(u) match(u, names(wellTypeColor)))
    ## include the controls that were not annotated as "neg" or "pos":
    if (isTwoWay)  {
    mtrep[actCtrls[[1]],] [which(is.na(mtrep[actCtrls[[1]],]))]=iA
    mtrep[inhCtrls[[1]],] [which(is.na(mtrep[inhCtrls[[1]],]))]=iI
    } else {
    mtrep[unlist(posCtrls[[1]]),] [which(is.na(mtrep[unlist(posCtrls[[1]]),]))]=iP
    }
    mtrep[negCtrls[[1]],] [which(is.na(mtrep[negCtrls[[1]],]))]=iN

    ## replace the remaining NA positions by "other" (these corresponds to wells that
    ## although annotated as controls in the configuration file, don't behave as
    ## controls in the current channel
    mtrep[which(is.na(mtrep))]=iO
    aa = apply(fwa, 2, function(u) sum(u=="flagged"))
    aa = order(aa, decreasing=TRUE) # position 1 contains the replicate with more flagged values
    nrWellTypes = sapply(seq(along=wellTypeColor), function(i) sum(mtrep[,aa[1]]==i, na.rm=TRUE))

    ## flagged wells
    wellCount[1,1] = paste(sprintf("%s: %d", names(wellTypeColor)[iF], nrWellTypes[iF]), collapse=", ")
    ## all the other wells, except controls
    wellCount[2, 1] = paste(sprintf("<FONT COLOR=\"%s\">%s: %d</FONT>", wellTypeColor[-c(iF,iC)], names(wellTypeColor)[-c(iF,iC)], nrWellTypes[-c(iF, iC)]), collapse=", ")
    ## so "flagged" always wins over "pos", "neg" or "sample"
    mtt[[1]][is.na(mtt[[1]])]=apply(mtrep[is.na(mtt[[1]]),, drop=FALSE], 1, max) 
    ## so "controls" always win over "pos" or "neg" or "act" or "inh" or "sample"
    mtt[[1]][!is.na(mtt[[1]])]=apply(mtrep[!is.na(mtt[[1]]),, drop=FALSE], 1, max)

  } else { ## if hasLessCh

    for (ch in 1:nrChannel) {
      mtt[[ch]] = mt
      mtrep = apply(finalWellAnno[,,,ch, drop=FALSE], 3, function(u) match(u, names(wellTypeColor)))

      ## include the controls that were not annotated as "neg" or "pos":
      ## correct 'pos' controls just for one-way assays
      if (!isTwoWay) {
         if (length(unlist(posCtrls[[ch]]))) {
           mtrep[unlist(posCtrls[[ch]]),][which(is.na(mtrep[unlist(posCtrls[[ch]]),]))]=iP
         } else { ## if length pos
         ## replace possible wells annotated as "pos" by NA, because they shouldn't be considered as a positive control for this channel:
           if (any(mtt[[ch]] %in% iP)) {
             mtrep[mtt[[ch]] %in% iP,]=NA
             mtt[[ch]][mtt[[ch]] %in% iP]=NA 
           } ## if any
         } ## else length pos
      }else{ ## if !isTwoWay

    ## include the controls that were not annotated as "act" or "neg", but only if they
    ## should be regarded as such in this channel
    if (length(actCtrls[[ch]])) {
      mtrep[actCtrls[[ch]],] [which(is.na(mtrep[actCtrls[[ch]],]))]=iA
    }else{## if length act
       if (any(mtt[[ch]] %in% iA)) {
        mtrep[mtt[[ch]] %in% iA,]=NA
        mtt[[ch]][mtt[[ch]] %in% iA]=NA 
      } ## if any
    } ## else length act
    if (length(inhCtrls[[ch]])) {
      mtrep[inhCtrls[[ch]],] [which(is.na(mtrep[inhCtrls[[ch]],]))]=iI
    }else{## if length inh
       if (any(mtt[[ch]] %in% iI)) {
        mtrep[mtt[[ch]] %in% iI,]=NA
        mtt[[ch]][mtt[[ch]] %in% iI]=NA 
      } ## if any
    } ## else length inh
   }##else if !isTwoWay


      ## for the negative controls
     if (length(negCtrls[[ch]])) {
       mtrep[negCtrls[[ch]],] [which(is.na(mtrep[negCtrls[[ch]],]))]=iN 
     } else { ## if length neg
      if (any(mtt[[ch]] %in% iN)) {
        mtrep[mtt[[ch]] %in% iN,]=NA
        mtt[[ch]][mtt[[ch]] %in% iN]=NA 
      } ## if any
    } ## else length neg

      ## replace the remaining NA positions by "other" (these corresponds to wells that
      ## although annotated as controls in the configuration file, don't behave as controls
      ## in the current channel
      mtrep[which(is.na(mtrep))]=iO
      aa = apply(finalWellAnno[,,,ch, drop=FALSE], 3, function(u) sum(u=="flagged"))
      aa = order(aa, decreasing=TRUE)
      nrWellTypes = sapply(seq(along=wellTypeColor), function(i) sum(mtrep[,aa[1]]==i, na.rm=TRUE))

      ## flagged wells
      wellCount[1,ch] = paste(sprintf("%s: %d", names(wellTypeColor)[iF], nrWellTypes[iF]), collapse=", ")
      ## the other wells, except controls
      wellCount[2, ch] = paste(sprintf("<FONT COLOR=\"%s\">%s: %d</FONT>",
                 wellTypeColor[-c(iF, iC)], names(wellTypeColor)[-c(iF,iC)],
                 nrWellTypes[-c(iF, iC)]), collapse=", ")
      ## so "flagged" always wins over "pos", "neg" or "sample" or "act" or "inh"
      mtt[[ch]][is.na(mtt[[ch]])]=apply(mtrep[is.na(mtt[[ch]]),, drop=FALSE], 1, max)
    }## for channel
  }## else hasLessCh

  cat("<BR>\n", file=con)
  cat("<BR>\n", file=con)

  ## Create a dataframe for the plots of each channel
  plotTable = data.frame(matrix(data = NA, nrow = 0, ncol = nrChannel + 1))
  names(plotTable) = c("", paste("Channel", 1:nrChannel, sep=" "))

  plsiz = 4
  for (ch in 1:nrChannel) {
    nrRep = nrRepCh[ch]
    ## scatterplot
    ## plot title
    plotTable[1, ch+1] = "<H5 align=center><FONT color=#494F8A>SCATTERPLOT BETWEEN REPLICATES</FONT></H5>\n"
    count = 1

    if(nrRep==2) {
      makePlot(file.path(basePath, subPath), con=con,
               name=sprintf("scp_Channel%d", ch), w=plsiz, h=plsiz, fun = function() {
                 par(mai=c(0.9,0.9,0.2,0.2))
                 ylim=c(min(x[,,,ch], na.rm=TRUE), max(x[,,,ch], na.rm=TRUE))
                 plot(x[,,whHasData[[ch]][1],ch], x[,,whHasData[[ch]][2],ch], pch=16, cex=0.5,
                      ylim=ylim, xlab="replicate 1", ylab="replicate 2", col=wellTypeColor[mtt[[ch]]])
                 abline(a=0, b=1, col="lightblue")
               }, print=FALSE)

      ## color legend:
      wellLeg = paste(sprintf("<CENTER>%s</CENTER><BR>\n", wellCount[1,ch]),
        sprintf("<CENTER><em>Color legend: </em><br> %s</CENTER><BR>\n", wellCount[2,ch]), collapse="")

      plotTable[count + 1, ch+1] = sprintf("%s<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER>\n",
                 wellLeg, sprintf("scp_Channel%d.pdf", ch), sprintf("scp_Channel%d.png", ch))
 
    } else {## if nrRep
      ##plotTable[count + 1, ch+1] = ""
      plotTable[count + 1, ch+1] = sprintf("<CENTER>%d replicate(s): scatterplot omitted</CENTER>\n", nrRep)
    }
    count = count + 1 

    ## histograms (replicates)
    ## plot title
    plotTable[count + 1, ch+1] = "<H5 align=center><FONT color=#494F8A>HISTOGRAM(S)</FONT></H5>\n"
      count= count + 1
    for (r in 1:maxRep) {
      plotTable[count+1, 1] = sprintf("<H4 align=left>Replicate %d</H4>\n", as.integer(r))
      if (r %in% whHasData[[ch]]){
        makePlot(file.path(basePath, subPath), con=con,
                 name=sprintf("hist_Channel%d_%02d",ch,r), w=plsiz, h=plsiz*.6, fun = function() {
                   par(mai=c(0.7,0.25,0.01,0.1))
                   hist(x[,,r,ch], xlab ="", breaks=brks[[ch]],
                        col = gray(0.95), yaxt = "n", main="")
                   rug(x[,,r,ch])
                 }, print=FALSE)
        plotTable[count+1,ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER>\n",
                   sprintf("hist_Channel%d_%02d.pdf",ch,r), sprintf("hist_Channel%d_%02d.png",ch,r)) 
      } else {
        plotTable[count + 1, ch+1] = sprintf("<CENTER>Replicate %d is missing</CENTER>\n", r)}
      count = count+1
    }## for r
  } ## for channel


  if(is.list(plotPlateArgs)) {
    if(!all(names(plotPlateArgs) %in% c("sdcol", "sdrange", "xcol", "xrange")))
      stop("Only elements 'sdcol', 'sdrange', 'xcolx', and 'xrange' are allowed for 'plotPlateArgs'")

    plsiz = 4

    ## Currently, it does not allows to use different colors for different channels
    if(is.null(plotPlateArgs$sdcol))
      plotPlateArgs$sdcol = brewer.pal(9, "YlOrRd")
    if(is.null(plotPlateArgs$xcol))
      plotPlateArgs$xcol = rev(brewer.pal(9, "RdBu"))

    ## set this argument as a list with the same length as the number of channels
    if(is.null(plotPlateArgs$xrange)) { 
      plotPlateArgs$xrange = list()
      length(plotPlateArgs$xrange)=nrChannel} else {
        if  (!is.list(plotPlateArgs$xrange)) {
          plotPlateArgs$xrange=list(plotPlateArgs$xrange)
	  length(plotPlateArgs$xrange)=nrChannel} }
 
    ## set this argument as a list with the same length as the number of channels
    if(is.null(plotPlateArgs$sdrange)) {
      plotPlateArgs$sdrange = list()
      length(plotPlateArgs$sdrange) = nrChannel } else {
        if (!is.list(plotPlateArgs$sdrange)) {
          plotPlateArgs$sdrange=list(plotPlateArgs$sdrange)
          length(plotPlateArgs$sdrange)=nrChannel } }

    oldcount = count

    for (ch in 1:nrChannel) {
      char <- character(dim(x)[1])
      char[negCtrls[[ch]]] <- "N"
      if (isTwoWay) {
        char[actCtrls[[ch]]] <- "A"
        char[inhCtrls[[ch]]] <- "I"
      } else {
        char[unlist(posCtrls[[ch]])] <- "P" 
      }
      count = oldcount
      ## plot global title
      plotTable[count+1, ch+1] = "<H5 align=center><FONT color=#494F8A>PLATE PLOT(S)</FONT></H5>\n"

        ## plot title
        plotTable[count+2, 1] = "<H5 align=left>Standard deviation across replicates</H5>\n"

      ## platePlot of sd
      psd = apply(x[,,,ch,drop=FALSE], 1, sd, na.rm=TRUE)

      if(!all(is.na(psd))){

        if(is.null(plotPlateArgs$sdrange[[ch]]))
          plotPlateArgs$sdrange[[ch]]=c(0, quantile(psd, 0.95, na.rm=TRUE))

        pp <- makePlot(file.path(basePath, subPath), con=con,
                        name=sprintf("ppsd_Channel%d",ch), w=plsiz, fun = function() {
                          return(plotPlate(psd, nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
                                           main="between replicate standard deviations",
                                           col=plotPlateArgs$sdcol, char=char,
                                           xrange=plotPlateArgs$sdrange[[ch]]) )
                        }, print=FALSE, isPlatePlot=TRUE)
        img <- myImageMap(object=pp$coord, tags=list(TITLE=paste(geneAnno, ": sd=", signif(psd,3), sep=""),
                                             HREF=rep(sprintf("ppsd_Channel%d.pdf", ch),
                                               length(geneAnno))), sprintf("ppsd_Channel%d.png", ch))
        plotTable[count+2, ch+1] = paste("<CENTER>", img, "</CENTER><BR>\n", sep="")
      } else {
        plotTable[count+2, ch+1] = sprintf("<CENTER>%d replicate(s): plate plot omitted</CENTER>\n",
                          nrRep)}

      count = count + 2
      ## platePlot of intensities
      for (r in 1:maxRep) {
        plotTable[count+1, 1] = sprintf("<H4 align=left>Replicate %d</H4>\n", as.integer(r))
        if (r %in% whHasData[[ch]]){

          if(is.null(plotPlateArgs$xrange[[ch]]))
            plotPlateArgs$xrange[[ch]]=quantile(x[,,,ch], c(0.025, 0.975), na.rm=TRUE)

          pp <- makePlot(file.path(basePath, subPath), con=con,
                   name=sprintf("pp_Channel%d_%d",ch,r), w=plsiz, h=plsiz*0.66, fun = function() {
                     plotPlate(x[,,r,ch], nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
                               main=sprintf("intensities for replicate %d", r),
                               col=plotPlateArgs$xcol, char=char,
                               xrange=plotPlateArgs$xrange[[ch]])
                   }, print=FALSE, isPlatePlot=TRUE)
          img <- myImageMap(object=pp$coord, tags=list(TITLE=paste(geneAnno, ": value=",
                            signif(x[,,r,ch],3), sep=""), HREF=rep(sprintf("pp_Channel%d_%d.pdf", ch, r),
                                       length(geneAnno))), sprintf("pp_Channel%d_%d.png", ch, r))
          plotTable[count+1,ch+1] =  paste("<CENTER>", img, "</CENTER><BR>\n", sep="")
        } else {## if r %in$ whHasData[[ch]]
          plotTable[count + 1, ch+1] = sprintf("<CENTER>Replicate %d is missing</CENTER>\n", r)
        }## else if r %in% whHasData[[ch]]
        count = count+1
      } # maxRep

##      ## plate plot of summarized values
##      plotTable[count+1, 1] = "<H4 align=left>Summarized replicates</H4>\n"
##      sumVal <- rowMeans(x[,,,ch])
##      if(!all(is.na(sumVal))){
##        if(is.null(plotPlateArgs$xrange[[ch]]))
##          plotPlateArgs$xrange[[ch]]=quantile(sumVal, c(0.025, 0.975), na.rm=TRUE)
##        pp <- makePlot(file.path(basePath, subPath), con=con,
##                        name=sprintf("ppSum_Channel%d",ch), w=plsiz, h=plsiz*0.66, fun = function() {
##                          plotPlate(sumVal, nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
##                                    main="mean intensities for all replicates",
##                                    col=plotPlateArgs$xcol, char=char,
##                                    xrange=plotPlateArgs$xrange[[ch]])
##                        }, print=FALSE, isPlatePlot=TRUE)
##        img <- myImageMap(object=pp$coord, tags=list(TITLE=paste(geneAnno, ": score=",
##                          signif(sumVal,3), sep=""), HREF=rep(sprintf("ppSum_Channel%d.pdf", ch),
##                          length(geneAnno))), sprintf("ppSum_Channel%d.png", ch))      
##        plotTable[count+1,ch+1] =  paste("<CENTER>", img, "</CENTER><BR>\n", sep="")
##      } else {
##        plotTable[count + 1, ch+1] = sprintf("<CENTER>%d replicate(s): plate plot omitted</CENTER>\n",
##                          nrRep)
##      }

    } # channel
  } # if(is.list(plotPlateArgs))

  ## include also a "channel 2 vs channel 1" plot if the number of channels is 2
  if (nrChannel==2) {	
    ## correct the color code for the 2-channel scatterplot
    ## For the original configuration plate corrected by the screen log information:
    wellCount = data.frame(matrix(NA, ncol = maxRep, nrow = 2))
    names(wellCount) = sprintf("Replicate %d", 1:maxRep)
    mtt <- vector("list", length = maxRep)
    iF <- which(names(wellTypeColor)=="flagged")
    iE <- which(names(wellTypeColor)=="empty")
    iO <- which(names(wellTypeColor)=="other")
    if (isTwoWay) 
      ctrls <- unique(c(unlist(actCtrls), unlist(inhCtrls), unlist(negCtrls))) 
    else
      ctrls <- unique(c(unlist(posCtrls), unlist(negCtrls)))

    iPNAI <- which(names(wellTypeColor) %in% c("pos", "neg", "act", "inh"))

    for (r in 1:maxRep) {
      mtt[[r]] = mt
      mtrep = apply(finalWellAnno[,,r,, drop=FALSE], 4, function(u) match(u, names(wellTypeColor)))

      ## set the controls in any of the channels as "controls":
      mtrep[ctrls,] [which(is.na(mtrep[ctrls,]) | mtrep[ctrls,] %in% iPNAI)] = which(names(wellTypeColor)=="controls")

      aa = apply(finalWellAnno[,,r,, drop=FALSE], 4, function(u) sum(u=="flagged"))
      aa = order(aa, decreasing=TRUE)
      nrWellTypes = sapply(seq(along=wellTypeColor), function(i) sum(mtrep[,aa[1]]==i, na.rm=TRUE))

      wellCount[1,r] = paste(sprintf("%s: %d", names(wellTypeColor)[iF], nrWellTypes[iF]), collapse=", ")
      wellCount[2, r] = paste(sprintf("<FONT COLOR=\"%s\">%s: %d</FONT>", wellTypeColor[-c(iF, iPNAI)], names(wellTypeColor)[-c(iF, iPNAI)], nrWellTypes[-c(iF, iPNAI)]), collapse=", ")
      ## so "flagged" or "empty" always wins over "controls" or "sample"
      mtt[[r]][is.na(mtt[[r]])]=apply(mtrep[is.na(mtt[[r]]),, drop=FALSE], 1, max) 
      ## so "controls" always win over "pos" or "neg" or "sample" or "act" or "inh"
      mtt[[r]][!is.na(mtt[[r]])]=apply(mtrep[!is.na(mtt[[r]]),, drop=FALSE], 1, max) 

    }## for r

    plotTable$Channel2vs1 = ""

    ## plot title
    plotTable[3, 4] <- "<H5 align=center><FONT color=#494F8A>SCATTERPLOT BETWEEN CHANNELS</FONT></H5>\n"
    for (r in 1:maxRep) {

      if ( (r %in% whHasData[[1]]) & (r %in% whHasData[[2]]) ) {
        ## color legend:
        wellLeg = paste(sprintf("<CENTER>%s</CENTER><BR>\n", wellCount[1,r]),
          sprintf("<CENTER><em>Color legend: </em> %s</CENTER><BR>\n", wellCount[2,r]), collapse="")

        ## scatterplot between channels
        makePlot(file.path(basePath, subPath), con=con,
                 name=sprintf("scp_Rep%d", r), w=plsiz, h=plsiz, fun = function() {
                   par(mai=c(0.5,0.5,0.1,0.1))
                   ylim=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))
                   plot(x[,,r,1], x[,,r,2], pch=16, cex=0.5,
                        ylim=ylim, xlab="Channel 1", ylab="Channel 2", col=wellTypeColor[mtt[[r]]])
                   abline(a=0, b=1, col="lightblue")
                 }, print=FALSE)
        plotTable[r+3, 4] = sprintf("<CENTER>%s</CENTER><CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER>\n", wellLeg, sprintf("scp_Rep%d.pdf", r), sprintf("scp_Rep%d.png", r)) 
      } else {
        plotTable[r+3, 4] = sprintf("<CENTER>Replicate %d is missing in one of the channels: scatterplot omitted</CENTER>\n", r)
      }## if r
    }## for r
  }## if nrChannel

  plotTable[is.na(plotTable)] = ""

  writeHTMLtable4plots(plotTable, con=con)
  writetail(con)
  return(list(url=fn, qmsummary=qmsummary)) 
}## function
