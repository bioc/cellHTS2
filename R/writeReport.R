writehref = function(x, url, con)
  cat(sprintf("<A HREF=\"%s\">%s</A>", url, x), file=con)

writeheader = function(x, level, con)
    cat(sprintf("<HTML><HEAD><TITLE>%s</TITLE></HEAD>\n<BODY><CENTER><H%d>%s</H%d></CENTER>\n\n",
                as.character(x), as.integer(level), as.character(x), as.integer(level)), file=con)

writeExperimentHeader = function(xy, x, y, url, level, con)
    cat(sprintf("<HTML><HEAD><TITLE>%s</TITLE></HEAD>\n<BODY><CENTER><H%d>%s<A HREF=\"%s\">%s</A></H%d></CENTER>\n\n",
                as.character(xy), as.integer(level), as.character(x), url,  as.character(y), as.integer(level)), file=con)

writetail = function(con)
    cat(sprintf("<BR><HR>%s</HTML></HEAD>\n", date()), file=con)


writeHTMLtable = function(x, url, con,
  colors = c("#e0e0ff", "#d0d0f0", "#f0f0ff", "#e0e0f0"), center=FALSE, extra=NULL) {

  if(!is.data.frame(x))
    stop("'x' must be a data.frame")
  nr = nrow(x)
  nc = ncol(x)
  if(!missing(url)) {
    if(! (is.matrix(url) && is.character(url) && nrow(url)==nr && ncol(url)==nc))
      stop("'url' must be a character matrix of the same size as 'x'")
    for(j in 1:nc)
      x[, j] = ifelse(is.na(url[, j]), x[, j], sprintf("<A HREF=\"%s\">%s</A>", url[, j], x[, j]))
  }


  if(center) cat("<CENTER>\n", file=con)
if (!is.null(extra)){
nn = nc/length(extra)
cat("<TABLE border=0><TR>", paste(sprintf("<TH colspan=%d align=center BGCOLOR=\"%s\">%s</TH>", nn, rep(colors[1], length(extra)), extra), collapse=""), "</TR>\n", sep="", file=con)
cat("<TR>", paste(sprintf("<TH BGCOLOR=\"%s\">%s</TH>", colors[(1:nc)%%2+1], colnames(x)[1:nn]), collapse=""),"</TR>\n", sep="", file=con)
} else {cat("<TABLE border=0><TR>",
      paste(sprintf("<TH BGCOLOR=\"%s\">%s</TH>", colors[(1:nc)%%2+1], colnames(x)), collapse=""),
      "</TR>\n", sep="", file=con) }

  for(i in 1:nr)
#     cat("<TR>", paste(sprintf("<TD BGCOLOR=\"%s\" align=center>%s</TD>", colors[2*(i%%2)+(1:nc)%%2+1], x[i,]), collapse=""),
#         "</TR>\n", sep="", file=con)
    cat("<TR>", paste(sprintf("<TD BGCOLOR=\"%s\">%s</TD>", colors[2*(i%%2)+(1)%%2+1], x[i,1]), collapse=""), paste(sprintf("<TD BGCOLOR=\"%s\" align=center>%s</TD>", colors[2*(i%%2)+(2:nc)%%2+1], x[i,-1]), collapse=""),
        "</TR>\n", sep="", file=con)

  cat("</TABLE>\n", file=con)
  if(center) cat("</CENTER>\n", file=con)
}


writeHTMLtable4plots = function(x, con,
  colors = c("#e0e0ff", "#d0d0f0", "#f0f0ff", "#e0e0f0")) {

  nr = nrow(x)
  nc = ncol(x)

  cat("<CENTER><TABLE border=0><TR>",
      paste(sprintf("<TH BGCOLOR=\"%s\">%s</TH>", colors[(1:nc)%%2+1], names(x)), collapse=""),
      "</TR>\n", sep="", file=con)

  for(i in 1:nr) {
    cat("<TR>", paste(paste("<TD BGCOLOR=\"", colors[2*(i%%2)+(1:nc)%%2+1],
                            "\">", x[i,], "</TD>", sep=""), collapse=""),
        "</TR>\n", sep="", file=con)
         }
  cat("</TABLE><CENTER>\n", file=con)
}

##
 myUpdateProgress <- function(ti, tmax, si, smax){
       updateProgress(ti/tmax*100, sub=sprintf("step %s of %s", si, smax), autoKill=!TRUE)
 }



##----------------------------------------------------------------------------
writeReport = function(x,
  outdir=file.path(getwd(), name(x)),
  force=FALSE,
  plotPlateArgs=FALSE,
  imageScreenArgs=NULL,
  progressReport = interactive(),
  posControls,
  negControls) {



  ## consistency checks:

  if(!inherits(x, "cellHTS"))
    stop("'x' must be a 'cellHTS' object")

  if (!is.logical(progressReport))
    stop("'progressReport' must be a logical value.")

  if(is.logical(plotPlateArgs)) {
    if(plotPlateArgs)
      plotPlateArgs=list()
  } else {
    if(!is.list(plotPlateArgs))
      stop("'plotPlateArgs' must either be logical or a list.")
  }


  # dimensions 
  d <- dim(rawdata(x))
  nrWell    <- d[1]
  nrPlate   <- d[2]
  nrReplicate <- d[3]
  nrChannel <- as.numeric(ifelse(state(x)["normalized"], dim(normdata(x))[4], d[4]))


  ## Progress bar 
  ## Rough estimation of the total computation time that the function will take
  ## 1 = one time unit
## Steps inside writeReport:
# Step 1 - creating the output directory
# Step 2 - Controls annotation (only if state(x)["configured"]=TRUE)
# Step 3 - QC per plate & channel (only if state(x)["configured"]=TRUE)
# Step 4 - Add plate result files and write with overall QC results
# Step 5 - Per experiment QC
# Step 6 - topTable  (only if scored)
# Step 7 -  Screen-wide image plot (only if scored)

  if (progressReport){
    timeCounter=0
    timePerStep <- c(
      step1 = 1,
      step2 = 2,
      step3 = nrPlate*nrReplicate*nrChannel*(1+2*is.list(plotPlateArgs)),
      step4 = 0.1*sum(x@plateList$status=="OK") + 2*nrChannel*nrReplicate,
      step5 = 5*nrChannel*nrReplicate, 
      step6 = 2*nrChannel*nrReplicate,
      step7 = nrPlate*(2 + ifelse("map" %in% names(imageScreenArgs), imageScreenArgs$map, FALSE))
      )

    steps2Do <- names(timePerStep)[c(TRUE, rep(state(x)["configured"],2), TRUE, TRUE, rep(state(x)["scored"],2))]
    totalTime <- sum(timePerStep[steps2Do])
    nsteps <- length(steps2Do)

    require("prada")
    progress(title="cellHTS is busy", message = sprintf("\nCreating HTML pages for '%s'. \nState: \n%s \n%s", name(x), 
             paste(paste(names(state(x))[1:2], state(x)[1:2], sep="="), collapse=", "), 
             paste(paste(names(state(x))[3:4], state(x)[3:4], sep="="), collapse=", ")), sub=sprintf("step %s of %s", 1, nsteps))


    on.exit(killProgress(), add=TRUE)
  }


  ## Step 1) Creating the output directory
  ## See if output directory exists. If not, create. If yes, check if it is empty,
  ## and if not, depending on parameter 'force', throw an error or clean it up.
  if(file.exists(outdir)){
    if(!file.info(outdir)$isdir)
      stop(sprintf("'%s' must be a directory.", outdir))
    outdirContents = dir(outdir, all.files = TRUE)
    outdirContents = setdiff(outdirContents, c(".", ".."))

    if(length(outdirContents)>0) {
      if(!force)
        stop(sprintf("'%s' is not empty.", outdir))
      unlink(file.path(outdir, outdirContents), recursive=TRUE)
    } 
  } else {
    dir.create(outdir, recursive=TRUE)
  }

  indexFile = file.path(outdir, "index.html") 
  con = file(indexFile, "w")
  on.exit(close(con), add=TRUE)

  dir.create(file.path(outdir, "in"))

  ## Create header for the HTML report & add description file if 'x' is configured
  if(state(x)["configured"]) {
     nm = file.path("in", "Description.txt")
     writeLines(x@screenDesc, file.path(outdir, nm))
     writeExperimentHeader(paste("Experiment report for ", name(x)), "Experiment report for ", name(x), nm, 1, con)
  } else { 
     writeheader(paste("Experiment report for", name(x)), 1, con)
  }


  if(progressReport){
   stepNr = 1
   timeCounter <- timeCounter + timePerStep[paste("step",stepNr,sep="")]
   # print cumulative time for last step and print number of next step:
   myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do)+1, nsteps)
  }


  ## initializations
  twoWay=FALSE

  ## the overview table of the plate result files in the experiment,
  ##   plus the (possible) urls for each table cell
  exptab = x@plateList
  url = matrix(as.character(NA), nrow=nrow(exptab), ncol=ncol(exptab))
  colnames(url) = colnames(exptab)
  qmHaveBeenAdded = FALSE



## -------------------------
 if(state(x)["configured"]) {


  if(progressReport) {
    timeCounter=timeCounter+2
    updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
  }


  ##   -------  Step 2) Controls annotation ---------------
  if (!missing(posControls)) {
    ## checks
    if(!is(posControls, "list")){
      ## check
      checkControls(posControls, nrChannel, "posControls")
      ## see if there are different positive controls (e.g. with different strengths)
      aux <- unique(posControls)
      aux <- findControls(aux[!emptyOrNA(aux)], as.character(wellAnno(x)))
      namePos <- unique(sapply(aux, function(i) unique(as.character(wellAnno(x)[i]))))
      namePos <- sort(x@plateConf$Content[match(namePos, tolower(x@plateConf$Content))])
    } else {
      checkControls2W(posControls, len=nrChannel, name="posControls")
      twoWay=TRUE
      namePos = NULL
    }## else is list

  }else{## if !missing
    ## assumes the screen is a one-way assay
    posControls=as.vector(rep("^pos$", nrChannel))
    namePos = "pos"
  }

  if (!missing(negControls)) 
     checkControls(y=negControls, len=nrChannel, name="negControls") #check
  else  
     negControls <- as.vector(rep("^neg$", nrChannel))
  #---------------------------------------------------------------------------------------------



  if(progressReport){
   stepNr = 2
   timeCounter <- timeCounter + timePerStep[paste("step",stepNr,sep="")]
   # print cumulative time for last step and print number of next step:
   myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do)+1, nsteps)
  }



    ## Define the bins for the histograms (channel-dependent)
    brks = apply(if(state(x)["normalized"]) { normdata(x) } else { rawdata(x) },
      4, range, na.rm=TRUE)
    brks = apply(brks, 2, function(s) pretty(s, n=ceiling(nrWell/10))) 
    if(!is(brks, "list")) brks=split(brks, col(brks))
    ## put as list also for the case ch=1 or for the case when brks have equal length for each channel 


   ## Correct wellAnno information:
     ## ... by taking into account the wells that were flagged in the screen log file, 
     ## or even by the user manually in xraw. Besides the categories in wellAnno(x), it contains the category "flagged".
   xrawWellAnno = array(rep(wellAnno(x), times = prod(dim(rawdata(x))[3:4])), dim=dim(rawdata(x)))
   ## see which wells are flagged, excluding "empty" wells
   iflagged = as.logical(is.na(rawdata(x))*(wellAnno(x)!="empty"))
   xrawWellAnno[iflagged]="flagged"


  ## Create geneAnnotation info for the image maps:
  if(state(x)["annotated"]){
    if ("GeneSymbol" %in% names(geneAnno(x))) 
       geneAnnotation <- geneAnno(x)$GeneSymbol
    else
       geneAnnotation <- geneAnno(x)$GeneID
  }else{##else if annotated
    geneAnnotation <- rep(paste("well", x@plateConf$Well), nrPlate)
  }##else annotated


  ##   -------  Step 3)  QC per plate & channel ---------------
  for(p in 1:nrPlate){
      nm = p
      wh = with(x@plateList, which(Plate==p & status=="OK"))
      if(length(wh)>0) {
        dir.create(file.path(outdir, nm))
        if(state(x)["normalized"]) {
          datPlat = normdata(x)[, p,,, drop=FALSE]
          ## datPlat = normdata(x)[, p,, ch, drop=FALSE]
          whatDat = "normalized"
        } else {
          datPlat = rawdata(x)[, p,,, drop=FALSE]
          whatDat = "unnormalized"
        }

        genAnno <- geneAnnotation[nrWell*(p-1)+(1:nrWell)]

        res <- QMbyPlate(datPlat, as.character(wellAnno(x)[nrWell*(p-1)+(1:nrWell)]), x@pdim, 
          name=sprintf("Plate %d (%s)", p, whatDat),
          basePath=outdir, subPath=nm, plotPlateArgs=plotPlateArgs, brks = brks,
          finalWellAnno = xrawWellAnno[,p,,, drop=FALSE], posControls, negControls, 
          isTwoWay=twoWay, geneAnno=genAnno, namePos=namePos)

        url[wh, "status"] = res$url

        if(!qmHaveBeenAdded) {
          if(twoWay){
            TableNames = c(paste("Replicate dynamic range", c("(Activators)", "(Inhibitors)"), sep=" "), paste("Average dynamic range", c("(Activators)", "(Inhibitors)"), sep=" "), "Spearman rank correlation")
          }else{## if twoWay

            if(length(namePos)==1 && namePos=="pos") 
              TableNames = c("Replicate dynamic range", "Average dynamic range", "Spearman rank correlation")
            else
              TableNames = c(sprintf("Replicate dynamic range (%s)", namePos), 
              sprintf("Average dynamic range (%s)", namePos), "Spearman rank correlation")
          }## else twoWay
          url = cbind(url,  matrix(as.character(NA), nrow=nrow(url), ncol=length(TableNames)))

          for (j in TableNames) exptab[, j] = rep("", nrow(exptab))
          qmHaveBeenAdded = TRUE
        }## if !qmHaveBeenAdded
        whh = split(wh, exptab$Channel[wh])
 
        for(ch in 1:length(res$qmsummary)) { # Channels
          resCh = res$qmsummary[[ch]]
          whCh = whh[[ch]]
          selrep= exptab$Replicate[whCh]
          if(twoWay){
            for (jj in 1:length(TableNames))
               exptab[whCh, TableNames[jj]] = resCh[unique((jj<3)*(selrep+nrReplicate*(jj-1))) + (jj>2)*(nrReplicate*2 + jj-2)] 
                #"Replicate dynamic range (Activators)"
                #"Replicate dynamic range (Inhibitors)"
                #TableNames[3] "Average dynamic range (Activators)"
                #TableNames[4] "Average dynamic range (Inhibitors)"
                #TableNames[5] "Spearman rank correlation"

          }else{ #oneway

            for (jj in 1:(length(TableNames)-1))
              exptab[whCh, TableNames[jj]] = resCh[unique((jj<(length(namePos)+1))*(selrep + (nrReplicate+1)*(jj-1))) + (jj>length(namePos))*(nrReplicate + 1)*(jj-length(namePos))]
              exptab[whCh, TableNames[length(TableNames)]] = resCh[length(resCh)]
          }## else twoWay
        }## for channel
      }## if length w

# update the progress bar each time a plate is completed:
  if(progressReport){
   stepNr = 3
   timeCounter <- timeCounter + timePerStep[paste("step",stepNr,sep="")]/nrPlate
   myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do), nsteps)
  }

    }## for p plates


 # after completing all plates:
 if(progressReport){
   stepNr = 3
   #timeCounter <- timeCounter + timePerStep[paste("step",stepNr,sep="")]/nrPlate
   # print cumulative time for last step and print number of next step:
   myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do)+1, nsteps)
  }
}## if configured


  ##   -------  Step 4) Add plate result files and write with overall QC results -------------
  ##  Report pages per plate result file 
  wh = which(x@plateList$status=="OK")
  nm = file.path("in", names(x@intensityFiles))
  for(w in wh) {
    txt = x@intensityFiles[[w]]
    if(is.null(txt))
      stop(sprintf("Object 'x' is internally inconsistent, plate %d (%s) is supposedly OK but has no raw data file.",
                   as.integer(w), nm[w]))
    writeLines(txt, file.path(outdir, nm[w]))
    url[w, "Filename"] = nm[w]


   ### time for step4 : 0.1*sum(x@plateList$status=="OK") + 2*nrChannel*nrReplicate

   # update progress bar each time w is updated:
   if(progressReport){
     stepNr = 4
     timeCounter <- timeCounter + 0.1
     myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do), nsteps)
   }
 
  } # for w


  # write table with overall CQ results
  cat("<CENTER>", file=con)
  writeHTMLtable(exptab, url=url, con=con)
  cat("</CENTER><BR><BR>", file=con)

# End of step 4 - update progress bar
 if(progressReport){
   stepNr = 4
   timeCounter <- timeCounter + 2*nrChannel*nrReplicate
   # print cumulative time for last step and print number of next step:
   myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do)+1, nsteps)
  }





  ##   -------  Step 5)  Per experiment QC ---------------
  plotTable = QMexperiment(x, outdir, con, posControls, negControls, isTwoWay=twoWay, namePos=namePos)

 
 if(progressReport){
   stepNr = 5
   timeCounter <- timeCounter + timePerStep[paste("step",stepNr,sep="")]
   # print cumulative time for last step and print number of next step:
   myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do)+1, nsteps)
  }




  if(state(x)["scored"]) {

   if(state(x)["annotated"]) ttInfo = "Table of scored <BR> and annotated probes" else ttInfo = "Table of scored probes"

  ##   -------  Step 6)  topTable ---------------
    out <- getTopTable(x, file=file.path(outdir, "topTable.txt"), verbose=FALSE)


    if(progressReport){
       stepNr = 6
       timeCounter <- timeCounter + timePerStep[paste("step",stepNr,sep="")]
       # print cumulative time for last step and print number of next step:
       myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do)+1, nsteps)
    }

  ##   -------  Step 7)  Screen-wide image plot ---------------
    if ("map" %in% names(imageScreenArgs)) {
      mapx = imageScreenArgs$map 
      imageScreenArgs = imageScreenArgs[!names(imageScreenArgs) %in% "map"] 
    } else {
      mapx=FALSE  # DO NOT make the mapping by default (changed on 18.06.2007, because this can take lots of time when there are many plates)
    }

    res <- makePlot(outdir, con=con, name="imageScreen", w=7, h=7, psz=8,
                    fun = function(map=mapx)
                      do.call("imageScreen", args=append(list(object=x, map=map), imageScreenArgs)),
                    print=FALSE, isImageScreen=TRUE)

    count = nrow(plotTable)
    plotTable = rbind(plotTable, rep("", length=prod(ncol(plotTable)* 2))) 
    plotTable[count + 1, 2] = "<H3 align=center>Screen-wide image plot of the scored values</H3>"
    plotTable[count + 2, 1] = sprintf("<CENTER><A HREF=\"topTable.txt\"><em>%s</em></A></CENTER><BR>\n", ttInfo)

    if (is.null(res)) {
      plotTable[count + 2, 2] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", "imageScreen.pdf", "imageScreen.png")
    }else{
      res <- myImageMap(object=res$obj, tags=res$tag, "imageScreen.png")
      plotTable[count + 2, 2] = paste("<BR><CENTER>", res, "</CENTER><BR><CENTER>",
              "<A HREF=\"imageScreen.pdf\">enlarged version</A></CENTER>\n", sep="")
    }

  } ## if scored

  writeHTMLtable4plots(plotTable, con=con)


 if(progressReport){
   stepNr = 7
   timeCounter <- timeCounter + timePerStep[paste("step",stepNr,sep="")]
   # print cumulative time for last step and print number of next step:
   myUpdateProgress(timeCounter, totalTime, match(paste("step",stepNr,sep=""), steps2Do), nsteps)
  }

  writetail(con)
  return(indexFile)
}
