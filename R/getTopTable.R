## getTopTable.txt
## Function to obtain the topTable data.frame and export it as a txt file.

getTopTable <- function(x, file="topTable.txt", verbose=interactive()){

  if(!state(x)["scored"])
    stop("Please score 'object' (using the function 'summarizeReplicates').")

    d <- dim(normdata(x))
    nrWell <- d[1]
    nrPlate <- d[2]
    nrReplicate <- d[3]
    nrChannel <- d[4]

    ## Checks whether the number of channels has changed after normalization
    originalNrCh <- dim(rawdata(x))[4]
    w <- 1:length(scores(x))
    out <- data.frame(
      plate=1 + (w-1)%/%nrWell,
      position=1+(w-1)%%nrWell,
      score=scores(x), 
      wellAnno = wellAnno(x)
    )

  ## array that corrects the wellAnno information by taking into account the wells that were flagged in the screen log file, or even by the user manually in xraw. Besides the categories in wellAnno(x), it contains the category "flagged".
   xrawWellAnno = array(rep(wellAnno(x), times = prod(dim(rawdata(x))[3:4])), dim=dim(rawdata(x)))
 ## see which wells are flagged, excluding "empty" wells
   iflagged = as.logical(is.na(rawdata(x))*(wellAnno(x)!="empty"))
   xrawWellAnno[iflagged]="flagged"


    ## Include the normalized values
    out[sprintf("normalized_r%d_ch%d", 1:nrReplicate, 1:nrChannel)] = sapply(1:nrChannel, 
          function(i) round(getReplicatesMatrix(normdata(x), channel=i, na.rm=FALSE), 3))



    ## include also the final well annotation (after the screen log file)
    out[sprintf("xrawAnno_r%d_ch%d", 1:nrReplicate, 1:originalNrCh)] <- sapply(1:originalNrCh, 
            function(i) getReplicatesMatrix(xrawWellAnno, channel=i, na.rm=FALSE))

    ## include also the raw values for each replicate and channel	 
    out[sprintf("raw_r%d_ch%d", 1:nrReplicate, 1:originalNrCh)] <- sapply(1:originalNrCh, 
           function(i) getReplicatesMatrix(rawdata(x), channel=i, na.rm=FALSE))


    ## median between replicates (raw data) 
    if(nrReplicate>1) {
      out[sprintf("median_ch%d", 1:originalNrCh)] <- apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, 1:originalNrCh)], 1, 
                median, na.rm=TRUE)
      if(nrReplicate==2) { 
          ## Difference between replicates (raw data)
          out[sprintf("diff_ch%d", 1:originalNrCh)] = apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, 1:originalNrCh)], 1, diff)
        } else {
          ## average between replicates (raw data)
          out[sprintf("average_ch%d", 1:originalNrCh)] = apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, 1:originalNrCh)], 1, mean, na.rm=TRUE)
        } 
    }

    ## raw/plateMedian
    xn <- array(as.numeric(NA), dim=dim(rawdata(x)))
    for(p in 1:nrPlate) {
      samples <- (wellAnno(x)[(1:nrWell)+nrWell*(p-1)]=="sample")
      xn[,p,,] <- apply(rawdata(x)[,p,,,drop=FALSE], 3:4, function(j) j/median(j[samples], na.rm=TRUE))
    }

      out[sprintf("raw/PlateMedian_r%d_ch%d", 1:nrReplicate, 1:originalNrCh)] <- sapply(1:originalNrCh, 
          function(i) {
           signif(getReplicatesMatrix(xn, channel=i, na.rm=FALSE), 3)  })

    if(state(x)["annotated"]) {
      out = cbind(out, geneAnno(x))
      out = out[,!duplicated(tolower(names(out)))] 
    }

    ## Export everything to the file
    ## consider only the wells with sample and controls, at least for one of the replicates
    ##     toconsider = which(!apply(out[,grep("xrawAnno",names(out))], 1, function(u) all(u=="flagged") || any(u=="empty") || any(u=="other")))
    ##     toconsider = !is.na(out$score)
    ##     out = out[toconsider,]
    out = out[order(out$score, decreasing=TRUE), ]
    out$score = round(out$score, 2)
    write.tabdel(out, file=file)
    if(verbose) cat(sprintf("Saving 'topTable' list in file '%s'\n", file))
    return(out)
}
