## Correct wellAnno information:
## ... by taking into account the wells that were flagged in the screen log file, 
## or even manually by the user in the assayData slot. Besides the categories in
## wellAnno(x), it contains the category "flagged".
## Returns an array with corrected well annotation.
getArrayCorrectWellAnno <- function(x)
{
    wAnno <- wellAnno(x)
    xx <- Data(x)
    d <- dim(xx)
    correctedWellAnno <- array(rep(wAnno, times = prod(d[2:3])), dim=d)
    ## see which wells are flagged, excluding "empty" wells
    iflagged <- as.logical(is.na(xx)*(wAnno!="empty"))
    correctedWellAnno[iflagged]="flagged"
    return(correctedWellAnno)
}



## getTopTable function
## Function to obtain the topTable data.frame and export it as a txt file.
getTopTable <- function(cellHTSlist, file="topTable.txt", verbose=interactive())
{
    ## arguments:
    ## 'cellHTSlist' should be a list of cellHTS object(s) obtained from the same
    ## experimental data. Allowed components are:
    ## 'scored' - (mandatory) cellHTS object comprising scored data.
    ## 'raw' - (optional) cellHTS object containing raw experiment data.
    ## 'normalized' - (optional) cellHTS object containing normalized data.
    ## cellHTSlist=list("scored"=xsc, "raw"=xr, "normalized"=xn)

    mandatoryComps <- c("scored")
    optionalComps <- c("raw", "normalized") 

    ## A lot of sanity checking up front
    if(!is.list(cellHTSlist))
    {
        stop("Argument 'cellHTSlist' should be a list containing one or a ",
             "maximum of 3 'cellHTS' objects.") 
    }
    else
    {
        if(!all(sapply(cellHTSlist, class)=="cellHTS"))
            stop("Argument 'cellHTSlist' should be a list of cellHTS objects!")
        nm <- names(cellHTSlist)
        if(!(mandatoryComps %in% nm))
            stop("Argument 'cellHTSlist' should be a list containing at least ",
                 "one component named 'scored' that corresponds to a scored ",
                 "'cellHTS' object.")
        if( length(cellHTSlist)>3 | any(duplicated(nm)) )
            stop("Argument 'cellHTSlist' can only have a maximum of 3 components ",
                 "named 'raw', 'normalized' and 'scored'!")
        if(!all(nm %in% c(optionalComps, mandatoryComps))) 
            stop(sprintf("Invalid named component%s in argument 'cellHTSlist': %s", 
                         ifelse(sum(!(nm %in% c(optionalComps, mandatoryComps)))>1, "s", ""), 
                         nm[!(nm %in% c(optionalComps, mandatoryComps))])) 
    }

    xr <- cellHTSlist[["raw"]]
    xn <- cellHTSlist[["normalized"]]
    xsc <- cellHTSlist[["scored"]]
    
    ## now check whether the given components of 'cellHTSlist' are valid cellHTS objects:
    if(!(state(xsc)[["scored"]]))
        stop(sprintf(paste("The component 'scored' of argument list 'cellHTSlist' should be ",
                           "a 'cellHTS' object containing scored data!\nPlease check its ",
                           "preprocessing state: %s"), paste(names(state(xsc)), "=",
                                                             state(xsc), collapse=", ")))
    if(!is.null(xr))
    {
        if(any(state(xr)[c("normalized", "scored")]))
            stop(sprintf(paste("The component 'raw' of argument list 'cellHTSlist' ",
                               "should be a 'cellHTS' object containing raw data!",
                               "\nPlease check its preprocessing state: %s"),
                         paste(names(state(xr)), "=", state(xr), collapse=", ")))
        if(!compare2cellHTS(xsc, xr))
            stop("Difference across 'cellHTS' objects! The scored 'cellHTS' ",
                 "object given in cellHTSlist[['scored']] was not calculated ",
                 "from the data stored in 'cellHTS' object indicated in ",
                 "'cellHTSlist[['raw']]'!")
    }
    if(!is.null(xn))
    {
        if(!(state(xn)[["normalized"]] & !state(xn)[["scored"]]))
            stop(sprintf(paste("The component 'normalized' of argument list 'cellHTSlist' ",
                               "should be a 'cellHTS' object containing normalized data!",
                               "\nPlease check its preprocessing state: %s"),
                         paste(names(state(xn)), "=", state(xn), collapse=", ")))
        if(!compare2cellHTS(xsc, xn))
            stop("'cellHTS' objects contained in cellHTSlist[['scored']] and ",
                 "cellHTSlist[['normalized']] are not from the same experiment!")
    }

    xraw <- if(is.null(xr)) xr else Data(xr)
    xnorm <- if(is.null(xn)) xn else Data(xn)
    scores <- Data(xsc)
    d <- if(is.null(xn)) dim(scores) else dim(xnorm)
    nrWell <- prod(pdim(xsc))
    nrPlate <- max(plate(xsc))
    nrReplicate <- d[2]
    nrChannel <- d[3]
    wAnno <- wellAnno(xsc)
    

    ## array with corrected wellAnno information (by taking into account the wells
    ## that were flagged in the screen log file, or even manually by the user).
    ## Besides the categories in wellAnno(x), it contains the category "flagged".
    scoresWellAnno <- getArrayCorrectWellAnno(xsc)
    
    ## include also the final well annotation (after the screen log file)
    out <- data.frame(plate=plate(xsc),
                      position=position(xsc),
                      well=well(xsc),
                      score=as.vector(Data(xsc)), 
                      wellAnno = wAnno,
                      finalWellAnno = as.vector(scoresWellAnno))

    ## add columns with 
    if(!is.null(xraw))
    {
        ## Checks whether the number of channels has changed after normalization
        originalNrCh <- dim(xraw)[3]
        
        ## include also the raw values for each replicate and channel	 
        out[sprintf("raw_r%d_ch%d", rep(1:nrReplicate, originalNrCh),
                    rep(1:originalNrCh, each=nrReplicate))] <- sapply(1:originalNrCh, 
                                                                      function(i) xraw[,,i])
        for(ch in 1:originalNrCh)
        {
            ## median between replicates (raw data) 
            if(nrReplicate>1) {
                out[sprintf("median_ch%d", ch)] <- apply(out[sprintf("raw_r%d_ch%d",
                                                                     1:nrReplicate,
                                                                     rep(ch, nrReplicate))],
                                                         1, median, na.rm=TRUE)
                if(nrReplicate==2)
                { 
                    ## Difference between replicates (raw data)
                    out[sprintf("diff_ch%d", ch)] <- apply(out[sprintf("raw_r%d_ch%d",
                                                                       1:nrReplicate,
                                                                       rep(ch, nrReplicate))],
                                                           1, diff)
                }
                else
                {
                    ## average between replicates (raw data)
                    out[sprintf("average_ch%d", ch)] <- apply(out[sprintf("raw_r%d_ch%d",
                                                                          1:nrReplicate,
                                                                          rep(ch, nrReplicate))],
                                                              1, mean, na.rm=TRUE)
                } 
            }
        }## for ch
        
        ## raw/plateMedian
        xrp <- array(as.numeric(NA), dim=dim(xraw))
        isSample <- (as.character(wAnno) == "sample")
        for(p in 1:nrPlate)
        {
            indp <- (1:nrWell)+nrWell*(p-1)
            samples <- isSample[indp]
            xrp[indp,,] <- apply(xraw[indp,,,drop=FALSE], 2:3,
                                 function(j) j/median(j[samples], na.rm=TRUE))
        }

        out[sprintf("raw/PlateMedian_r%d_ch%d", rep(1:nrReplicate, originalNrCh),
                    rep(1:originalNrCh, each=nrReplicate))] <-
                        sapply(1:originalNrCh, 
                               function(i) signif(xrp[,,i], 3))
    }

    if(!is.null(xnorm))
    {
        ## Include the normalized values
        out[sprintf("normalized_r%d_ch%d", rep(1:nrReplicate, nrChannel),
                    rep(1:nrChannel, each=nrReplicate))] <-
                        sapply(1:nrChannel, 
                               function(i) round(xnorm[,,i], 3))
    }
    
    if(state(xsc)[["annotated"]])
    {
        n <- tolower(names(fData(xsc)))
        sel <- !(n %in% tolower(c("controlStatus", names(out))))
        out <- cbind(out, fData(xsc)[, sel, drop=FALSE])
    }

    ## Export everything to the file
    out <- out[order(out$score, decreasing=TRUE), ]
    out$score <- round(out$score, 2)
    if(!is.null(file))
    {
        if(verbose)
            cat(sprintf("Saving 'topTable' list in file '%s'\n", file))
        write.tabdel(out, file=file)
    }
    return(out)
}
