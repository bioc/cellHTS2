## cellHTSlist verification, stop if the verification fails, else return the list
## FIXME: the 'cellHTSlist' argument is deprecated, however it is still referenced
##        in many of the error messages. Needs to be fixed once 'cellHTSlist' is
##        gone for good.
cellHTSlistVerification <- function(xr, xn, xsc, cellHTSlist)
{
    ## The cellHTSlist argument is deprecated in favour of the new
    ## separate arguments 'raw', 'normalized' and 'scored'
    if(is.null(cellHTSlist))
    {
        cellHTSlist <- list()
        if(!is(xr, "cellHTS"))
            stop("Argument 'raw' has to be a cellHTS object.")
        cellHTSlist$raw <- xr
        if(!is.null(xn))
        {
            if(!is(xn, "cellHTS"))
                stop("Argument 'normalized' has to be a cellHTS object or NULL.")
            cellHTSlist$normalized <- xn
        }
        if(!is.null(xsc))
            if(!is(xsc, "cellHTS"))
                stop("Argument 'scored' has to be a cellHTS object or NULL.")
            cellHTSlist$scored <- xsc
    }
    else
    {
        .Deprecated(msg=paste("The 'cellHTSlist' argument is deprecated.\nPlease provide all",
                    "necessary cellHTS objects separately via the 'raw', 'normalized' and",
                    "'scored' arguments`"))
        xr <- cellHTSlist[["raw"]]
        xn <- cellHTSlist[["normalized"]]
        xsc <- cellHTSlist[["scored"]] 
    }
    ## cellHTSlist verifications
    allowedListNames <- c("raw", "normalized", "scored")
    if(!is.list(cellHTSlist))
        stop("Argument 'cellHTSlist' should be a list containing one or a maximum of ",
             "3 'cellHTS' objects.")
    if(!all(sapply(cellHTSlist, is, "cellHTS")))
        stop("Argument 'cellHTSlist' should be a list of cellHTS objects!")
    nm <- names(cellHTSlist)
    if(!("raw" %in% nm)) 
        stop("Argument 'cellHTSlist' should be a list containing at least ",
             "one component named 'raw' that corresponds to a 'cellHTS' object ",
             "containing unnormalized data.")
    if(length(cellHTSlist)>3 | any(duplicated(nm)))
        stop("Argument 'cellHTSlist' can only have a maximum of 3 components named ",
             "'raw', 'normalized' and 'scored'!")
    if(!all(nm %in% allowedListNames))
        stop(sprintf("Invalid named component%s in argument 'cellHTSlist': %s",
                     ifelse(sum(!(nm %in% allowedListNames))>1, "s", ""),
                     nm[!(nm %in% allowedListNames)]))
    ## now check whether the given components of 'cellHTSlist' are valid cellHTS objects:	
    if(any(state(xr)[c("scored", "normalized")]))
        stop(sprintf(paste("The component 'raw' of argument 'cellHTSlist' should be a",
                     "'cellHTS' object containing unnormalized data!\nPlease check",
                     "its preprocessing state: %s"), paste(names(state(xr)), "=",
                                                          state(xr), collapse=", ")))  
    if(!is.null(xn))
    {
        if(!(state(xn)[["normalized"]] & !state(xn)[["scored"]]))
            stop(sprintf(paste("The component 'normalized' of 'cellHTSlist' should be a",
                               "'cellHTS' object containing normalized data!\nPlease check",
                               "its preprocessing state: %s"), paste(names(state(xn)), "=",
                                                              state(xn), collapse=", ")))
        if(!compare2cellHTS(xr, xn))
            stop("'cellHTS' objects contained in cellHTSlist[['raw']] and ",
                 "cellHTSlist[['normalized']] are not from the same experiment!")
    }
    if(!is.null(xsc))
    {
        if(!state(xsc)["scored"])
            stop(sprintf(paste("The component 'scored' of 'cellHTSlist' should be a",
                               "'cellHTS' object containing scored data!\nPlease check",
                               "its preprocessing state: %s"),
                         paste(names(state(xsc)), "=", state(xsc), collapse=", ")))
        if(!compare2cellHTS(xr, xsc))
            stop("Difference across 'cellHTS' objects! The scored 'cellHTS' object given ",
                 "in cellHTSlist[['scored']] was not calculated from the data stored in ",
                 "the 'cellHTS' object in 'cellHTSlist[['raw']]'!")
        if(is.null(xn))
            stop("Please add to 'cellHTSlist' list a component named 'normalized' ",
                 "corresponding to a cellHTS object containing the normalized data!") 
    }
    return(cellHTSlist)
}




## plotPlateArgs verification
plotPlateArgsVerification <- function(plotPlateArgs, map)
{
    if(is.null(plotPlateArgs))
        plotPlateArgs <- FALSE
    else
        warning("The plotPlateArgs argument is deprecated. Please use the settings ",
                "infrastructure as explained in '?setSettings'.", call.=FALSE)
    if(is.logical(plotPlateArgs))
    {
        if(plotPlateArgs)
            chtsSetSetting(list(plateList=list(reproducibility=list(include=TRUE, map=map),
                                intensities=list(include=TRUE, map=map),
                                average=list(include=TRUE, map=map))))
    }
    else
    {
        if(!is.list(plotPlateArgs))
            stop("'plotPlateArgs' must either be logical or a list.") 
        if(!all(names(plotPlateArgs) %in% c("sdcol", "sdrange", "xcol", "xrange", "map")))
            stop("Only elements 'sdcol', 'sdrange', 'xcolx' and 'xrange' are allowed for ",
                 "'plotPlateArgs' list!")
        plotPlateArgs$map <- map
        chtsSetSetting(list(plateList=list(reproducibility=c(list(include=TRUE),
                                           plotPlateArgs), 
                            intensities=c(list(include=TRUE, plotPlateArgs)),
                            average=c(list(include=TRUE, plotPlateArgs)))))    
    }
    return(invisible())
}




## imageScreenArgs verification
imageScreenArgsVerification <- function(imageScreenArgs, map, ar=1)
{	
    if(is.list(imageScreenArgs))
    {
        if(!all(names(imageScreenArgs) %in% c("ar", "zrange", "map","anno"))) 
            stop("Only elements 'ar', 'zrange', 'map' and 'anno'are allowed for ",
                 "'imageScreenArgs' list!")
        if(!("map" %in% names(imageScreenArgs)))
            imageScreenArgs$map <- map
        if(!("ar" %in% names(imageScreenArgs)))
        {
            imageScreenArgs$aspect <- ar
        }
        else
        {
            imageScreenArgs$aspect <- imageScreenArgs$ar
            imageScreenArgs$ar <- NULL
        }
        chtsSetSetting(list(screenSummary=list(scores=imageScreenArgs)))
        warning("The imageScreenArgs argument is deprecated. Please use the settings ",
                 "infrastructure as explained in '?setSettings'.", call.=FALSE)
    }
    else
    {
        if(!is.null(imageScreenArgs)) 
            stop("'imageScreenArgs' must either be a list or NULL.")
        chtsSetSetting(list(screenSummary=list(scores=list(map=map))))
    }	
    return(invisible())
}




## create Output folder after ensuring that there will not be accidental file deletion
createOutputFolder <- function(outdir, xr, force)
{
    ## See if output directory exists. If not, create. If yes, check if it is empty,
    ## and if not, depending on parameter 'force', throw an error or clean it up.	
    if(missing(outdir))
    {
        if(force)
            stop("To prevent accidental deletion of files, please specify 'outdir' ",
                 "explicitely if you want to use the 'force=TRUE' option.")
        outdir <- file.path(getwd(), name(xr))
    }
    if(file.exists(outdir))
    {
        if(!file.info(outdir)$isdir)
            stop(sprintf("'%s' must be a directory.", outdir))
        outdirContents <- dir(outdir, all.files=TRUE)
        outdirContents <- setdiff(outdirContents, c(".", ".."))  
        if(!force && length(outdirContents)>0)
            stop(sprintf("'%s' is not empty.", outdir))
    }
    else
    {
        dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
    }
    ## create "in" and "html" folder
    dir.create(file.path(outdir, "in"), showWarnings=FALSE)
    dir.create(file.path(outdir, "html"), showWarnings=FALSE)
    return(outdir)
}



## This function produces the HTML report from the cellHTS object(s) in the output folder.
## The entry point to the report is the file "index.html" and should be viewed with a web
## browser
## NOTE: 'writeReport' can be called on different cellHTS objects at different preprocessing
## stages, and the output will differ
## Arguments:
##    cellHTSlist: should be a list of cellHTS object(s) obtained for the same experimental
##                 data. Allowed components are:
##                 'raw' - (mandatory) cellHTS object containing raw experiment data.
##                 'normalized' (mandatory only if component 'scored' is given)- cellHTS object
##                              containing normalized data.
##                 'scored' - cellHTS object comprising scored data.
##                 NOTE: the argument is deprecated, the list items should now be supplied
##                       via separate arguments of the same names
##                       e.g. writeReport(raw=xr, normalized=xn, scored=xsc)
##
## Steps inside writeReport:
##    Step 1 - creating the output directory
##    Step 2 - Controls annotation (only if overallState["configured"]=TRUE)
##    Step 3 - QC per plate & channel (only if overallState(x)["configured"]=TRUE)
##    Step 4 - Add plate result files, main script, and write the overall QC results
##             in the 'in' folder of the report' 
##    Step 5 - Per experiment QC
##    Step 6 - topTable  (only if scored data are available)
##    Step 7 - Screen-wide image plot (only if scored data are available)	
writeReport <- function(raw, normalized=NULL, scored=NULL, cellHTSlist=NULL, outdir,
                        force=FALSE, map=FALSE, plotPlateArgs=NULL, imageScreenArgs=NULL,
                        posControls, negControls, mainScriptFile=NA, gseaModule=NULL,
                        settings=list())
{
    ## Verification of the arguments
    ## We are very particular about the values in cellHTSlist
    cellHTSlist <- cellHTSlistVerification(xr=raw, xn=normalized, xsc=scored,
                                           cellHTSlist=cellHTSlist)
    if (!is.logical(map))
        stop("'map' must be a logical value.")

    ## Deal with settings
    oldSettings <- chtsGetSetting()
    on.exit(chtsSettings[["report"]] <- oldSettings)
    chtsSetSetting(settings)
    
    ## Initialization
    nm <- names(cellHTSlist)
    xr <- cellHTSlist[["raw"]]
    xn <- cellHTSlist[["normalized"]]
    xsc <- cellHTSlist[["scored"]] 	
    xraw <- Data(xr)  ## xraw should always be given!
    xnorm <- if(is.null(xn)) xn else Data(xn)
	
    ## dimensions 
    d <- as.integer(dim(xraw))
    nrWell    <- prod(pdim(xr))
    nrPlate   <- max(plate(xr))
    nrReplicate <- as.numeric(d[2])
    ## will be defined based on xnorm, if it exists
    nrChannel <- if(!is.null(xnorm)) as.integer(dim(xnorm)[3]) else d[3] 
    objState <- sapply(cellHTSlist, function(i) if(!is.null(i)) state(i))	
    overallState <- apply(objState, 1, any)
    if(overallState["configured"] && ! state(xr)["configured"])
       stop("All cellHTS2 object must be configured, object '", deparse(substitute(raw)),
            "' is not.")
    whAnnotated <- colnames(objState)[objState["annotated",]]

    ## get appropriate data
    if(overallState["normalized"])
    {
        dat <- xnorm
        whatDat<-"normalized"
    }
    else
    {
        dat <- xraw
        whatDat <- "unnormalized"
    }		
		
    ## initializations
    twoWay <- FALSE
    wAnno <- as.character(wellAnno(xr))
	
    ## The overview table of the plate result files in the experiment,
    ## plus the (possible) urls for each table cell
    ## We want the columns in a particular order
    exptab <- plateList(xr)
    mt <- match(c("Plate", "Replicate", "Channel", "Filename"), colnames(exptab))
    exptab <- cbind(exptab[,mt], exptab[,-mt, drop=FALSE])
    url <- matrix(as.character(NA), nrow=nrow(exptab), ncol=ncol(exptab))
    colnames(url) <- colnames(exptab)
    qmHaveBeenAdded <- FALSE
    ## FIXME: This should go away after the next release since all settings
    ## are handled by the chtsSettings infrastructure.
    plotPlateArgsVerification(plotPlateArgs, map)
    imageScreenArgsVerification(imageScreenArgs, map, ar=pdim(xr)[1]/pdim(xr)[2])

    ## Set up the progress report and status output
    progress <- createProgressList(nrReplicate, nrChannel, nrPlate, plotPlateArgs,
                                   xr, overallState)
    dname <- if(length(cellHTSlist)>1) paste(paste(nm[-length(cellHTSlist)],
                                                   collapse=", "), "and",
                                             nm[length(cellHTSlist)],  collapse=" ") else nm 
    cat(sprintf("cellHTS2 is busy creating HTML pages for '%s'. \nFound %s data.\nState:\n%s\n",
                name(xr), dname, paste(paste("configured", overallState[["configured"]],
                                             sep="="),
                                       paste("annotated", overallState[["annotated"]],
                                             sep="="), sep=", ")))
    progress <-  myUpdateProgress(progress, "step0")

    ## Step 1 : Creating the output directory and write the screen description if present	
    outdir <- createOutputFolder(outdir, xr, force)	
    if(overallState["configured"])
    {
        nm <- file.path("in", "Description.txt")
        writeLines(screenDesc(xr), file.path(outdir, nm))
        progress <-  myUpdateProgress(progress, "step1")
        
        ## Step 2 : Controls annotation
        ## check, determine assay type and name of positive controls if assay is one-way
        if (!missing(posControls))
        {
            namePos <- checkPosControls(posControls, nrChannel, wAnno, plateConf(xr)$Content)
            twoWay <- namePos$twoWay
            namePos <- namePos$namePos 
        }
        else
        {
            ## this assumes the screen is a one-way assay
            posControls <- as.vector(rep("^pos$", nrChannel))
            namePos <- "pos"
        }    
        if (!missing(negControls))
        {
            checkControls(y=negControls, len=nrChannel, name="negControls")
        }
        else
        {  
            negControls <- as.vector(rep("^neg$", nrChannel))
        }
   
        ## Define the bins for the histograms (channel-dependent)
        brks <- apply(if(overallState["normalized"]) xnorm else xraw, 3, range, na.rm=TRUE,
                      finite=TRUE)
        brks <- apply(brks, 2, function(s) pretty(s, n=ceiling(nrWell/10)))
        ## Coerce to list also for the case ch=1 or for the case when brks have equal length
        ## for each channel 
        if(!is.list(brks))
            brks <- split(brks, col(brks))
    
        ## Correct wellAnno information:
        ## by taking into account the wells that were flagged in the screen log file, 
        ## or even by the user manually in xraw. Besides the categories in wellAnno(x), it
        ## contains the category "flagged".
        xrawWellAnno <- getArrayCorrectWellAnno(xr)
        ## coerce to array with dimensions nrWells x nrPlates x nrReplicates x nrChannels
        ## don't use variable 'nrChannel' because it may be different when defined based
        ## on xnorm data!
        xrawWellAnno <- array(xrawWellAnno, dim=c("Wells"=nrWell, "Plates"=nrPlate,
                                            nrReplicate, dim(xrawWellAnno)[3])) 
        ## Create geneAnnotation info for the image maps:
        geneAnnotation <- if(overallState["annotated"])
        {
            ## follow the order 'scored' - 'normalized' - 'raw'
            for(i in c("scored", "normalized", "raw")) {
                if(i %in% whAnnotated) {
                    screenAnno <- fData(cellHTSlist[[i]]) 
                    break
                }
            }      
            if ("GeneSymbol" %in% names(screenAnno)) screenAnno$GeneSymbol else
            screenAnno$GeneID
        }
        else
        {
            well(xr)
        }
        
        ## which of the replicate plates has not just NA values
        datPerPlate <- array(dat, dim=c("Wells"=nrWell, "Plates"=nrPlate, nrReplicate,
                                  nrChannel))
        ## nrPlates x nrReplicates x nrChannels		
        hasData <- apply(datPerPlate, 2:4, function(z) !all(is.na(z))) 
    
        ## Get controls positions (for all plates)
        allControls <- getControlsPositions(posControls, negControls, twoWay, namePos,
                                            nrChannel, wAnno)
        actCtrls <- allControls$actCtrls
        inhCtrls <- allControls$inhCtrls
        posCtrls <- allControls$posCtrls
        negCtrls <- allControls$negCtrls		
        
        ## get controls positions for each plate
        act <- lapply(actCtrls, function(i) if(is.null(i)) NULL else ctrlsPerPlate(i, nrWell))
        inh <- lapply(inhCtrls, function(i) if(is.null(i)) NULL else ctrlsPerPlate(i, nrWell))
        neg <- lapply(negCtrls, function(i) if(is.null(i)) NULL else ctrlsPerPlate(i, nrWell))
        pos <- vector("list", length=nrChannel)
        for (ch in 1:nrChannel)
        {
            notNull <- !sapply(posCtrls[[ch]], is.null)
            if(any(notNull))
            {
                pp <- posCtrls[[ch]][notNull]
                pos[[ch]] <- lapply(pp, ctrlsPerPlate, nrWell)
            } 
        }		
		
        ## Get per-plate dynamic range,  per-plate repeatability standard deviation
        ## (plate replicates), Z'-factor for each replicate and channel (needed as input
        ## for QMexperiment later on)
        if(whatDat=="normalized")
        {
            dr <- getDynamicRange(xn, verbose=FALSE, posControls=posControls,
                                  negControls=negControls)
            repMeasures <- getMeasureRepAgreement(xn, corr.method="spearman")
            allZfac <- getZfactor(xn, verbose=FALSE, posControls=posControls,
                                  negControls=negControls) 
        }
        else
        { ## use cellHTS object containing raw data
            dr <- getDynamicRange(xr, verbose=FALSE, posControls=posControls,
                                  negControls=negControls)
            repMeasures <- getMeasureRepAgreement(xr, corr.method="spearman")
            allZfac <- getZfactor(xr, verbose=FALSE, posControls=posControls,
                                  negControls=negControls) 			
        }		
        if(all(is.null(names(dr))))
            names(dr) <- namePos
		
        ## Define well colors and comment on them.
        ## (to avoid having the legend for 'pos' when we have 'inhibitors' and 'activators'
        ## or vice-versa)
        wellTypeNames <- c("sample", "neg", "controls", "other", "empty", "flagged",
                           if(twoWay) c("act", "inh") else "pos")
        #colPal <- brewer.pal(9, "Set1")
        #wellTypeColor <- c("black", colPal[c(2:4, 5, 7)], if(twoWay) colPal[c(1,6)] else
        #                   colPal[1])
        #names(wellTypeColor) <- wellTypeNames
                   	
        ##  Step 3 : QC per plate & channel
	## writes a report for each Plate, and prepare argument for the writing of the table
        ## with overall CQ results
        allmt <- match(wAnno, wellTypeNames)		
        for(p in 1:nrPlate)
        {
            wh <- with(plateList(xr), which(Plate==p & Status=="OK"))
            if(length(wh)>0) {
                dir.create(file.path(outdir, p), showWarnings=FALSE)
                ## QMbyPlate also writes the QC report for the current plate with making res
                channelNames <- if(!is.null(xn)) channelNames(xn) else channelNames(xr)
                res <- QMbyPlate(platedat=datPerPlate[, p,,, drop=FALSE], 
                                 pdim=pdim(xr), 
                                 name=sprintf("Plate %d <i><small>(%s)</small></i>", p, whatDat),
                                 channelNames=channelNames,
                                 basePath=outdir, 
                                 subPath=p, 
                                 genAnno=geneAnnotation[nrWell*(p-1)+(1:nrWell)], 
                                 mt=allmt[nrWell*(p-1)+(1:nrWell)],
                                 brks=brks,
                                 finalWellAnno=xrawWellAnno[,p,,, drop=FALSE], 
                                 activators=act, inhibitors=inh, positives=pos, negatives=neg, 
                                 isTwoWay=twoWay, namePos=namePos, wellTypeNames=wellTypeNames,
                                 plateDynRange=lapply(dr, function(i) i[p,,,drop=FALSE]), 
                                 plateWithData=hasData[p,,, drop=FALSE],repMeasures=repMeasures)
                url[wh, "Status"] <- res$url				
                if(!qmHaveBeenAdded)
                {
                    TableNames <-
                        unique(
                               if(twoWay)
                           {
                               c(paste("Replicate dynamic range",
                                       c("(Activators)", "(Inhibitors)"), sep=" "),
                                 paste("Average dynamic range",
                                       c("(Activators)", "(Inhibitors)"), sep=" "),
                                 "Spearman rank correlation")
                           }
                               else
                           {
                               if(length(namePos)==1 && namePos=="pos")
                               { 
                                   c("Replicate dynamic range",
                                     "Average dynamic range", "Repeatability standard deviation",
                                     sprintf("Spearman rank correlation %s",
                                             ifelse(nrReplicate==2, "", "(min - max)")))
                               }
                               else
                               {
                                   c(sprintf("Replicate dynamic range (%s)", namePos), 
                                     sprintf("Average dynamic range (%s)", namePos),
                                     "Repeatability standard deviation", 
                                     sprintf("Spearman rank correlation %s",
                                             ifelse(nrReplicate==2, "", "(min - max)")))
                               }
                           })
                    url <- cbind(url,  matrix(as.character(NA), nrow=nrow(url),
                                              ncol=length(TableNames)))
                    for(j in TableNames)
                        exptab[, j] <- rep("", nrow(exptab))
                    qmHaveBeenAdded <- TRUE
                }
                whh <- split(wh, exptab$Channel[wh])
                for(ch in 1:length(res$qmsummary))
                { ## Channels
                    resCh <- res$qmsummary[[ch]]
                    whCh <- whh[[ch]]
                    selrep <- exptab$Replicate[whCh]
                    if(twoWay)
                    {
                        for (jj in 1:length(TableNames))
                        {
                            sel <- (unique((jj<3)*(selrep+nrReplicate*(jj-1))) +
                                    (jj>2)*(nrReplicate*2 + jj-2))
                            exptab[whCh, TableNames[jj]] <- resCh[sel]
                        }
                        ##"Replicate dynamic range (Activators)"
                        ##"Replicate dynamic range (Inhibitors)"
                        ##TableNames[3] "Average dynamic range (Activators)"
                        ##TableNames[4] "Average dynamic range (Inhibitors)"
                        ##TableNames[5] "Repeatability standard deviation"
                        ##TableNames[6] "Spearman rank correlation"				
                    }
                    else
                    { ##oneway
                        for (jj in 1:(length(TableNames)-2))
                        {
                            ## exclude "Repeatability standard deviation" and "Spearman rank
                            ## correlation"
                            sel <- (unique((jj<(length(namePos)+1))*
                                           (selrep + (nrReplicate+1)*(jj-1))) +
                                    (jj>length(namePos))*(nrReplicate + 1)*(jj-length(namePos)))
                            exptab[whCh, TableNames[jj]] <- resCh[sel]
                        }
                        exptab[whCh, TableNames[length(TableNames)-1]] <- resCh[length(resCh)-1]
                        exptab[whCh, TableNames[length(TableNames)]] <- resCh[length(resCh)]
                    }## else twoWay
                }## for channel
            }## if length w			
            ## update the progress bar each time a plate is completed. Once the computation
            ## has been done for every Plate, step 3 is completed
            progress <- myUpdateProgress(progress, "step2",
                                         progress$timePerStep["step2"]/nrPlate)
        }## for p plates				
    }## if configures
    else
    {
        ## We need these variables to pass down to the modules, where
        ## the conditional evaluation based on configuration status
        ## etc takes place
        posControls <- negControls <- allControls <- allZfac <- NULL
    }

    ## copying all necessary files into the html folder (css, javascripts, gifs) 
    cpfiles <- dir(system.file("templates", package="cellHTS2"),
                   full.names=TRUE)
    htmldir <- file.path(outdir, "html")
    cpfiles <- path.expand(cpfiles)
    htmldir <-  path.expand(htmldir)
    ## On some systems the path expansion in file.copy seems to be broken, hence we test here
    if(length(grep("~", cpfiles)) || length(grep("~", htmldir)))
        warning("Path expansion seems to be broken. Please provide fully qualified direcroty paths (i.e., avoiding '~')")
    file.copy(from=cpfiles, to=htmldir, overwrite=TRUE)
    ## saving the glossary as a web page. createGlossary() returns a glossary with all
    ## the definitions 	
    saveHtmlGlossary(parseGlossaryXML(), file.path(htmldir, 'glossary.html'))	

    ## The 'Plate List' module: this is a matrix of quality metrics for the different
    ## plates and linked per plate quality reports. The workhorse function to produce
    ## the necessary HTML code is 'writeHtml.plateList'.
    wh <- which(plateList(xr)$Status=="OK")
    nm <- file.path("in", names(intensityFiles(xr)))
    expOrder <- order(exptab[["Plate"]], exptab[["Channel"]], exptab[["Replicate"]])
    url[wh, "Filename"] <- nm[wh]
    plateList.module <- chtsModule(cellHTSlist, url=file.path(htmldir, "plateList.html"),
                                   htmlFun=writeHtml.plateList, title="Plate List",
                                   funArgs=list(center=TRUE,
                                   links=url[expOrder,,drop=FALSE],
                                   exptab=exptab[expOrder,,drop=FALSE],
                                   outdir=outdir, htmldir=htmldir,
                                   expOrder=expOrder,
                                   configured=overallState["configured"]))
    tab <- writeHtml(plateList.module)
    progress <- myUpdateProgress(progress, "step3")
     		
    ## The 'Plate Configuration' module: this is an array of image plots indicating the
    ## plate layout (controls, samples, flagged wells). The workhorse function to produce
    ## the necessary HTML code is 'writeHtml.plateConf'.
    plateConf.module <- chtsModule(cellHTSlist, url=file.path(htmldir, "plateConf.html"),
                               htmlFun=writeHtml.plateConf, title="Plate Configuration",
                               funArgs=list(nrPlate=nrPlate, posControls=posControls,
                               negControl=negControls))
    tab <- rbind(tab, writeHtml(plateConf.module))
    progress <- myUpdateProgress(progress, "step4")

    ## The 'Plate Summaries' module: boxplots of raw and normalized
    ## data as well as controls plots.  The workhorse function to
    ## produce the necessary HTML code is 'writeHtml.experimentQC'.
    experimentQC.module <- chtsModule(cellHTSlist, url=file.path(htmldir, "experimentQC.html"),
                                      htmlFun=writeHtml.experimentQC, title="Plate Summaries",
                                      funArgs=list(allControls=allControls, allZfac=allZfac))
    tab <- rbind(tab, writeHtml(experimentQC.module))
    		
    ## The 'Screen Summary' module: an image plot of the results for the whole screen, possibly
    ## with an underlying HTML imageMap to allow for drill-down to the quality report page of
    ## the respective plates. The workhorse function to produce the necessary HTML code is
    ## 'writeHtml.screenSummary'.
    screenSummary.module <- chtsModule(cellHTSlist, url=file.path(htmldir, "screenImage.html"),
                                       htmlFun=writeHtml.screenSummary, title="Screen Summary",
                                       funArgs=list(nrPlate=nrPlate,
                                                    overallState=overallState))
    tab <- rbind(tab, writeHtml(screenSummary.module))
    progress <- myUpdateProgress(progress, "step5")

    ## The 'Screen Results module': a downloadable ASCII table of the screening results and
    ## a sortable HTML table. The workhorse function to produce the necessary HTML code is
    ## 'writeHtml.screenResults'.
    screenResults.module <- chtsModule(cellHTSlist,
                                       url=file.path(htmldir, "screenResults.html"),
                                       htmlFun=writeHtml.screenResults, title="Screen Results",
                                       funArgs=list(file=file.path(outdir, "in",
                                                    "topTable.txt"),
                                       verbose=FALSE, overallState=overallState))
    tab <- rbind(tab, writeHtml(screenResults.module))
    progress <- myUpdateProgress(progress, "step6")
    
    ## The 'Screen Description module': currently an ASCII file of the
    ## screen description. FIXME: Later this is supposed to be
    ## formatted HTML output. The workhorse function to produce the
    ## necessary HTML code is 'writeHtml.screenDescription'.
    screenDescription.module <- chtsModule(cellHTSlist, url=file.path(htmldir,
                                                        "screenDescription.html"),
                                           htmlFun=writeHtml.screenDescription,
                                           title="Screen Description",
                                           funArgs=list(overallState=overallState,
                                           outFile=file.path(outdir, "in", "Description.txt")))
    tab <- rbind(tab, writeHtml(screenDescription.module, con=))


    ## The 'Screen Script module': The commands that generated this
    ## report.  The workhorse function to produce the necessary HTML
    ## code is 'writeHtml.screenScript'.
    screenScript.module <- chtsModule(cellHTSlist, url=file.path(htmldir, "screenScript.html"),
                                      htmlFun=writeHtml.screenScript, title="Analysis Script",
                                      funArgs=list(mainScriptFile=mainScriptFile,
                                      outputFile=file.path(outdir, "in", "mainScript.R")))
    tab <- rbind(tab, writeHtml(screenScript.module))

    ## The 'GSEA module': A basic gene set enrichment analysis.  The
    ## workhorse function to produce the necessary HTML code is
    ## 'writeHtml.gseaModule'.
    if(!is.null(gseaModule))
    {
        gsea.module <- chtsModule(cellHTSlist, url=file.path(htmldir, "gseaModule.html"),
                                  htmlFun=writeHtml.gseaModule, title="GSEA",
                                  funArgs=list(gmod=gseaModule, outdir=outdir))
        tab <- rbind(tab, writeHtml(gsea.module))
    }
    
    ## Create the main navgation page from the tab data.frame. This
    ## includes the basic screen information as well as the tabs to
    ## navigate to the different modules.
    indexFile <- file.path(outdir, "index.html")
    con <- file(indexFile, open="w")
    on.exit(close(con), add=TRUE)
    writeHtml.mainpage(title=name(xr), tabs=tab, con=con)
    progress <- myUpdateProgress(progress, "step7")
    
    ## finally, return indexFile
    cat(sprintf("\rReport was successfully generated in folder %s\n", indexFile))
    return(invisible(indexFile))
}

