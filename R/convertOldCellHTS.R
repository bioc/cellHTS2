## Function that converts an S3 class cellHTS object to a S4 cellHTS object 
## Ligia P. Bras (September 
convertOldCellHTS <- function(oldObject)
{ 
    ## old S3 class cellHTS object
    if(!(mode(oldObject)=="list" & class(oldObject)=="cellHTS"))
        stop("'oldObject' should be an object of S3 class 'cellHTS' (obtained using ",
             "'cellHTS' package)")

    ## depending on the state of 'oldObject', we might return 1 to 3 cellHTS objects
    ## 1) Raw data:
    ## arrange the assayData slot:
    xraw <- oldObject$xraw
    nrChannel <- dim(xraw)[4]
    nrRep <- dim(xraw)[3]
    nrPlate <- dim(xraw)[2]
    nrWell <- dim(xraw)[1]
    adata <- assayDataNew(storage.mode="environment")
    chNames <- paste("ch", 1:nrChannel, sep="")
    for(ch in 1:nrChannel) 
        assign(chNames[ch], matrix(xraw[,,,ch, drop=TRUE], ncol=nrRep, nrow=nrWell*nrPlate),
               envir=adata)
    storageMode(adata) <- "lockedEnvironment"

    ## arrange the phenoData slot:
    pdata <- new("cellHTS")@phenoData
    pData(pdata) <- data.frame(replicate=1:nrRep, assay=I(rep(oldObject$name, nrRep)))
    varMetadata(pdata)[["channel"]] = factor(rep("_ALL_",2), levels=c(chNames, "_ALL_"))
    wa <- if(oldObject$state[["configured"]]) oldObject$wellAnno else
    factor(rep("unknown", nrWell*nrPlate))

    ## arrange the featureData slot:
    wells <- convertWellCoordinates(as.integer(1:nrWell), pdim=oldObject$pdim)$letnum
    fdata <- new("cellHTS")@featureData
    pData(fdata) <- data.frame(plate=rep(1:nrPlate, each=nrWell), well=I(rep(wells,nrPlate)), 
                               controlStatus=wa)

    ## create S4 class cellHTS object
    objRaw <- new("cellHTS", 
                  assayData=adata,
                  phenoData=pdata,
                  featureData=fdata,
                  plateList=oldObject$plateList,
                  intensityFiles=oldObject$intensityFiles
                  )


    ## add additional slots and update the state of the new object if it is
    ## configured and annotated
    if(oldObject$state[["configured"]])
    {
        objRaw@state[["configured"]] <- TRUE

        ## add plate configuration slot, screen log slot, experimentData slot
        objRaw@plateConf <- oldObject$plateConf
        objRaw@screenLog <- oldObject$screenLog
        objRaw@screenDesc <- oldObject$screenDesc

        ## description data
        ## Process the description info to store it in 'experimentData' slot:
        descript <- oldObject$screenDesc

        ## Store the contents of the description file in the 'experimentData' slot
        ## which is accessed via description(object):
        miameList <- list(sepLab=grep("Lab description", descript),
                          name = grep("^Experimenter name:", descript),
                          lab  =   grep("^Laboratory:", descript),
                          contact = grep("^Contact information:", descript),
                          title=grep( "^Screentitle:", descript),
                          pubMedIds=grep( "^PMID",descript),
                          url=grep("^Web:",descript),
                          abstract=grep("^Abstract:",descript)
                          )

        miameInfo <- lapply(miameList, function(i)
                            unlist(strsplit(descript[i], split=": "))[2L]) 
        miameInfo <- lapply(miameInfo, function(i){
            if(is.null(i)) "" else { if(is.na(i)) "" else i }})
        miameInfo <- with(miameInfo, new("MIAME", 
                                         name=name,
                                         lab = lab,
                                         contact=contact,
                                         title=title,
                                         pubMedIds=pubMedIds,
                                         url=url,
                                         abstract=abstract))

        ## store the rest of the description information into slot "other":
        otherInfo <- descript[-unlist(miameList)]
        notes(miameInfo) <- lapply(otherInfo, function(i) paste("\t", i, "\n", collapse="")) 

        ## add the 'miame'-like description:
        description(objRaw) <- miameInfo
    }

    ## Handle annotation
    if(oldObject$state[["annotated"]])
    {
        objRaw@state[["annotated"]] <- TRUE
        gInfo <- oldObject$geneAnno
        gInfo <- gInfo[, !c(names(gInfo) %in% c("Plate", "Well"))]
        ## flag 'NA' values in the "GeneID" column:
        gInfo[apply(gInfo, 2, function(i) i %in% "NA")] <- NA 

        ## add the columns into featureData:
        fData(objRaw)[names(gInfo)] <- gInfo
        fvarMetadata(objRaw)[names(gInfo),] <- names(gInfo)
    }

    
    ## return the cellHTS object(s) as a named list:
    cellHTSlist <- list("raw" = objRaw)

    ## 2) Does 'oldObject' contain normalized data?
    if(oldObject$state[["normalized"]])
    {
        objNorm <- objRaw
        xnorm <- oldObject$xnorm
        hasLessChan <- dim(xnorm)[4] < dim(xraw)[4]
        xnorm <- array(xnorm, dim=c(prod(nrWell*nrPlate), nrRep, dim(xnorm)[4]))
        
        Data(objNorm) <- xnorm
        objNorm@state[["normalized"]] <- TRUE

        cellHTSlist$"normalized" = objNorm
    }


    ## 3) Does 'oldObject' contain scored data?
    if(oldObject$state[["scored"]])
    {
        objSc <- objNorm[,1] # construct a cellHTS object just with the first sample
        scores <- oldObject$score
        assayData(objSc) <- assayDataNew("score"=matrix(scores,
                                         dimnames=list(featureNames(objNorm), 1)))

        ## just to ensure that the object will pass the validity checks:
        objSc@state[["scored"]] <- TRUE
        cellHTSlist$"scored" <- objSc
    }
    
    sapply(cellHTSlist, validObject)
    validObject(objRaw)
    return(cellHTSlist)
}



## Update an outdated S4 cellHTS object (i.e., the new processingInfo and plateData slots are missing
updateCellHTS <- function(object)
{
    if(isUpToDate(object)[["valid"]]){
        warning("This cellHTS object is already up to date.", call.=FALSE)
        return(object)
    }
    availSlots <- getObjectSlots(object)
    availSlotNames <- names(availSlots)
    definedSlotNames <- slotNames(object)
    commonSlots <- intersect(definedSlotNames, availSlotNames)
    missingSlots <- setdiff(definedSlotNames, availSlotNames)
    newObject <- new(class(object))
    for(s in commonSlots)
        slot(newObject, s) <- availSlots[[s]]
    oldbatch <- object@batch
    if(length(oldbatch))
        warning("Unable to update the batch information.", call.=FALSE)
    return(newObject)
}

