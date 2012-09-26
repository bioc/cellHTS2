## Read the actual plate list. If 'file' is a character vector we will use
## read.table(file...), otherwise it has to be a function which returns the
## appropriate data.frame or data.frame-like list:
##   - mandatory columns 'Filename', 'Plate', and 'Replicate', the latter
##     two of class 'numeric'
##   - optional columns 'Channel' and 'Batch' which have to be of class
##     'numeric'
##   - additional optional columns of any type
getPlist <- function(filename, path, ...)
{
    if(is.character(filename))
    {
        if(is.na(path)) path <- dirname(filename)
        filename <- basename(filename)
        fp <- file.path(path, filename)
        if(!file.exists(fp))
            stop(sprintf("The file '%s' could not be found in the given path '%s'.",
                         filename, path))
        pd <- read.table(file.path(path, filename), sep="\t", header=TRUE, as.is=TRUE)
        pd$Filename <- file.path(path, gsub("^ *", "", gsub(" *$", "", pd$Filename)))
        checkColumns(pd, filename, mandatory=c("Filename", "Plate", "Replicate"),
                     numeric=c("Plate", "Replicate", "Channel", "Batch"))
        ## check if the data files are in the given directory
        if(!any(file.exists(pd$Filename)))
            stop(sprintf(paste("None of the files specified in '%s'",
                               "were found in the given path '%s'"), filename, path))
    }
    else if(is.function(filename))
    {
        pd <- filename(...)
        checkColumns(pd, "Created by function", mandatory=c("Filename", "Plate", "Replicate"),
                     numeric=c("Plate", "Replicate", "Channel", "Batch"))
    }
    else
    {
        stop("'filename' must be either a character scalar or a function.\n",
             "Please see '?readPlateList' for details.") 
    }
    return(pd)
}



## Read in the list of plates in the 'Platelist' file and create a cellHTS object
readPlateList <- function(filename,
                          path=NA,
                          name="anonymous",
                          importFun,
                          dec=".",
                          verbose=interactive(),
                          ...)
{
    ## We read in the plate list file first
    if(!is.na(path) && !(is.character(path) && length(path)==1))
        stop("'path' must be character of length 1")
    if(is.na(path) && !is.function(filename))
        path <- dirname(filename)
    pd <- getPlist(filename, path, ...)

    ## consistency check for "importFun"
    if (!missing(importFun))
    {
        if (!is(importFun, "function"))
            stop("'importFun' should be a function uses to read the raw data files\n.",
                 "Please see '?readPlateList' for details.")
    }
    else
    {
        ## default function (compatible with the file format of the plate reader)
        importFun <- function(f, dec)
        {
            if(!file.exists(f))
                stop("File not found: ", f)
            txt <- readLines(f, warn=FALSE)
            sp <- strsplit(txt, "\t")
            well <- sapply(sp, "[", 2)
            val <- sapply(sp, "[", 3)
            if (dec!=".") val <- sub(dec, ".", val)
            out <- list(data.frame(well=I(well), val=as.numeric(val)), txt=I(txt))
            return(out)
        }
    }
    
    ## check if 'importFun' gives the output in the desired form. Since we want
    ## to allow for missing files and reading errors later, we have to iterate through
    ## the whole list until we first find something that resembles what we need. If
    ## the iteration stops and we still haven't found anything useful we cast an
    ## error.
    failed <- TRUE
    for(f in pd$Filename)
    {
        aux <- try(importFun(f, dec), silent=TRUE)
        if(is.list(aux) && length(aux)==2 && is.data.frame(aux[[1]]) &&
           all(c("val", "well") %in% names(aux[[1]])))
        {
            failed <- FALSE
            break
        }
    }
    if(failed)
        stop("The output of 'importFun' must be a list with 2 components;\n",
             "the first component should be a 'data.frame' with slots 'well' and 'val'.",
             if(is(aux, "try-error")) paste("\nThe following additional errors where encountered:\n",
                                            as.character(aux)))

    ## auto-determine the plate format
    well <- as.character(aux[[1]]$well)
    codes <- parseLetNum(well)
    if(any(is.na(codes$lindex)) || any(is.na(codes$numbers)))
        stop(sprintf("Malformated column 'well' in input file %s", f[1]))
    dimPlate <- c(nrow=max(codes$lindex), ncol=max(codes$numbers))
    nrWell <- prod(dimPlate)
    if(verbose)
        cat(sprintf("%s: found data in %d x %d (%d well) format.\n", name,
                    dimPlate[1], dimPlate[2], nrWell))
    ## Should we check whether these are true?
    ##     "96"  = c(nrow=8, ncol=12),
    ##     "384" = c(nrow=16, ncol=24),
    nrRep <- max(pd$Replicate)
    nrPlate <- max(pd$Plate)
    combo <- paste(pd$Plate, pd$Replicate)

    ## Channel: if not given, this implies that there is just one
    if("Channel" %in% colnames(pd)) {
        nrChannel <- max(pd$Channel)
        channel <- pd$Channel
        combo <- paste(combo, pd$Channel)
    } else {
        nrChannel <- 1L
        channel <- rep(1L, nrow(pd))
        pd$Channel <- channel	
    }

    ## Make sure all entries are unique
    whDup <- which(duplicated(combo))
    if(length(whDup)>0L) {
        idx <- whDup[1:min(5L, length(whDup))]
        msg <- paste("The following rows are duplicated in the plateList table:\n",
                     "\tPlate Replicate Channel\n", "\t",
                     paste(idx, combo[idx], sep="\t", collapse="\n\t"),
                     if(length(whDup)>5) sprintf("\n\t...and %d more.\n",
                                                 length(whDup)-5), "\n", sep="")
        stop(msg)
    }

    ## Prepare the raw, batch and status vectors
    xraw <- array(NA_real_, dim=c(nrWell, nrPlate, nrRep, nrChannel))
    intensityFiles <- vector(mode="list", length=nrow(pd))
    finalFilename <-
        if(is.character(pd[, "Filename"])) basename(pd[, "Filename"]) else
           paste("Plate", combo)
    names(intensityFiles) <- finalFilename
    status <- character(nrow(pd))
    batch <- as.data.frame(matrix(ncol=nrRep, nrow=nrPlate))
    colnames(batch) <- sprintf("replicate%d", seq_len(nrRep))
    rownames(batch) <- sprintf("plate%d", seq_len(nrPlate))

    ## Iterate over each file and read in the data
    for(i in seq_len(length(pd$Filename)))
    {
        if(verbose)
            cat("\rReading ", i, ": ", finalFilename[i], sep="")
        status[i] <- tryCatch(
                          {
                              out <- importFun(pd$Filename[[i]], dec)
                              pos <- convertWellCoordinates(out[[1]]$well, dimPlate)$num
                              intensityFiles[[i]] <- out[[2]]
                              xraw[pos, pd$Plate[i], pd$Replicate[i], channel[i]] <- out[[1]]$val
                              "OK"
                          },
                              warning=function(e) paste(class(e)[1], e$message, sep=": "),
                              error=function(e) paste(class(e)[1], e$message, sep=": "))
        bt <- pd$Batch[i]
        batch[pd$Plate[i], pd$Replicate[i]] <- if(!is.null(bt)) bt else 1
    } ## for
    if(verbose)
        cat("\rRead", nrow(pd), "plates.                                                \n\n")

    ## ----  Store the data as a "cellHTS" object ----
    ## arrange the assayData slot:
    dat <- lapply(seq_len(nrChannel), function(ch) 
                  matrix(xraw[,,,ch], ncol=nrRep, nrow=nrWell*nrPlate))
    channelNames <- paste("Channel", seq_len(nrChannel))
    names(dat) <- channelNames
    adata <- do.call(assayDataNew, c(storage.mode="lockedEnvironment", dat))
    
    ## arrange the phenoData slot:
    pdata <- new("AnnotatedDataFrame",
                 data=data.frame(replicate=seq_len(nrRep),
                                 assay=rep(name, nrRep),
                                 stringsAsFactors=FALSE),
                 varMetadata=data.frame(labelDescription=c("Replicate number",
                                                           "Biological assay"),
                                        channel=factor(rep("_ALL_", 2L),
                                                       levels=c(names(dat), "_ALL_")),
                                        row.names=c("replicate", "assay"),
                                        stringsAsFactors=FALSE))

    ## arrange the featureData slot:
    well <- convertWellCoordinates(seq_len(nrWell), pdim=dimPlate)$letnum
    fdata <- new("AnnotatedDataFrame", 
                 data <- data.frame(plate=rep(seq_len(nrPlate), each=nrWell),
                                    well=rep(well, nrPlate), 
                                    controlStatus=factor(rep("unknown", nrWell*nrPlate)),
                                    stringsAsFactors=FALSE), 
                 varMetadata=data.frame(labelDescription=c("Plate number", "Well ID",
                                                           "Well annotation"),
                                        row.names=c("plate", "well", "controlStatus"),
                                        stringsAsFactors=FALSE))
    res <- new("cellHTS", 
               assayData=adata,
               phenoData=pdata,
               featureData=fdata,
               plateList=cbind(Filename=I(finalFilename), Status=I(status), pd[,-1L,drop=FALSE]),
               intensityFiles=intensityFiles,
               plateData=list(Batch=batch))

    ## output the possible errors that were encountered along the way:
    whHadProbs <- which(status!="OK")
    if(length(whHadProbs)>0 & verbose) {
        idx <- whHadProbs[1:min(5, length(whHadProbs))]
        msg <- paste("Please check the following problems encountered while reading the data:\n",
                     "\tFilename \t Error\n", "\t",
                     paste(plateList(res)$Filename[idx], status[idx], sep="\t", collapse="\n\t"),
                     if(length(whHadProbs)>5) sprintf("\n\t...and %d more.\n",
                                                      length(whHadProbs)-5), "\n", sep="")
        warning(msg, call.=FALSE)
    }
    
    ## We only need the error code in the plateList, the full error
    ## message can be supplied as an additional column, which we later
    ## use to create the tooltips.
    res@plateList$errorMessage <- NA
    if(any(whHadProbs))
    {
        res@plateList[whHadProbs, "errorMessage"] <- gsub("'", "", status[whHadProbs])
        res@plateList$Status <- gsub("File not found.*", "ERROR",
                                     gsub("simpleError.*", "ERROR",
                                          gsub("simpleWarning.*", "WARNING",
                                               res@plateList$Status)))
    }
    return(res)
}

## build a cellHTS2 object from a data.frame xd containing the plate, replicate, well and measurement columns
buildCellHTS2 = function(xd, measurementNames) {
  ## check arguments
  if (missing(xd)) stop("'xd' is missing")
  if (!is.data.frame(xd)) stop("'xd' must be a data.frame")
  iplate = grep('plate', colnames(xd), ignore.case=TRUE)
  ireplicate = grep('replicate', colnames(xd), ignore.case=TRUE)
  iwell = grep('well', colnames(xd), ignore.case=TRUE)
  if (length(c(iplate, ireplicate, iwell))!=3) stop("'xd' must contain the columns 'plate', 'replicate' and 'well'")
  if (ncol(xd)<4) stop("'xd' must contain one or several columns with measurements")

  # enforce standardization of well id to have two digits in the numbers part of the alphanumeric notation
  xd[,iwell] = standardizeWellID(xd[,iwell])

  buildPlist = function(xd) {
    nbPlates = max(xd[,iplate])
    nbReplicates = max(xd[,ireplicate])
    nbChannels = ncol(xd) - 3
    pl = expand.grid(Plate=1:nbPlates, Replicate=1:nbReplicates, Channel=1:nbChannels, stringsAsFactors=FALSE)
    cbind(Filename=apply(pl, 1, paste, collapse='-'), pl, stringsAsFactors=FALSE)
  }
  
  importFun = function(f, dec) {
    prc = as.numeric(strsplit(f, '-')[[1]])
    z = paste(prc[1:2], collapse='-') == paste(xd[,iplate], xd[,ireplicate], sep='-')
    list(data.frame(well=xd[z, iwell], val=xd[z, -c(iplate, ireplicate, iwell), drop=FALSE][,prc[3]], stringsAsFactors=FALSE), txt='computed with buildCellHTS2()')
  }
  
  if (missing(measurementNames)) measurementNames = colnames(xd)[-c(iplate, ireplicate, iwell)]
  measurementNames = measurementNames[order(measurementNames)]
  xd = xd[,c(colnames(xd)[c(ireplicate, iplate, iwell)], measurementNames)]
  ireplicate = 1
  iplate = 2
  iwell = 3

  x = readPlateList(buildPlist, importFun=importFun, xd=xd)
  if (!is.null(measurementNames)) channelNames(x) = measurementNames
  x
}

## parse the LetterNumber representation of well coordinates into something
## more amenable for computational processing. The output is a list of three
## items:
##   - letters: the letters part
##   - numbers: the numbers part
##   - lindex: a numerical index for the letters (i.e., the row indices)
parseLetNum <- function(well)
{
    well <- gsub(" ", "", well)
    s <- strsplit(well, "")
    nl <- lapply(s, function(x) grep("[A-Za-z]", x))
    if(any(lapply(nl, function(x) {sum(x>2)})>0))
        stop("More than 2 letters in a well identifier.")
    let = vector('list', length=length(well))
    num = vector('list', length=length(well))
    letInd = array(dim=length(well))
    for (i in 1:length(well)) {
    let[[i]] = substr(well[[i]], 1, max(nl[[i]]))
    num[[i]] = as.integer(substr(well[[i]], max(nl[[i]])+1, 100))
    letInd[i] <- if (max(nl[[i]]) == 1) {match(let[[i]],LETTERS)} else {
       (match(substr(let[[i]],1,1), LETTERS)) * 26 + match(substr(let[[i]],2,2), LETTERS)
       }
    }
    if(any(is.na(num)) || any(is.na(letInd))) 
       stop("Malformated well identifier.")
    return(list(letters=unlist(let), numbers=unlist(num), lindex=letInd))
}


## convert coordinates from one representation to another
convertWellCoordinates <- function(x, pdim, type="384")
{
    if(!missing(pdim))
    {
        if(!missing(type))
            stop("Please specify either 'pdim' or 'type' but not both.")
        storage.mode(pdim) <- "integer"
        if(!(all(names(pdim) %in% c("nrow", "ncol")) && (length(names(pdim))==2L)))
            stop("'pdim' should be a vector of length 2 with names 'nrow' and 'ncol'.")
        if(any(is.na(pdim)))
            stop("'pdim' contains invalid values: %s", paste(as.character(pdim),
                                                             collapse="\n"))
    }
    else
    {
        if(!(is.character(type)&&(length(type)==1L)))
            stop("'type' must be a character of length 1.")
        pdim <- switch(type,
                      "24"  = c(nrow= 4L, ncol= 6L),
                      "96"  = c(nrow= 8L, ncol=12L),
                      "384" = c(nrow=16L, ncol=24L),
               	      "1536" = c(nrow = 32L, ncol = 48L),
                      stop("Invalid 'type': %s", type))
    }
    

    if(is.character(x))
    {
        ## If coordinates are passed in as a matrix (when would that happen?!?)
        if(is.matrix(x))
            x <- apply(x, 1L, paste, collapse="") 

        ## Parse to something machine-readble
        tmp <- parseLetNum(x)
        let <- tmp$letters
        num <- tmp$numbers
        let.num <- cbind(let, sprintf("%02d",num))
        letnum <- apply(let.num, 1, paste, collapse="")
        irow <- tmp$lindex
        icol <- as.integer(num)
        if( any(is.na(irow)) || any(irow>pdim["nrow"]) || any(irow<1L) ||
           any(is.na(icol)) || any(icol>pdim["ncol"]) || any(icol<1L) )
            stop("Invalid position IDs in 'x'.")
        num <- (irow-1L) * pdim["ncol"] + icol
      }
    else if(is.numeric(x))
    {
        ## x is of the form 1, 14, 18, ...
        num <- as.integer(x)
        if(any(num<1L)||any(num>prod(pdim))) 
            stop(sprintf("Invalid values in 'x', must be between 1 and %d.", prod(pdim)))

        irow <- 1L + (num-1L) %/% pdim["ncol"]
        icol <- 1L + (num-1L) %%  pdim["ncol"]
        id=getAlphaNumeric(irow, icol)
        letters=id$id.alpha
        letnum=id$id
        let.num <- cbind(letters, sprintf("%02d", icol))
    }
    else if(!length(x))
    {
        letnum <- let.num <- num <- NULL
    }
    else
    {
        stop("'x' must be either a character vector with alphanumeric well IDs ",
             "(e.g. 'B03' or c('B', '03'))\n or a vector of integers with position ",
             "IDs within a plate (e.g. 27).")
    }
    return(list(letnum = letnum, let.num = let.num, num = as.integer(num)))
}




