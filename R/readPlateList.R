## Read in the list of plates in the 'Platelist' file and create a cellHTS object
readPlateList <- function(filename,
                          path=dirname(filename),
                          name,
                          importFun,
                          verbose=interactive())
{
    file <- basename(filename)
    dfiles <- dir(path)
  
    if(!(is.character(path)&&length(path)==1))
        stop("'path' must be character of length 1")
  
    pd <- read.table(file.path(path, file), sep="\t", header=TRUE, as.is=TRUE)
  
    checkColumns(pd, file, mandatory=c("Filename", "Plate", "Replicate"),
                 numeric=c("Plate", "Replicate", "Channel", "Batch"))
  
    ## consistency check for "importFun"
    if (!missing(importFun)) {
        if (!is(importFun, "function"))
            stop("'importFun' should be a function to use to read the raw data files")
    } else {
        ## default function (compatible with the file format of the plate reader)
        importFun <- function(f) {
            txt <- readLines(f, warn=FALSE)
            sp <- strsplit(txt, "\t")
            well <- sapply(sp, "[", 2)
            val <- sapply(sp, "[", 3)
            out <- list(data.frame(well=I(well), val=as.numeric(val)), txt=I(txt))
            return(out)
        }
    }
  
    ## check if the data files are in the given directory
    a <- unlist(sapply(pd$Filename, function(z) grep(z, dfiles, ignore.case=TRUE)))
    if (length(a)==0)
        stop(sprintf("None of the files were found in the given 'path': %s", path))
  
    f <- file.path(path, dfiles[a])

    ## check if 'importFun' gives the output in the desired form
    aux <- importFun(f[1])
    if (which(unlist(lapply(aux, is, "data.frame"))) != 1 |
        !all(c("val", "well") %in% names(aux[[1]])) | length(aux)!=2)
        stop("The output of 'importFun' must be a list with 2 components;\n",
             "the first component should be a 'data.frame' with slots 'well' and 'val'.")

    ## auto-determine the plate format
    well <- as.character(importFun(f[1])[[1]]$well)
    let <- substr(well, 1, 1)
    num <- substr(well, 2, 3)
    let <- match(let, LETTERS)
    num <- as.integer(num)
    if(any(is.na(let))||any(is.na(num)))
        stop(sprintf("Malformated column 'well' in input file %s", f[1]))

    dimPlate <- c(nrow=max(let), ncol=max(num))
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

    ## We delete all leading and trailing white space from the filenames
    pd$Filename <- gsub("^ *", "", gsub(" *$", "", pd$Filename))
    
    
    xraw <- array(NA_real_, dim=c(nrWell, nrPlate, nrRep, nrChannel))
    intensityFiles <- vector(mode="list", length=nrow(pd))
    names(intensityFiles) <- pd[, "Filename"]

    status <- character(nrow(pd))
    batch <- as.data.frame(matrix(ncol=nrRep, nrow=nrPlate))
    colnames(batch) <- sprintf("replicate%d", seq_len(nrRep))
    rownames(batch) <- sprintf("plate%d", seq_len(nrPlate))

    for(i in seq_len(nrow(pd))) {
        if(verbose)
            cat("\rReading ", i, ": ", pd$Filename[i], sep="")

        ff <- grep(pd[i, "Filename"], dfiles, ignore.case=TRUE)

        if (length(ff)!=1) {
            f <- file.path(path, pd[i, "Filename"])
            status[i] <- sprintf("File not found: %s", f)
        } else {
            f <- file.path(path, dfiles[ff])
            names(intensityFiles)[i] <- dfiles[ff]
            status[i] <- tryCatch({
                out <- importFun(f)
                pos <- convertWellCoordinates(out[[1]]$well, dimPlate)$num
                intensityFiles[[i]] <- out[[2]]
                xraw[pos, pd$Plate[i], pd$Replicate[i], channel[i]] <- out[[1]]$val
                "OK"
            },
                                  warning=function(e) paste(class(e)[1], e$message, sep=": "),
                                  error=function(e) paste(class(e)[1], e$message, sep=": ")
                                  ) ## tryCatch
        } ## else
        bt <- pd$Batch[i]
        batch[pd$Plate[i], pd$Replicate[i]] <- if(!is.null(bt)) bt else 1

    } ## for

    if(verbose)
        cat("\rRead", nrow(pd), "plates.                                                \n\n")
        
    ## ----  Store the data as a "cellHTS" object ----
    ## arrange the assayData slot:
    dat <- lapply(seq_len(nrChannel), function(ch) 
                  matrix(xraw[,,,ch], ncol=nrRep, nrow=nrWell*nrPlate))
    channelNames = paste("Channel", seq_len(nrChannel))
    names(dat) <- channelNames
    
    adata <- do.call(assayDataNew, c(storage.mode="lockedEnvironment", dat))
    
    ## arrange the phenoData slot:
    pdata <- new("AnnotatedDataFrame",
                 data <- data.frame(replicate=seq_len(nrRep),
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
               plateList=cbind(pd[,1L,drop=FALSE], status=I(status), pd[,-1L,drop=FALSE]),
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
        res@plateList$status <- gsub("File not found.*", "ERROR",
                                     gsub("simpleError.*", "ERROR",
                                          gsub("simpleWarning.*", "WARNING",
                                               res@plateList$status)))
    }
    return(res)
}


