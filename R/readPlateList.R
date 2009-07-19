## Read in the list of plates in the 'Platelist' file and create a cellHTS object
readPlateList <- function(filename,
                          path=dirname(filename),
                          name="anonymous",
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
    a <- unlist(sapply(pd$Filename, function(z) grep(paste("^", z, "$", sep=""),
                                                     dfiles, ignore.case=TRUE)))
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

        ff <- grep(paste("^", pd[i, "Filename"], "$", sep=""),
                   dfiles, ignore.case=TRUE)

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




## parse the LetterNumber representation of well coordinates into something
## more amenable for computational processing. The output is a list of three
## items:
##   - letters: the letters part
##   - numbers: the numbers part
##   - lindex: a numerical index for the letters (i.e., the row indices)
parseLetNum <- function(well)
{
    well <- gsub(" ", "", well)
    s <- strsplit(well[1], "")[[1]]
    nl <- length(unlist(sapply(s, function(x) grep("[A-Za-z]", x))))
    if(nl>2)
        stop("Too many letters in the well identifier.")
    let <- substr(well, 1, nl)
    num <- as.integer(substr(well, nl+1, 100))
    letInd <- if(nl==1) match(let, LETTERS) else
    (match(substr(let,1,1), LETTERS)-1) * 26 + match(substr(let,2,2), LETTERS)
    if(any(is.na(num)) || any(is.na(letInd)))
        stop("Malformated well identifier.")
    return(list(letters=let, numbers=num, lindex=letInd))
}


## convert from one representation to another
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
        let.num <- cbind(let, num)
        letnum <- x
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
        letters <- if(max(irow) <= 26) LETTERS[irow] else cbind(LETTERS[((irow-1) %/% 26)+1],
                         LETTERS[((irow-1) %% 26)+1])
        let.num <- cbind(letters, sprintf("%02d", icol))
        letnum <- apply(let.num, 1L, paste, collapse="")
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
    return(list(letnum = letnum, let.num = let.num, num = num))
}




