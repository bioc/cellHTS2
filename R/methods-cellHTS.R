#------------------------------------------------------------
# methods related to the class 'cellHTS'
#------------------------------------------------------------
setMethod("initialize",
          signature(.Object="cellHTS"),
          function(.Object, assayData, phenoData, featureData,
                   ...)
      {
          mySlots <- slotNames(.Object)
          dotArgs <- list(...)
          isSlot <- names(dotArgs) %in% mySlots
          if (missing(assayData))
          {
              assayData <- do.call(assayDataNew, dotArgs[!isSlot],
                                   envir=parent.frame())
          }
          if (missing(phenoData))
          {
              phenoData <- annotatedDataFrameFrom(assayData, byrow=FALSE)
              ## add the mandatory columns: "plate" and "assay"
              pData(phenoData) <- data.frame(replicate=integer(), assay=character())
              varMetadata(phenoData) <-
                  data.frame(labelDescription=I(c("Replicate number", "Biological assay")))
          } 

          if (is.null(varMetadata(phenoData)[["channel"]]))
          {
              varMetadata(phenoData)[["channel"]] <- 
                  factor(rep("_ALL_", nrow(varMetadata(phenoData))),
                         levels=c(assayDataElementNames(assayData), "_ALL_"))
          }

          if (missing(featureData))
          {
              featureData <- annotatedDataFrameFrom(assayData, byrow=TRUE)
              pData(featureData) <- data.frame(plate=integer(), well=I(character()),
                                               controlStatus=factor(integer()))
              varMetadata(featureData) <-
                  data.frame(labelDescription=I(c("Plate number", "Well ID", "Well annotation")))
          }

          ## ensure sample names OK -- all assayData with names;
          ## phenoData with correct names from assayData
          appl <- if (storageMode(assayData)=="list") lapply else eapply
          assaySampleNames <- appl(assayData, function(elt) {
              cnames <- colnames(elt)
              if (is.null(cnames)) sampleNames(phenoData)
              else cnames
          })
          sampleNames(assayData) <- assaySampleNames
          sampleNames(phenoData) <- sampleNames(assayData)
          do.call(callNextMethod,
                  c(.Object,
                    assayData = assayData, phenoData = phenoData, featureData=featureData,
                    dotArgs[isSlot]))
      })



setMethod("show",
          signature=signature(object="cellHTS"),
          function(object)
      {
          isUpToDate(object, error=TRUE)
          cat(class( object ), " (storageMode: ", storageMode(object), ")\n", sep="")
          adim <- dim(object)
          if (length(adim)>1)
              cat("assayData:",
                  if (length(adim)>1)
                  paste(adim[[1]], "features,",
                        adim[[2]], "samples") else NULL,
                  "\n")
          cat("  element names:",
              paste(assayDataElementNames(object), collapse=", "), "\n")
          Biobase:::.showAnnotatedDataFrame(phenoData(object),
                                            labels=list(object="phenoData"))
          Biobase:::.showAnnotatedDataFrame(featureData(object),
                                            labels=list(
                                            object="featureData",
                                            sampleNames="featureNames",
                                            varLabels="fvarLabels",
                                            varMetadata="fvarMetadata"))
          cat("experimentData: use 'experimentData(object)'\n")
          cat("state: ", paste(paste(names(state(object)),state(object), sep=" = "),
                               collapse="\n\t"), "\n") 
          cat("Number of plates:", if(length(plate(object))) max(plate(object)), "", "\n")
          cat("Plate dimension:", if(length(pdim(object))) paste(paste(names(pdim(object)),
                                                                       pdim(object), sep=" = "),
                                                                 collapse=", "), "\n")
          cat("Number of batches:", nbatch(object), "\n")
          if(!is.null(wellAnno(object)))
              cat("Well annotation:", paste(levels(wellAnno(object))), "\n")
          pmids <- pubMedIds(object)
          if (length(pmids) > 0 && all(pmids != ""))
              cat("  pubMedIds:", paste(pmids, sep=", "), "\n")
          if(length(annotation(object)))
              cat("Annotation:", annotation(object), "\n")
          return(NULL)
      })


##----------------------------------------
## Data accessors and replacement functions
##----------------------------------------
# plate
setMethod("plate", signature(object="cellHTS"),
          function(object) fData(object)$"plate" )

# well
setMethod("well", signature(object="cellHTS"),
          function(object)
      {
          w <- fData(object)$"well" 
          return(if(!length(w)) NULL else w)
      }
          )


# plate dimension
setMethod("pdim", signature(object="cellHTS"),
          function(object)
      {
          pdim <- NULL
          if(!is.null(well(object)))
          {
              tmp <- parseLetNum(well(object))
              pdim <- c("nrow"=max(tmp$lindex),
                        "ncol"=max(tmp$numbers))
          }
          return(pdim)
      })

# well position
setMethod("position", signature(object="cellHTS"),
          function(object)
      { 
          if(!is.null(well(object))) 
              return(convertWellCoordinates(well(object), pdim=pdim(object))$num)
          else
              return(NULL)
      })


# well annotation
setMethod("wellAnno", signature(object="cellHTS"),
          function(object)
      { 
          if(state(object)[["configured"]]) 
              fData(object)$controlStatus
          else
              NULL
      })


setMethod("geneAnno", signature(object="cellHTS"),
          function(object) 
          if(state(object)[["annotated"]]) fData(object)$GeneID else NULL
          )

## the assayData is overriden each time new data are generated from the analysis.
setMethod("Data", signature(object="cellHTS"),
          function(object)
      {
          ch <- channelNames(object)
          dat=NULL
          if(length(ch)) {
              dat <- array(NA, dim=c(dim(object), "Channels"=length(ch)))
              ## NB: we assume that sampleNames is identical across channels
              dimnames(dat)=list(Features=featureNames(object),
                                 ##Sample=sampleNames(object)[[1]],
                                 Sample=sampleNames(object),
                                 Channels=ch)
              dat[] <- sapply(ch, function(i) assayDataElement(object, i))
          }
          return(dat) 
      })


setReplaceMethod("Data",
                 signature=signature(
                 object="cellHTS",
                 value="array"),
                 function(object, value)
             {
                 if(is.null(Data(object)))
                     stop("'object' has no data! No replacement with 'value' can be made!")
                 ch <- channelNames(object)
                 ## If 'value' is a matrix, set it as an array.
                 if(inherits(value, "matrix"))
                     value <- array(value, dim=c(dim(value),1))
                 d <- c(dim(object), "Channels"=length(ch))
                 if(any(dim(value)!=d))
                     stop(sprintf(paste("'value' should be an array with dimensions 'Features",
                                        "x Samples x Channels' (%s)."), paste(d, collapse=" x "))) 
                 if(is.null(dimnames(value)))
                     dimnames(value) <- list(featureNames(object),
                                             sampleNames(object),
                                             channelNames(object))
                 for(i in c(1:length(ch)))
                     assayDataElement(object, ch[i]) <- matrix(value[,,i], nrow=d[1], ncol=d[2], 
                                                               dimnames=dimnames(value)[1:2])

                 #if(any(dimnames(value)[[3]]!=channelNames(object)))
                 #    channelNames(object) <- dimnames(value)[[3]]
                 ## in case 'value' had different sample names in it.
                 sampleNames(phenoData(object)) <- sampleNames(object)
                 featureNames(object) <- dimnames(value)[[1]]

                 ## sampleNames(assayData(object)) <- sampleNames(phenoData(object))
                 validObject(object)
                 return(object)
             })

## accessors to see the overall state of the cellHTS object:
setMethod("state", signature(object="cellHTS"),
          function(object) object@state
          )

setMethod("plateList", signature(object="cellHTS"),
          function(object)
      {
          tmp <- object@plateList
          sel <- match("errorMessage", names(tmp))
          if(!is.na(sel))
              tmp <- tmp[,-sel]
          return(tmp)
      })

setMethod("intensityFiles", signature(object="cellHTS"),
          function(object) object@intensityFiles
          )

setMethod("plateConf", signature(object="cellHTS"),
          function(object) object@plateConf
          )

setMethod("screenLog", signature(object="cellHTS"),
          function(object) object@screenLog
          )

setMethod("screenDesc", signature(object="cellHTS"),
          function(object) object@screenDesc
          )



setMethod("plateEffects", signature(object="cellHTS"),
          function(object){
              list(rowcol=slot(object, "rowcol.effects"),
                  overall = slot(object, "overall.effects")) 
 })
 
setMethod("name", signature(object="cellHTS"),
          function(object) {
            unique(as.character(pData(object)$"assay"))
          })

setReplaceMethod("name",
                 signature=signature(
                   object="cellHTS",
                   value="character"),
                 function(object, value) {
                     pData(object)$"assay" <- value
                     validObject(object)
                     return(object)
                 })



setMethod("batch", signature(object="cellHTS"),
          function(object) {
             bb <- slot(object, "plateData")$Batch
             if(!length(bb)) bb <- NULL
             return(bb) 
          })


setReplaceMethod("batch",
                 signature=signature(
                   object="cellHTS",
                   value="data.frame"),
                 function(object, value) {
                     oldbatch <- batch(object)
                     if(!all(dim(oldbatch) == dim(value)))
                        stop("Replacement value for the batch information ",
                             "must be a data frame with ",
                             nrow(oldbatch), " rows and ", ncol(oldbatch),
                             " columns.", call.=FALSE)
                        dimnames(value) <- dimnames(oldbatch)
                     if(!all(is.numeric(value)))
                         stop("Replacement value for the batch information ",
                             "must all be integers.", call.=FALSE)
                     slot(object, "plateData")$Batch <- value
                     validObject(object)
                     return(object)
                 })


setMethod("nbatch", signature(object="cellHTS"),
          function(object) {
             bb <- batch(object)
             bb <- if(!is.null(bb)) length(unique(unlist(bb))) else NULL
             return(bb)
          })




##----------------------------------------
## annotate
##----------------------------------------
setMethod("annotate",
          signature=signature("cellHTS"),
          definition=function(object, geneIDFile, path=dirname(geneIDFile))
      {
          file <- basename(geneIDFile)
 
          if(!(is.character(path)&&(length(path)==1L)))
              stop("'path' must be character of length 1")

          geneIDs <- read.table(file.path(path, file), sep="\t", header=TRUE,
                                stringsAsFactors=FALSE, na.string="", quote="",
                                fill=FALSE)
          
          checkColumns(geneIDs, file, mandatory=c("Plate", "Well", "GeneID"),
                       numeric=c("Plate"))

          ## sort the data by Plate and then by well
          ## ordering by well might be problematic when we have "A2" instead of "A02"...
          ## so first check if all well IDs are given as alphanumeric characters with
          ## 3 characters.
          ## if(any(nchar(geneIDs$Well)!=3)) 
          ##    stop(sprintf(paste("Well IDs in the gene annotation file '%s' must ",
          ##                       "contain 1 letter and 2 digits. E.g. 'A02'."), geneIDFile))
          ## geneIDs <- geneIDs[order(geneIDs$Plate, geneIDs$Well),]
          ## Why not simply convert wellIDs to that format? Sorting is also not needed
          ## anymore because we map by name
          geneIDs$Well <- standardizeWellID(geneIDs$Well)

          ## Some checkings for dimension of "Plate" and "Well"
          ## expect prod(x@pdim) * x@nrPlate rows. We warn if there are eroneous entries
          ## but since we map to rownames we can still procede
          pdims <- pdim(object)
          wcoord <- convertWellCoordinates(geneIDs$Well, pdims)
          nrWpP <- prod(pdim(object))
          nrPlate <- max(plate(object))
          plates <- rep(seq_len(nrPlate), each=nrWpP)
          if(nrow(geneIDs) != nrWpP*nrPlate){
              warning(paste("Incomplete input file '", geneIDFile, "':\n for full annotation",
                            " we expect ", nrWpP*nrPlate, " rows,\n one for each of the ",
                            nrWpP, " wells on each of the ", nrPlate, " plates,\n but only ",
                            nrow(geneIDs), " have been provided.\n The available annotation ",
                            "information has been mapped,\n but for some probes it will be ",
                            "missing.\n", sep=""), call.=FALSE)
          }else if(!(all(wcoord$num %in% seq_len(nrWpP)) && all(geneIDs$Plate %in% plates)))
              warning(paste("Invalid values in the  input file '", geneIDFile, "' are being",
                            " ignored for annotation./n", sep=""))

          ## We want to allow for missing entries in the annotation file, so we first
          ## create a complete table and map values by name (combination of plate and
          ## well)
          wells <- expand.grid(1:pdims['ncol'], 1:pdims['nrow'])
          wells <- getAlphaNumeric(horizontal=wells[,2], vertical=wells[,1])$id
          wells <- rep(wells, nrPlate)

  	  rnames <- paste(plates, wells, sep="_")
          annTab <- as.data.frame(matrix(NA, ncol=ncol(geneIDs), nrow=nrWpP*nrPlate,
                                         dimnames=list(rnames, colnames(geneIDs))))
          annTab[paste(geneIDs$Plate, geneIDs$Well, sep="_"),] <- geneIDs

          ## store the geneIDs data.frame into the featureData slot
          ## 'annotation(object)' returns a character vector indicating the annotation package
          geneIDs <- annTab[, !c(names(annTab) %in% c("Plate", "Well")), drop=FALSE]
          ## flag 'NA' values in the "GeneID" column:
          geneIDs[apply(geneIDs, 2, function(i) i %in% "NA")] <- NA 
          
          ## add the columns into featureData:
          fData(object)[names(geneIDs)] <- geneIDs
          fvarMetadata(object)[names(geneIDs),]=names(geneIDs)

          ## change status and return object
          object@state[["annotated"]] = TRUE
          validObject(object)
          return(object)
      })



## Read the content of the plate config file needed for 'configure',
## or alternatively evaluate the user-defined function to prepare the
## necessary data structure: a list with two elements, a integer
## vector of length 2 giving the total number of wells and plates,
## respetively, and the actual configuration table.
getConfiguration <- function(filename, path, nrPlate, nrWpP, ...)
{
    if(is.character(filename))
    {
        if(is.na(path)) path <- dirname(filename)
        filename <- basename(filename)
        fp <- file.path(path, filename)
        if(!file.exists(fp))
            stop(sprintf("The file '%s' could not be found in the given path '%s'.",
                         filename, path))
        tt <- readLines(file.path(path, filename), n=2, warn=FALSE)
        ## Parse the header line. We try to grep and gsub ourselves to reasonable values here
        hinfo <- list(Wells=grep("^Wells:", gsub("^ *| *$", "", tt)),
                      Plates=grep("^Plates:", gsub("^ *| *$", "", tt)))
        tt <- sapply(sapply(tt, strsplit, split=":"), "[", 2L)
        tt <- as.integer(gsub(" .*", "", gsub("^ *", "", tt)))
        if(any(listLen(hinfo)==0))
            stop(sprintf(paste("Could not find all expected header rows ('Wells' and 'Plates')",
                               "in plate configuration file '%s'.\n"), filename))
        conf <- read.table(file.path(path, filename), sep="\t", header=TRUE, as.is=TRUE,
                           na.strings="", fill=TRUE, skip=2)
    }
    else if(is.function(filename))
    {
        conf <- filename(...)
        tt <- conf[[1]]
        conf <- conf[[2]]
        hinfo <- list(Plates=1, Wells=2)
        filename <- "Created by function"
    }
    else
    {
        stop("'filename' must be either a character scalar or a function.\n",
             "Please see '?readPlateList' for details.") 
    }  
    ## Check for consistency
    if(tt[hinfo$Plates] != nrPlate)
        stop(sprintf(paste("in '%s', the number of plates \n specified in the header",
                           "line 'Plates:' is %d but I expected %d."), filename, nrPlate,
                     tt[hinfo$Plates]))
    if(tt[hinfo$Wells] != nrWpP)
        stop(sprintf(paste("in '%s', the number of wells per plate \n specified in the",
                           "header line 'Wells:' is %d but I expected %d!"), filename, nrWpP,
                     tt[hinfo$Wells]))  

    checkColumns(conf, filename, mandatory=c("Plate", "Well", "Content"),
                 numeric=integer(0))
    return(conf)
}


## Read the content of the screen log file needed for 'configure', or
## alternatively evaluate the user-defined function to prepare the
## necessary data structure: a data.frame with mandatory columns
## "Plate", "Well" and "Flag", and optional columns "Sample" and
## "Channel" (if there are multiple replicates or channels). Further
## columns are allowed.
getScreenlog <- function(filename=NULL, path, nrSample, nrChannel, nrPlate, ...)
{
    slog <- NULL
    if(!is.null(filename))
    {
        if(is.character(filename))
        {
            if(is.na(path)) path <- dirname(filename)
            filename <- basename(filename)
            fp <- file.path(path, filename)
            if(!file.exists(fp))
                stop(sprintf("The file '%s' could not be found in the given path '%s'.",
                             filename, path))
            slog <- read.table(file.path(path, filename),  sep="\t", header=TRUE,
                               as.is=TRUE, na.strings="", fill=TRUE)
        }
        else if(is.function(filename))
        {
            slog <- filename(...)
            filename <- "Created by function"
        }
        else
        {
            stop("'filename' must be either a character scalar or a function.\n",
                 "Please see '?readPlateList' for details.") 
        }
    }
    ## Check for consistency
    if(!is.null(slog))
    {
        if(nrow(slog))
        {
            ## check consistency of columns 'Plate', 'Channel' and 'Sample'
            for(i in c("Sample", "Channel"))
            {
                if(!(i %in% names(slog))) 
                    slog[[i]] <- rep(1L, nrow(slog)) 
                else {
                  expectedSymbols = seq_len(get(paste("nr", i, sep="")))
                  if(!all(slog[[i]] %in% expectedSymbols)) 
                    stop(sprintf(paste("Column '%s' of the screen log file '%s'",
                    "contains invalid entries. Expected were %s; found was %s."),
                    i, filename, paste(expectedSymbols, collapse=", "),
                                 paste(unique(setdiff(slog[[i]], expectedSymbols)), collapse=", ")))
                }
            }
            checkColumns(slog, filename, mandatory=c("Plate", "Well", "Flag", "Sample", "Channel"),
                         numeric=c("Plate", "Sample", "Channel"))
            invalidPlateID <- !(slog$Plate %in% 1:nrPlate)
            if(sum(invalidPlateID))
                stop(sprintf(paste("Column 'Plate' of the screen log file '%s' contains",
                                   "invalid entries."), filename))
        }
        else
        {
            slog <- NULL
            warning("The screen log file is empty and will be ignored.") 
        }
    }
    return(slog)
} 



## Read the content of the screen description file needed for 'configure', or
## alternatively evaluate the user-defined function to prepare the
## necessary data structure: the MIAME-compliant description vector
getScreendesc <- function(filename, path, ...)
{
    if(is.character(filename))
    {
        if(is.na(path)) path <- dirname(filename)
        filename <- basename(filename)
        fp <- file.path(path, filename)
        if(!file.exists(fp))
            stop(sprintf("The file '%s' could not be found in the given path '%s'.",
                         filename, path))
        descript <- readLines(file.path(path, filename), warn=FALSE)
    }
    else if(is.function(filename))
    {
        descript <- filename(...)
    }
    else
    {
        stop("'filename' must be either a character scalar or a function.\n",
             "Please see '?readPlateList' for details.") 
    }  
    return(descript)
}


## Functions for dealing with alphanumeric identifiers for larger well plates
## There may be one or two letters in the string
getAlphaNumeric = function(horizontal, vertical) {
    alpha1=(horizontal-1) %/% length(LETTERS)
    alpha1 = ifelse(alpha1>0, LETTERS[alpha1], '')
    alpha2=LETTERS[(horizontal-1) %% length(LETTERS) +1]
    id.num=sprintf('%02d', vertical)
    id.alpha=paste(alpha1, alpha2, sep='')
    id=paste(id.alpha, id.num, sep='')
    return(list(id=id, id.alpha=id.alpha, id.num=id.num))
}
 
standardizeWellID = function(x) {
    x<-as.character(x)
    x<-parseLetNum(x)
    let.num <- cbind(x$letters, sprintf("%02d",x$numbers))
    letnum <- apply(let.num, 1, paste, collapse="")
    return(letnum)
}


##----------------------------------------
## configure
##----------------------------------------
setMethod("configure", signature("cellHTS"),
          function(object, descripFile, confFile,
                   logFile, path=NA, descFunArgs=NULL,
                   confFunArgs=NULL, logFunArgs=NULL)
      {
          
          ## If 'path' is given, we assume that all the files are in this directory.
          if (!missing(path))
	    if (!is.na(path)) 
              if(!(is.character(path)&&length(path)==1))
                  stop("'path' must be character of length 1")

          ## get dimensions:
          nrWpP <- prod(pdim(object))
          nrPlate <- max(plate(object))
          nrSample <- ncol(object)
          chNames <- channelNames(object)
          nrChannel <- length(chNames)
          xraw <- Data(object)
          pWells <- well(object)[1:nrWpP] 

          ## Process the plate annotation file
          conf <- do.call(getConfiguration, args=c(list(filename=confFile, path=path,
                                                        nrPlate=nrPlate, nrWpP=nrWpP),
                                                   confFunArgs))
          
          ## Process the screen log file
          if(missing(logFile))
              logFile <- NULL
          slog <- do.call(getScreenlog, args=c(list(filename=logFile, path=path,
                                                    nrSample=nrSample, nrChannel=nrChannel,
                                                    nrPlate=nrPlate), logFunArgs))
          
          ## Process the description file
          descript <- do.call(getScreendesc, c(list(filename=descripFile, path=path),
                                               descFunArgs))

          ## Store the contents of the description file in the 'experimentData' slot which
          ## is accessed via description(object):
          miameList <- list(sepLab=grep("Lab description", descript),
                            name=grep("^Experimenter name:", descript),
                            lab=grep("^Laboratory:", descript),
                            contact=grep("^Contact information:", descript),
                            title=grep( "^Title:", descript),
                            pubMedIds=grep( "^PMIDs:",descript),
                            url=grep("^URL:",descript),
                            abstract=grep("^Abstract:",descript))
          
          miameInfo <- lapply(miameList, function(i) unlist(strsplit(descript[i], split=": "))[2L]) 
          miameInfo <- lapply(miameInfo, function(i) if(is.null(i)) "" else { if(is.na(i)) "" else i })
          miameInfo <- with(miameInfo, new("MIAME", 
                                           name=name,
                                           lab=lab,
                                           contact=contact,
                                           title=title,
                                           pubMedIds=pubMedIds,
                                           url=url,
                                           abstract=abstract))

          ## store the rest of the description information into slot "other":
          otherInfo <- descript[-unlist(miameList)]
          ## otherInfo <- otherInfo[nchar(otherInfo)>0] #remove empty lines
          ## notes(miameInfo) <- unlist(lapply(otherInfo, function(i) append(append("\t",i), "\n")))
          notes(miameInfo) <- lapply(otherInfo, function(i) paste("\t", i, "\n", collapse=""))         

          ## Process the configuration file into wellAnno slot
          ## and set all 'empty' wells to NA in object
          pcontent <- tolower(conf$Content)  ## ignore case!
          wAnno <- factor(rep(NA, nrWpP*nrPlate), levels=unique(pcontent))

          ## In the current plate configuration file, the concept of 'batch' is separated
          ## from the plate configuration issue. However it might make sense to be able to
          ## specify batches at this level. Have to think about how that would work.
          conf[conf=="*"] <- " *"
          pdimo = pdim(object)
          for (i in 1:nrow(conf))
          {
              iconf <- conf[i,]
              ## get plate IDs
              if(iconf$Plate != " *")
                  iconf$Plate <- gsub("^ *| *$", "", iconf$Plate)
              c2i <- suppressWarnings(as.integer(iconf$Plate))
              wp <- if(!is.na(c2i)) c2i  else c(1:nrPlate)[regexpr(iconf$Plate, 1:nrPlate)>0]
              ## get well IDs (remove heading and trailing whitespace)
              if(iconf$Well != " *")
                  iconf$Well <- gsub("^ *| *$", "", iconf$Well)
              ww <- convertWellCoordinates(pWells[regexpr(iconf$Well, pWells)>0], pdimo)$num
              if(!length(wp))
                  stop(sprintf(paste("In the plate configuration file '%s', no plate matches",
                                     "were found for rule specified by line %d:\n\t %s \n\t %s"),
                               if(is.function(confFile)) "Created from file" else confFile,
                               i, paste(names(conf), collapse="\t"),
                               paste(iconf, collapse="\t")))

              if(!length(ww))
                  stop(sprintf(paste("In the plate configuration file '%s', no well matches",
                                     "were found for rule specified by line %d:\n\t %s \n\t %s"),
                               if(is.function(confFile)) "Created from file" else confFile,
                               i, paste(names(conf), collapse="\t"),
                               paste(iconf, collapse="\t")))
 
              wAnno[ww + rep(nrWpP*(wp-1), each=length(ww))] <- pcontent[i] 
          }

          ## Each well and plate should be covered at leat once.
          ## Allow duplication and consider the latter occurence.
          missAnno <- is.na(wAnno)
          if(sum(missAnno))
          {
              ind <- which(missAnno)[1:min(5, sum(missAnno))]
              msg <- paste("The following plates and wells were not covered in the plate configuration file\n",
                           "'", confFile, "':\n",
                           "\tPlate Well\n", "\t",
                           paste((ind-1) %/% nrWpP + 1,  1+(ind-1)%%nrWpP, sep="\t",
                                 collapse="\n\t"),
                           if(sum(missAnno)>5) sprintf("\n\t...and %d more.\n",
                                                       sum(missAnno)-5), "\n", sep="")
              stop(msg)
          }

          ## get empty positions from the final well anno and flag them in each replicate
          ## and channel
          empty <- which(wAnno=="empty")
          xraw[] <- apply(xraw, 2:3, replace, list=empty, NA) 

          ## store the conf data.frame into the 'plateConf' slot of 'object' and
          ## slog into the 'screenlog' slot
          ## descript into the screenDesc slot
          object@plateConf <- conf
          object@screenLog <- slog
          object@screenDesc <- descript
          
          ## Process the configuration file into 'controlStatus' column of featureData slot
          ## and set all 'empty' wells to NA in assayData slot

          ## Process screenlog
          if (!is.null(slog))
          {
              ipl <- slog$Plate
              irep <- slog$Sample
              ich <- slog$Channel
              ipos <- convertWellCoordinates(slog$Well, pdim(object))$num
              stopifnot(!any(is.na(ipl)), !any(is.na(irep)), !any(is.na(ich)))         
              xraw[cbind(ipos + nrWpP*(ipl-1), irep, ich)] <- NA 
          } 

          ## update object (measurements and well anno) 
          Data(object) <- xraw

          ## update well anno information: 
          fData(object)$controlStatus <- wAnno
          stopifnot(all(fData(object)$controlStatus!="unknown"))

          ## add the 'miame'-like description:
          description(object) <- miameInfo
          object@state[["configured"]] <- TRUE
          validObject(object)
          return(object)
      } )





##----------------------------------------
## Export the contents of assayData slot of 'object' to a file as .txt
##----------------------------------------
setMethod("writeTab", signature("cellHTS"), 
   function(object, file=paste(name(object), "txt", sep=".")) {

if(is.null(Data(object))) stop("No available data in 'object'!")

  toMatrix = function(y, prefix) {
    m = matrix(y, ncol=prod(dim(y)[2:3]), nrow=dim(y)[1]) #(wells and plates) x (replicates and channels)
    #colnames(m) = sprintf("%s-col%dChan%d", prefix, rep(1:dim(y)[2], dim(y)[3]), rep(1:dim(y)[3], each=dim(y)[2]))
    colnames(m) = sprintf("%s[[%d]][,%d]", prefix, rep(1:dim(y)[3], each=dim(y)[2]), rep(1:dim(y)[2], dim(y)[3]))
    return(m)
  }

  # prefix = if(state(object)[["scored"]]) "scores" else ifelse(state(object)[["normalized"]], "norm", "raw")


  #out = cbind(geneAnno(object), toMatrix(Data(object), prefix))
  out <- toMatrix(Data(object), prefix="assayData")
  out <- if(state(object)[["annotated"]]) cbind(geneAnno(object), out)
  write.table(out, file=file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
  return(file)
})


##----------------------------------------
## Compares two cellHTS objects to find out whether they derived from the same initial cellHTS object
##----------------------------------------
## Generic method for cellHTS class
## Function that compares different cellHTS objects to 
setMethod("compare2cellHTS",
          signature(x="cellHTS", y="cellHTS"),
          function(x, y) {

   out <- tryCatch({
     # slots that should always be defined when a cellHTS object was created by reading input data files
     stopifnot(identical(name(x), name(y)),
            identical(plateList(x), plateList(y)),
            identical(intensityFiles(x), intensityFiles(y)),
            identical(dim(x)[1], dim(y)[1]),
            identical(pdim(x), pdim(y)),
            identical(plate(x), plate(y)),
            identical(well(x), well(y))
            )
    if(state(x)[["configured"]] & state(y)[["configured"]]) {
        stopifnot(identical(plateConf(x), plateConf(y)),
                  identical(screenLog(x), screenLog(y)),
 		  identical(screenDesc(x), screenDesc(y)),
                  identical(experimentData(x), experimentData(y)),
                  identical(wellAnno(x), wellAnno(y)))
      }

     if(state(x)[["annotated"]] & state(y)[["annotated"]])  stopifnot(identical(geneAnno(x), geneAnno(y)))

     TRUE
             },
 #    warning = function(e) {
 #               paste(class(e)[1], e$message, sep=": ")
 #             }, 
     error = function(e) {
                return(FALSE) #paste(class(e)[1], e$message, sep=": ")
              }
   )
# don't consider classVersion(x) vs classVersion(y)
return(out)
}
)


##-------------------------------------------
## Generate data to plot a ROC curve from the scored data
##-------------------------------------------
setMethod("ROC", signature("cellHTS"), 

   function(object, positives, negatives){
##'positives' and 'negatives' is a vector of characters specifying the name of the controls
    if(!state(object)["scored"])
    stop("Please score 'object' using the function 'summarizeReplicates' before creating the ROC object.")

 # default
 assayType <- "one-way assay"
 score <- as.vector(Data(object))

  if (!missing(positives)) {
## checks
      if(!is(positives, "list")){ 
        checkControls(positives, len=1, name="positives")
      }else{

        checkControls2W(positives, len=1, name="positives")
        positives <- paste(positives, collapse="|")
        score <- abs(score) # because this is a two way assay
        assayType <- "two-way assay"
      }## else is list

    }else{## if !missing
## assumes the screen is a one-way assay
      positives = "^pos$"
    }


    if (!missing(negatives)) {
      ## check
      checkControls(negatives, len=1, name="negatives")
    } else {
      negatives = "^neg$"
    }


  wAnno <- as.character(wellAnno(object))

  xpos <- regexpr(positives, wAnno, perl=TRUE)>0
  xneg <- regexpr(negatives, wAnno, perl=TRUE)>0

  if(!any(xneg))
    stop("Negative controls not found")
    #stop(sprintf("The 'wellAnno' slot does not contain any entries with value '%s'.", negatives))
  if(!any(xpos))
    stop("Positive controls not found")
    #stop(sprintf("The 'wellAnno' slot does not contain any entries with value '%s'.", positives))

  br <- unique(quantile(score, probs=seq(0, 1, length=1001), na.rm=TRUE))
  ct  <- cut(score, breaks=br)
  spNeg <- split(xneg, ct)
  spPos <- split(xpos, ct)
  nNeg <- sapply(spNeg, sum)
  nPos <- sapply(spPos, sum)
  stopifnot(all(names(nPos)==names(nNeg)))

  posNames <- unique(wAnno[xpos])
  posNames <- plateConf(object)$Content[match(posNames, tolower(plateConf(object)$Content))]
  negNames <- unique(wAnno[xneg])
  negNames <- plateConf(object)$Content[match(negNames, tolower(plateConf(object)$Content))]

  x <- new("ROC", 
        name=name(object),
        assayType = assayType,
        TP = cumsum(rev(nPos)),
        FP = cumsum(rev(nNeg)),
           #positives = positives,
           #negatives = negatives, 
        posNames = posNames,
        negNames = negNames)

  return(x) 
})



## MeanSdPlot from vsn package
setMethod("meanSdPlot", signature="cellHTS", definition =
   function(x, ranks = TRUE, xlab = ifelse(ranks, "rank(mean)", "mean"),
            ylab = "sd", pch  = ".", plot = TRUE, ...) {
      xdat <- Data(x)
      d <- dim(xdat)
      nCh <- d[3]
      nRep <- d[2]

      if(!state(x)[["configured"]]) stop("Please configure 'x' before calling this function.")
      if(nRep<2) stop("'x' contains data only for 1 sample! Cannot do the 'sd vs mean' plot!")

      ## only consider sample wells
      toconsider <- wellAnno(x)=="sample"

      nc <- ceiling(sqrt(nCh))
      nr <- ceiling(nCh/nc)

      if(plot)
        par(mfrow=c(nr, nc))
      
      invisible(lapply(seq_len(nCh),
        function(ch)
          meanSdPlot(as.matrix(xdat[toconsider,,ch]),
                     main=sprintf("across samples, channel %d\n(only sample wells)", ch),
                     ranks=ranks, xlab=xlab, ylab=ylab, pch=pch, plot=plot, ...))) 
})



## Replace the channel names in a cellHTS2 object
setReplaceMethod("channelNames",
		signature=signature(object="cellHTS", value="character"),
		function(object, value) 
		{
			if(length(channelNames(object)) != length(value))
				stop("Length of replacement values does not match number of channels.")
			if(storageMode(object) == "list") 
				names(assayData(object)) <- value
			else {
				keys <- assayDataElementNames(object) 
				for(i in seq_along(keys)) {
					assayDataElement(object, value[i]) <- assayDataElement(object, keys[i])
					assayDataElement(object, keys[i]) <- NULL
				}
			}
			return(object)
		})
