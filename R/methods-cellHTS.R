#------------------------------------------------------------
# methods related to the class 'cellHTS'
#------------------------------------------------------------

setMethod("initialize",
          signature(.Object="cellHTS"),
          function(.Object, assayData, phenoData, featureData,
                   ...) {
              mySlots <- slotNames(.Object)
              dotArgs <- list(...)
              isSlot <- names(dotArgs) %in% mySlots
              if (missing(assayData)) {
                  assayData <- do.call(assayDataNew, dotArgs[!isSlot],
                                       envir=parent.frame())
              }
              if (missing(phenoData)) {
                  phenoData <- annotatedDataFrameFrom(assayData, byrow=FALSE)
                  ## add the mandatory columns: "plate" and "assay"
                  pData(phenoData) <- data.frame(replicate=integer(), assay=character())
                  varMetadata(phenoData) <- data.frame(
                      labelDescription=I(c("Replicate number", "Biological assay")))
               } 

              if (is.null(varMetadata(phenoData)[["channel"]])) {
                  varMetadata(phenoData)[["channel"]] <- 
                      factor(rep("_ALL_", nrow(varMetadata(phenoData))),
                             levels=c(assayDataElementNames(assayData), "_ALL_"))
              }

          if (missing(featureData)) {
             featureData <- annotatedDataFrameFrom(assayData, byrow=TRUE)
             pData(featureData) <- data.frame(plate=integer(), well=I(character()), controlStatus=factor(integer()))
             varMetadata(featureData) <- data.frame(labelDescription=I(c("Plate number", "Well ID", "Well annotation")))
          }

              ## ensure sample names OK -- all assayData with names;
              ## phenoData with correct names from assayData
              appl <-
                  if (storageMode(assayData)=="list") lapply
                  else eapply
              assaySampleNames <-
                  appl(assayData, function(elt) {
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
          function(object) {
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
              cat("state: ", paste(paste(names(state(object)),state(object), sep=" = "), collapse="\n\t"), "\n") 
              cat("Number of plates:", if(length(plate(object))) max(plate(object)), "", "\n")
              cat("Plate dimension:", if(length(pdim(object))) paste(paste(names(pdim(object)), pdim(object), sep=" = "), collapse=", "), "\n")
              cat("Number of batches:", nbatch(object), "\n")
              cat("Well annotation:", paste(levels(wellAnno(object))), "\n")
              pmids <- pubMedIds(object)
              if (length(pmids) > 0 && all(pmids != ""))
                  cat("  pubMedIds:", paste(pmids, sep=", "), "\n")
              cat("Annotation:", annotation(object), "\n")
          })


##----------------------------------------
## Data accessors and replacement functions
##----------------------------------------
# plate
setMethod("plate", signature(object="cellHTS"),
          function(object) fData(object)$"plate" )

# well
setMethod("well", signature(object="cellHTS"),
          function(object) {
             w = fData(object)$"well" 
             return(if(!length(w)) NULL else w)
             }
        )


# plate dimension
setMethod("pdim", signature(object="cellHTS"),
          function(object) {
             pdim=NULL
             if(!is.null(well(object))) { 
               let=substr(well(object), 1,1)
               pdim=c("nrow"=max(match(let, LETTERS)),
                      "ncol"=max(as.integer(substr(well(object), 2,3))))
             }
             return(pdim)
          }
)

# well position
setMethod("position", signature(object="cellHTS"),
          function(object) 
            if(!is.null(well(object))) 
               return(convertWellCoordinates(well(object), pdim=pdim(object))$num)
            else
               return(NULL)
)


# well annotation
setMethod("wellAnno", signature(object="cellHTS"),
          function(object) 
             if(state(object)[["configured"]]) 
                fData(object)$controlStatus
             else
                NULL
)


setMethod("geneAnno", signature(object="cellHTS"),
          function(object) 
              if(state(object)[["annotated"]]) fData(object)$GeneID else NULL
)

## the assayData is overriden each time new data are generated from the analysis.
setMethod("Data", signature(object="cellHTS"),
        function(object){
             ch <- channelNames(object)
             dat=NULL
             if(length(ch)) {
               dat <- array(NA, dim=c(dim(object), "Channels"=length(ch)))
               #NB: we assume that sampleNames is identical across channels
               dimnames(dat)=list(Features=featureNames(object), Sample=sampleNames(object)[[1]], Channels=ch)
               dat[] <- sapply(ch, function(i) assayDataElement(object, i))
             }
             return(dat) 
} 
)


setReplaceMethod("Data",
                 signature=signature(
                   object="cellHTS",
                   value="array"),
                 function(object, value) {
                      if(is.null(Data(object))) stop("'object' has no data! No replacement with 'value' can be made!")
                      ch <- channelNames(object)
                      # If 'value' is a matrix, set it as an array.
                      if(inherits(value, "matrix"))  value <- array(value, dim=c(dim(value),1))
                      d=c(dim(object), "Channels"=length(ch))
                      if(any(dim(value)!=d)) stop(sprintf("'value' should be an array with dimensions 'Features x Samples x Channels' (%s).", paste(d, collapse=" x "))) 
                      #NB: we assume that sampleNames is identical across channels
                      if(is.null(dimnames(value))) dimnames(value) <- list(featureNames(object), sampleNames(object)[[1]], channelNames(object))


                      for(i in c(1:length(ch))) assayDataElement(object, ch[i]) <- matrix(value[,,i], nrow=d[1], ncol=d[2], 
                           dimnames=dimnames(value)[1:2])

                      if(any(dimnames(value)[[3]]!=channelNames(object))) channelNames(object) <- dimnames(value)[[3]]
                      sampleNames(phenoData(object)) <- sampleNames(object)[[1]] #in case 'value' had different sample names in it.
                      featureNames(object) <- dimnames(value)[[1]]

                      #sampleNames(assayData(object)) <- sampleNames(phenoData(object))
                      validObject(object)
                      return(object)
         } 
          )

## accessors to see the overall state of the cellHTS object:
setMethod("state", signature(object="cellHTS"),
        function(object) object@state
       )

setMethod("plateList", signature(object="cellHTS"),
         function(object) object@plateList
  )

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
 
 



##------------------------------------------------------------------------------
## Function to create an empty Description file with default entries (compliant with MIAME class and with additional entries specific for an RNAi experiment
##------------------------------------------------------------------------------


newDescriptionFile <- function(filename="Description.txt", path) {
# If 'path' is given, check if it exists. If not, create it. If yes, check is the 'filename' file already exits and return an error message if yes.
if (!missing(path)) {
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")
} else {
  path=dirname(filename)
}

  file = basename(filename)
 
  f = file.path(path, file)

  if(file.exists(path)){
    if(file.exists(f))
      stop(sprintf("'%s' already exists!", f))
  } else {
    dir.create(path, recursive=TRUE)
  }

desc=c("[Lab description]",
"Experimenter name: <put here the experimenter name>",
"Laboratory: <put here the name of the laboratory where the experiment was conducted>",
"Contact information: <put here contact information for lab and/or experimenter>",
"",
"[Screen description]",
 "Screen: <put here the screen name>",
 "Title: <put there the single-sentence giving the experiment title>",
 "Version: <put here the screen version>",
 "Date: <put here the date when the experiment was performed>",
 "Screentype: <put here the type of screen>",
 "Organism:",
 "Celltype:",
 "Library:",
 "Assay: <put here the name of the assay>",
 "Assaytype: <put here the type of assay>",
 "Assaydescription: <put here the description of the assay>",
 "",
 "[Publication description]",
 "Publicationtitle:",
 "Reference:",
 "PMIDs: <put here the PubMed identifiers of papers relevant to the dataset>",
 "URL: <put here the URL for the experiment>",
 "License:",
 "Abstract: <put here the abstract describing the experiment>",
 "",
 "[Files]",
 "plateList: <put the name of the plate result list file>",
 "annotation: <put the name of the screen library annotation file>",
 "plateConf: <put the name of the screen plate configuration file>",
 "screenLog: <put the name of screen log file, if available>"
)

 writeLines(desc, f)
}
#--------------------------------------------------------


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
             bb = slot(object, "batch")
             if(!length(bb)) bb=NULL
             return(bb) 
          })

setReplaceMethod("batch",
                 signature=signature(
                   object="cellHTS",
                   value="array"),
                 function(object, value) {
                      if(is.null(Data(object))) stop("'object' has no data yet! 'batch' can only be added to a non-empty cellHTS object!")
                      d = dim(Data(object))
                      # If 'value' is a matrix, set it as an array.
                      if(inherits(value, "matrix"))  value <- array(value, dim=c(dim(value),1))
                      if(any(dim(value)!=d)) stop(sprintf("'value' should be an array with dimensions 'Features x Samples x Channels' (%s).", paste(d, collapse=" x "))) 
                      if(!is.integer(value)) stop("'value' should be an array of integer values corresponding to the batch number for each plate, sample and channel!")

                      object@batch <- value
                      validObject(object)
                      return(object)
         } 
          )


setMethod("nbatch", signature(object="cellHTS"),
          function(object) {
             bb = batch(object)
             if(!is.null(bb)) bb <- length(unique(as.vector(bb))) else bb=NULL
             return(bb)
          })




##----------------------------------------
## annotate
##----------------------------------------
setMethod("annotate", signature("cellHTS"),
function(object, geneIDFile, path=dirname(geneIDFile)) {

  file = basename(geneIDFile)
 
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")

  geneIDs = read.table(file.path(path, file), sep="\t", header=TRUE, as.is=TRUE, na.string="", quote="",fill=TRUE)

  checkColumns(geneIDs, file, mandatory=c("Plate", "Well", "GeneID"),
               numeric=c("Plate"))

  ## sort the data by Plate and then by well
  ## ordering by well might be problematic when we have "A2" instead of "A02"... so first check if all well IDs are given as alphanumeric characters with 3 characters.
if(any(nchar(geneIDs$Well)!=3)) 
    stop(sprintf("Well IDs in the gene annotation file '%s' must contain 1 letter and 2 digits. E.g. 'A02'.", geneIDFile))


  geneIDs = geneIDs[order(geneIDs$Plate, geneIDs$Well),]

  ## Some checkings for dimension of "Plate" and "Well"
  ## expect prod(x@pdim) * x@nrPlate rows
  nrWpP   = prod(pdim(object))
  nrPlate = max(plate(object))

  if (!((nrow(geneIDs)==nrWpP*nrPlate) && all(convertWellCoordinates(geneIDs$Well, pdim(object))$num==rep(1:nrWpP, times=nrPlate)) &&
       all(geneIDs$Plate == rep(1:nrPlate, each=nrWpP))))
    stop(paste("Invalid input file '", geneIDFile, "': expecting ", nrWpP*nrPlate,
               " rows, one for each well and for each plate. Please see the vignette for",
               " an example.\n", sep=""))


  ## store the geneIDs data.frame into the geneAnno slot of x
  ## annotation(object) <- # should be the name of the ...  'annotation(object)' returns a character vector indicating the      annotation package

  geneIDs <- geneIDs[, !c(names(geneIDs) %in% c("Plate", "Well"))]
  ## flag 'NA' values in the "GeneID" column:
  #geneIDs$GeneID[geneIDs$GeneID %in% "NA"] = NA
  geneIDs[apply(geneIDs, 2, function(i) i %in% "NA")] <- NA 

  ## add the columns into featureData:
  fData(object)[names(geneIDs)] <- geneIDs
  fvarMetadata(object)[names(geneIDs),]=names(geneIDs)


  object@state[["annotated"]] = TRUE
  validObject(object)
  return(object)
})


##----------------------------------------
## configure
##----------------------------------------
setMethod("configure", signature("cellHTS"),
  function(object, descripFile, confFile, logFile, path) {

# If 'path' is given, we assume that all the files are in this directory.
if (!missing(path)) 
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")

# get dimensions:
  nrWpP   = prod(pdim(object))
  nrPlate = max(plate(object))
  nrSample <- ncol(object)
  chNames <- channelNames(object)
  nrChannel <- length(chNames)
  # assumes that 'object' contains raw data!
  xraw <- Data(object)

  ppath = ifelse(missing(path), dirname(confFile), path)
  confFile = basename(confFile)

  tt = readLines(file.path(ppath, confFile), n=2)
  hinfo = list(Wells = grep("^Wells:", tt),
              Plates  =   grep("^Plates:", tt))

  if(any(listLen(hinfo)==0))
    stop(sprintf("Could not find all expected header rows ('Wells' and 'Plates') in plate configuration file '%s'.\n", confFile))

  tt <- sapply(tt, strsplit, split=":")
  tt <- as.integer(sapply(tt, "[", 2L))

  if(tt[hinfo$"Plates"] != nrPlate) stop(sprintf("in plate screen log file '%s', the number of plates \n specified in the row header 'Plates:' should be %d instead of %d!", confFile, nrPlate, tt[hinfo$"Plates"]))

  if(tt[hinfo$"Wells"] != nrWpP) stop(sprintf("in plate screen log file '%s', the number of wells per plate \n specified in the row header 'Wells:' should be %d instead of %d!", confFile, nrWpP, tt[hinfo$"Wells"]))


  ## Check if the screen log file was given
  slog=NULL
  if(!missing(logFile)) {
    ppath = ifelse(missing(path), dirname(logFile), path)
    logFile = basename(logFile)
    slog = read.table(file.path(ppath, logFile),  sep="\t", header=TRUE, as.is=TRUE, na.string="", fill=TRUE)
    ## Check if the screen log file is empty
    if (nrow(slog)) {
      ## check consistency of columns 'Plate', 'Channel' and 'Sample'
        for(i in c("Sample", "Channel")) {
               if(!(i %in% names(slog))) 
                 slog[[i]] <- rep(1L, nrow(slog)) 
               else 
                 if(!all(slog[[i]] %in% 1:get(paste("nr", i, sep="")))) stop(sprintf("Column '%s' of the screen log file '%s' contains invalid entries.", i, logFile))
        }

        checkColumns(slog, logFile, mandatory=c("Plate", "Well", "Flag", "Sample", "Channel"), numeric=c("Plate", "Sample", "Channel"))

     invalidPlateID <- !(slog$Plate %in% 1:nrPlate)
     if(sum(invalidPlateID)) stop(sprintf("Column 'Plate' of the screen log file '%s' contains invalid entries.",logFile))
    }
  } 


   ## Process the description file
   ppath = ifelse(missing(path), dirname(descripFile), path)
   descripFile = basename(descripFile)
   descript = readLines(file.path(ppath, descripFile))


   ## Store the contents of the description file in the 'dexperimentData' slot which is accessed via description(object):
   miameList = list(sepLab=grep("Lab description", descript),
        name = grep("^Experimenter name:", descript),
	lab  =   grep("^Laboratory:", descript),
        contact = grep("^Contact information:", descript),
	title=grep( "^Title:", descript),
	pubMedIds=grep( "^PMIDs:",descript),
 	url=grep("^URL:",descript),
	abstract=grep("^Abstract:",descript)
  )

  miameInfo = lapply(miameList, function(i) unlist(strsplit(descript[i], split=": "))[2L]) 
  miameInfo = lapply(miameInfo, function(i) if(is.na(i)) "" else i)
  miameInfo = with(miameInfo, new("MIAME", 
    name=name,
    lab = lab,
    contact=contact,
    title=title,
    pubMedIds=pubMedIds,
    url=url,
    abstract=abstract))

  #store the rest of the description information into slot "other":
  otherInfo <- descript[-unlist(miameList)]
  #otherInfo <- otherInfo[nchar(otherInfo)>0] #remove empty lines
  #notes(miameInfo) <- unlist(lapply(otherInfo, function(i) append(append("\t",i), "\n")))
  notes(miameInfo) <- lapply(otherInfo, function(i) paste("\t", i, "\n", collapse="")) 


   ## Process the plate annotation file:
   conf = read.table(file.path(ppath, confFile), sep="\t", header=TRUE, as.is=TRUE, na.string="", fill=TRUE, skip=2)

   checkColumns(conf, confFile, mandatory=c("Plate", "Well", "Content"),
               numeric=integer(0))  #column 'Plate' is no longer numeric

   pWells <- well(object)[1:nrWpP] 

   ## Process the configuration file into wellAnno slot
   ## and set all 'empty' wells to NA in object
   pcontent = tolower(conf$Content)  ## ignore case!
   wAnno = factor(rep(NA, nrWpP*nrPlate), levels=unique(pcontent))


   # In the current plate configuration file, the concept of 'batch' is separated from the plate configuration issue.
   conf[conf=="*"] <- " *"
   #count <- character() 

   for (i in 1:nrow(conf)) {
     iconf <- conf[i,]
     # get plate IDs
     wp <- if(is.numeric(iconf$Plate)) iconf$Plate  else  c(1:nrPlate)[regexpr(iconf$Plate, 1:nrPlate)>0]
     # get well IDs
     ww <- convertWellCoordinates(pWells[regexpr(iconf$Well, pWells)>0], pdim(object))$num
     #count <- append(count, sprintf("%d-%d-%s", rep(wp, each=length(ww)), rep(ww, length(wp)), rep(pcontent[i], length(wp)*length(ww))) )
     if(!length(wp)) stop(sprintf("In the plate configuration file '%s', no plate matches were found for rule specified by line %d:\n\t %s \n\t %s", confFile, i, paste(names(conf), collapse="\t"), paste(iconf, collapse="\t")))

     if(!length(ww)) stop(sprintf("In the plate configuration file '%s', no well matches were found for rule specified by line %d:\n\t %s \n\t %s", confFile, i, paste(names(conf), collapse="\t"), paste(iconf, collapse="\t")))
 
     wAnno[ww + rep(nrWpP*(wp-1), each=length(ww))] = pcontent[i] 
   }


  ## Each well and plate should be covered at leat once.
  ## Allow duplication and consider the latter occurence.
  missAnno <- is.na(wAnno)
  if(sum(missAnno)) {
    ind <- which(missAnno)[1:min(5, sum(missAnno))]
    msg = paste("The following plates and wells were not covered in the plate configuration file\n",
      "'", confFile, "':\n",
      "\tPlate Well\n", "\t",
      paste((ind-1) %/% nrWpP + 1,  1+(ind-1)%%nrWpP, sep="\t", collapse="\n\t"),
      if(sum(missAnno)>5) sprintf("\n\t...and %d more.\n", sum(missAnno)-5), "\n", sep="")
    stop(msg)
  }



    #get empty positions from the final well anno and flag them in each replicate and channel
    empty = which(wAnno=="empty")
    xraw[] = apply(xraw, 2:3, replace, list=empty, NA) 


  ## store the conf data.frame into the 'plateConf' slot of 'object' and
  ## slog into the 'screenlog' slot
  ## descript into the screenDesc slot
   object@plateConf = conf
   object@screenLog = slog
   object@screenDesc = descript

  ## Process the configuration file into 'controlStatus' column of featureData slot
  ## and set all 'empty' wells to NA in assayData slot

  ## Process screenlog
  if (!is.null(slog)) {
    ipl  = slog$Plate
    irep = slog$Sample
    ich  = slog$Channel
    ipos = convertWellCoordinates(slog$Well, pdim(object))$num
    stopifnot(!any(is.na(ipl)), !any(is.na(irep)), !any(is.na(ich)))

    xraw[cbind(ipos + nrWpP*(ipl-1), irep, ich)] = NA 
  } 

  ## update object (measurements and well anno) 
  Data(object) <- xraw

  ## update well anno information: 
  fData(object)$controlStatus=wAnno
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

