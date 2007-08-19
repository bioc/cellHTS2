#------------------------------------------------------------
# methods related to the class 'cellHTS'
#------------------------------------------------------------

checkDots = function(...) {
  v = list(...)
  if(length(v)>0) {
    print(str(v))
    stop("Unused arguments.")
  }
}

##----------------------------------------
## show
##----------------------------------------

setMethod("show", signature("cellHTS"),
  function(object) {
  d=dim(rawdata(object))
  bt=object@batch
  if(length(bt)==0) bt=0 else bt=max(bt) 
  cat(class(object), sprintf("object of name '%s'.\n", object@name))
  cat(sprintf("%d plates with %d wells, %d replicates, %d channel%s. \nExperiment conducted in %d batch%s. State:\n",
              d[2], d[1], d[3], d[4], ifelse(d[4]>1, "s", ""), bt, ifelse(bt>1, "es", "")))
  print(state(object))
  })


##----------------------------------------
## annotate
##----------------------------------------

setMethod("annotate", signature("cellHTS"),
function(object, geneIDFile, path=dirname(geneIDFile), ...) {
  checkDots(...)
 
  file = basename(geneIDFile)
 
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")

  geneIDs = read.table(file.path(path, file), sep="\t", header=TRUE, as.is=TRUE, na.string="", quote="",fill=TRUE)

  checkColumns(geneIDs, file, mandatory=c("Plate", "Well", "GeneID"),
               numeric=c("Plate"))

  ## sort the data by Plate and then by well
  geneIDs = geneIDs[order(geneIDs$Plate, geneIDs$Well),]
  
  ## Some checkings for dimension of "Plate" and "Well"
  ## expect prod(x@pdim) * x@nrPlate rows
  nrWpP   = prod(object@pdim)
  nrPlate = dim(rawdata(object))[2]

  if (!((nrow(geneIDs)==nrWpP*nrPlate) && all(pos2i(geneIDs$Well, object@pdim)==rep(1:nrWpP, times=nrPlate)) &&
       all(geneIDs$Plate == rep(1:nrPlate, each=nrWpP))))
    stop(paste("Invalid input file '", geneIDFile, "': expecting ", nrWpP*nrPlate,
               " rows, one for each well and for each plate. Please see the vignette for",
               " an example.\n", sep=""))

  ## flag 'NA' values in the "GeneID" column:
  geneIDs$GeneID[geneIDs$GeneID %in% "NA"] = NA
  ## store the geneIDs data.frame into the geneAnno slot of x
  object@geneAnno = geneIDs
  object@state["annotated"] = TRUE
  validObject(object)
  return(object)
})

##----------------------------------------
## configure
##----------------------------------------
setMethod("configure", signature("cellHTS"),
  function(object, confFile, logFile, descripFile, path, ...) {
  checkDots(...)
# If 'path' is given, we assume that all the files are in this directory.
if (!missing(path)) 
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")

  ppath = ifelse(missing(path), dirname(confFile), path)
  confFile = basename(confFile)
  conf = read.table(file.path(ppath, confFile), sep="\t", header=TRUE, as.is=TRUE, na.string="", fill=TRUE)

  ## Check if the screen log file was given
  if(!missing(logFile)) {
    ppath = ifelse(missing(path), dirname(logFile), path)
    logFile = basename(logFile)
    slog = read.table(file.path(ppath, logFile),  sep="\t", header=TRUE, as.is=TRUE, na.string="", fill=TRUE)
    ## Check if the screen log file is empty
    if (nrow(slog)==0)
      slog = NULL
    else
      checkColumns(slog, logFile, mandatory=c("Filename", "Well", "Flag"),
                   numeric=character(0))
  } else {
    slog = NULL
  }


   ppath = ifelse(missing(path), dirname(descripFile), path)
   descripFile = basename(descripFile)
   descript = readLines(file.path(ppath, descripFile))

  ## backward compatibility
  colnames(conf) = sub("^Pos$", "Position", colnames(conf))
  checkColumns(conf, confFile, mandatory=c("Batch", "Well", "Content"),
               numeric=c("Batch"))

  if ("Position" %in% colnames(conf)) {
    ## check consistency between 'Position' and 'Well' columns
    badRows = which(conf$Position!=pos2i(conf$Well, object@pdim))
    if(length(badRows)>0) {
      if(length(badRows)>5)
        badRows = badRows[1:5]
      msg = paste("The columns 'Position' and 'Well' in ", confFile, " are inconsistent:\n",
        paste("Row ", badRows, ": ", conf$Position[badRows], " != ", conf$Well[badRows],
              sep="", collapse="\n"),
        sep="")
      stop(msg)
    }
  } else {
    ## creates the "Position" column
    conf$Position=pos2i(conf$Well, object@pdim)
  }

  ## expect prod(x@pdim) * x@nrBatch rows
  nrWpP   = prod(object@pdim)
  nrBatch = max(object@batch)
  d <- dim(rawdata(object))
  nrPlate = d[2]
  stopifnot(nrWpP==d[1])
  
  if(!((nrow(conf)==nrWpP*nrBatch) && all(conf$Position==rep(1:nrWpP, nrBatch)) &&
       all(conf$Batch==rep(1:nrBatch, each=nrWpP))))
    stop(paste("Invalid input file '", confFile, "': expecting ", nrWpP*nrBatch,
               " rows, one for each well and for each batch. Please see the vignette for",
               " an example.\n", sep=""))

  ## store the conf data.frame into the plateConf slot of x and
  ## slog into the screenlog slot
  ## descript into the screenDesc slot
  object@plateConf = conf
  object@screenLog = slog
  object@screenDesc = descript

  ## Process the configuration file into wellAnno slot
  ## and set all 'empty' wells to NA in object
  conf$Content = tolower(conf$Content)  ## ignore case!
  wAnno = factor(rep(NA, nrWpP*nrPlate), levels=unique(conf$Content))
  xraw <- rawdata(object)
  for(p in seq(along=object@batch)) {
    wa = conf$Content[ conf$Batch==object@batch[p] ]
    wAnno[(1:nrWpP)+nrWpP*(p-1)] = wa
    xraw[wa=="empty", p,,] <- NA
  }

  object@wellAnno <- wAnno


  ## Process screenlog
  if (!is.null(slog)) {
## To avoid problems with different case in filename extensions
    dfiles = sapply(object@plateList$Filename, function(z) { 
          a = unlist(strsplit(z, ".", fixed=TRUE))
          a = a[-length(a)] } )

    lfiles = sapply(slog$Filename, function(z) { 
          a = unlist(strsplit(z, ".", fixed=TRUE))
          a = a[-length(a)] } )

    mt = match(lfiles, dfiles)
    #mt = match(tolower(slog$Filename), tolower(object@plateList$Filename))

    if(any(is.na(mt)))
      stop(paste("'Filename' column in the screen log file '", logFile, "' contains invalid entries\n",
                 "(i.e. files that were not listed in the plateList file):\n",
                 paste(slog$Filename[is.na(mt)], collapse=", "), "\n", sep=""))
    ipl  = object@plateList$Plate[mt]
    irep = object@plateList$Replicate[mt]
    ich  = object@plateList$Channel[mt]
    ipos = pos2i(slog$Well, object@pdim)
    stopifnot(!any(is.na(ipl)), !any(is.na(irep)), !any(is.na(ich)))
    xraw[cbind(ipos, ipl, irep, ich)] = NA 
  } 

  rawdata(object) <- xraw
  object@state["configured"] = TRUE
  validObject(object)
  return(object)
})

##----------------------------------------
## export data to file as .txt
##----------------------------------------
setMethod("writeTab", signature("cellHTS"), 
   function(object, file=paste(object@name, "txt", sep="."), ...) {
  checkDots(...)
  
  toMatrix = function(y, prefix) {
    m = matrix(y, nrow=prod(dim(y)[1:2]), ncol=dim(y)[3:4])
    colnames(m) = sprintf("%sr%dc%d", prefix, rep(1:dim(y)[3], dim(y)[4]), rep(1:dim(y)[4], each=dim(y)[3]))	
    return(m)
  }

   out = cbind(geneAnno(object), toMatrix(rawdata(object), "R"))
   if(state(object)["normalized"])		    
     out = cbind(out, toMatrix(normdata(object), "N")) 
  
  write.table(out, file=file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
  return(file)
})


##-------------------------------------------
## Generate data to plot a ROC curve from the scored data
##-------------------------------------------

setMethod("ROC", signature("cellHTS"), 

   function(object, positives, negatives){
##'positives' and 'negatives' is a vector of characters specifying the name of the controls
    if(!state(object)["scored"])
    stop("Please score 'object' using the function 'summarizeReplicates') before trying to calculate ROC.")

 # default
 assayType = "one-way assay"
 score = scores(object)

  if (!missing(positives)) {
## checks
      if(!is(positives, "list")){ 
        checkControls(positives, len=1, name="positives")
      }else{

        checkControls2W(positives, len=1, name="positives")
        positives = paste(positives, collapse="|")
        score = abs(scores(object)) # because this is a two way assay
        assayType = "two-way assay"
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


  wAnno = as.character(wellAnno(object))

  xpos = regexpr(positives, wAnno, perl=TRUE)>0
  xneg = regexpr(negatives, wAnno, perl=TRUE)>0

  if(!any(xneg))
    stop("Negative controls not found")
    #stop(sprintf("The 'wellAnno' slot does not contain any entries with value '%s'.", negatives))
  if(!any(xpos))
    stop("Positive controls not found")
    #stop(sprintf("The 'wellAnno' slot does not contain any entries with value '%s'.", positives))

  br = unique(quantile(score, probs=seq(0, 1, length=1001), na.rm=TRUE))
  ct  = cut(score, breaks=br)
  spNeg = split(xneg, ct)
  spPos = split(xpos, ct)
  nNeg = sapply(spNeg, sum)
  nPos = sapply(spPos, sum)
  stopifnot(all(names(nPos)==names(nNeg)))

  posNames = unique(wAnno[xpos])
  posNames = object@plateConf$Content[match(posNames, tolower(object@plateConf$Content))]
  negNames = unique(wAnno[xneg])
  negNames = object@plateConf$Content[match(negNames, tolower(object@plateConf$Content))]

  x = new("ROC", 
        name=object@name,
        assayType = assayType,
        TP = cumsum(rev(nPos)),
        FP = cumsum(rev(nNeg)),
           #positives = positives,
           #negatives = negatives, 
        posNames = posNames,
        negNames = negNames)

  return(x) 
})


##----------------------------------------
## Data accessors and replacement functions
##----------------------------------------
setMethod("scores", signature(object="cellHTS"),
          function(object) slot(object, "score"))


setReplaceMethod("scores",
                 signature=signature(
                   object="cellHTS",
                   value="numeric"),
                 function(object, value) {
                     object@score <- value
                     validObject(object)
                     return(object)
                 })


setMethod("description", signature(object="cellHTS"),
          function(object) {
             print(slot(object, "screenDesc"), quote=FALSE)
          })

setMethod("name", signature(object="cellHTS"),
          function(object) {
             slot(object, "name")
          })

setReplaceMethod("name",
                 signature=signature(
                   object="cellHTS",
                   value="character"),
                 function(object, value) {
                     object@name <- value
                     validObject(object)
                     return(object)
                 })

setMethod("state", signature(object="cellHTS"),
          function(object) {
             slot(object, "state")
          })

setMethod("rawdata", signature(object="cellHTS"),
          function(object){
             slot(object, "xraw")
})


setReplaceMethod("rawdata",
                 signature=signature(
                   object="cellHTS",
                   value="array"),
                 function(object, value) {
                     object@xraw <- value
                     validObject(object)
                     return(object)
                 })

setMethod("normdata", signature(object="cellHTS"),
          function(object){
             slot(object, "xnorm")
})


setReplaceMethod("normdata",
                 signature=signature(
                   object="cellHTS",
                   value="array"),
                 function(object, value) {
                     object@xnorm <- value
                     validObject(object)
                     return(object)
                 })


setMethod("plateEffects", signature(object="cellHTS"),
          function(object){
             list(rowcol=slot(object, "rowcol.effects"),
                 overall = slot(object, "overall.effects")) 
})


setMethod("wellAnno", signature(object="cellHTS"),
          function(object){
             slot(object, "wellAnno")
})


setMethod("geneAnno", signature(object="cellHTS"),
          function(object){
             slot(object, "geneAnno")
})




