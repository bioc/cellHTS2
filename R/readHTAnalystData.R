readHTAnalystData = function(filenames, path=dirname(filenames), name, 
       nrPlates, verbose=interactive()) {

## NB-
## filenames can be a vector of characters giving the names of the files to read. Each file is expected to contain the same number of plates.Each file is expected to contain data from each set of plate replicates.

## path - can be either a character giving the path for the above file(s), or a vector of characters with the same length as the number of files to read

  nrRep <- length(filenames)

  if(!is.numeric(nrPlates) | length(nrPlates)!=1L) 
    stop(sprintf("'nrPlates' should be an integer value indicating the number of plates in the input file%s!", ifelse(nrRep>1,"s","")))

  if(!(is.character(path) && length(path) %in% c(1, nrRep)))
    stop("'path' must be character of length 1 or with the same length as 'filenames'")

  if(length(path)!=nrRep) path = rep(path, nrRep)  

  dfiles <- lapply(path, dir) 
  names(dfiles) <- basename(filenames)

  ## check wether the data files exist:
  ff <- sapply(1:length(dfiles), function(i) grep(names(dfiles)[i], dfiles[[i]], ignore.case=TRUE))

  fl <- sapply(ff, length)
  if (any(fl!=1)) stop(paste("File not found:", filenames[which(fl!=1)], collapse="\n"))

  ## read data files:
  f = file.path(path, dfiles[ff])


  ll <- lapply(1:length(dfiles), function(i) 
              readHTAnalystOneReplicate(file.path(path[i], dfiles[[i]][ff[i]]), nrPlates=nrPlates, verbose=verbose))
  ##check plate dimensions
  ncolPlate <- unique(sapply(ll, function(i) i[["plateDim"]]["ncol"]))
  nrowPlate <- unique(sapply(ll, function(i) i[["plateDim"]]["nrow"]))
  if(length(ncolPlate)!=1L | length(nrowPlate)!=1L) stop("Found different plate formats across the input files!\n")

  dimPlate <- c("nrow"=as.integer(nrowPlate), "ncol" = as.integer(ncolPlate))
  nrWell <- prod(dimPlate)
  if(verbose)
    cat(sprintf("Found data in %d x %d (%d well) format.\n", dimPlate[1], dimPlate[2], nrWell))

  ## this assumes we only have data for one channel!
  nrChannel <- 1L
  xraw = array(as.numeric(NA), dim=c(prod(nrWell, nrPlates), length(ll), nrChannel)) # array of nrFeatures (nrWells x nrPlates) x nrReplicates x 1)
  for(i in seq(along=ll)) 
    xraw[,i,1L] = ll[[i]]$xraw
    #dimnames(xraw)=list(well=NULL,plate=NULL,screen=basename(filenames),channel=NULL)
    intfl = lapply(ll, "[[", "intensityFiles")
    Plate = as.integer(unlist(lapply(intfl, names)))
    stopifnot(!any(is.na(Plate)|Plate<1|Plate>nrPlates))
    Replicate = rep(seq(along=intfl), times=listLen(intfl))


  for(i in seq(along=intfl))
    names(intfl[[i]]) = paste(names(dfiles)[i], names(intfl[[i]]), sep="-plate")

  uli = do.call(c, intfl)
  stopifnot(length(Replicate)==length(uli))


#####################################################################################
# ----  Store the data as a "cellHTS" object ----
# arrange the assayData slot:
adata <- assayDataNew(storage.mode="environment")
chNames <- paste("ch", 1:nrChannel, sep="")

for(ch in 1:nrChannel) 
    assign(chNames[ch], matrix(xraw[,,ch, drop=TRUE], ncol=nrRep, nrow=nrWell*nrPlates), envir=adata)

#stopifnot(ls(adata)==chNames)
storageMode(adata) <- "lockedEnvironment"

## arrange the phenoData slot:
pdata = new("cellHTS")@phenoData
pData(pdata) <- data.frame(replicate=1:nrRep, assay=I(rep(name, nrRep)))
varMetadata(pdata)[["channel"]] = factor(rep("_ALL_",2), levels=c(chNames, "_ALL_"))

# pdata <- new("AnnotatedDataFrame", 
#                   data=data.frame(replicate=1:nrRep, assay=I(rep(name, nrRep))),
#                   varMetadata=data.frame(labelDescription=I(c("Replicate number", "Biological assay")), 
#                                          channel=factor(rep("_ALL_",2), levels=c(chNames, "_ALL_"))))



## arrange the featureData slot:
well <- convertWellCoordinates(as.integer(1:nrWell), pdim=dimPlate)$letnum
fdata <- new("cellHTS")@featureData
pData(fdata) <- data.frame(plate=rep(1:nrPlates, each=nrWell), well=I(rep(well,nrPlates)), 
                           controlStatus=factor(rep("unknown", nrWell*nrPlates)))

# 
# fdata <- new("AnnotatedDataFrame", 
#            data=data.frame(plate=rep(1:nrPlate, each=nrWell), well=I(rep(well,nrPlate)), 
#                            controlStatus=factor(rep("unknown", nrWell*nrPlate))), 
#            varMetadata=data.frame(labelDescription=I(c("Plate number", "Well ID", "Well annotation"))))


 res = new("cellHTS", 
   assayData=adata,
   phenoData=pdata,
   featureData=fdata,
   plateList=data.frame(Filename  = I(names(uli)),
                           Plate     = Plate,
                           Replicate = Replicate,
                           Channel   = rep(1L, length(uli)),
                           Status    = I(rep("OK", length(uli)))),
   intensityFiles=uli
   #state=c("configured"=FALSE, "normalized"=FALSE, "scored"=FALSE, "annotated" = FALSE)
   )

  return(res)
}



##=================================================================================
## Auxiliary function to read the input data file from each set of plate replicates
##=================================================================================
readHTAnalystOneReplicate <- function(filename, nrPlates, verbose=verbose) {

if(verbose) cat("Reading file", basename(filename), "\n")
  
  x = readLines(filename)

  meta = list(barcode = grep("^Barcode:", x),
              data  =   grep("^Data:", x),
              units =   grep("^Units:", x),
              dispf =   grep("^Display format:", x),
              method=   grep("^Method ID:", x),
              micrf =   grep("^Microplate format:", x))
  
  if(length(unique(listLen(meta)))!=1L |(any(listLen(meta)==0)) )
    stop("Could not find all expected metadata fields.\n")
  
  getField = function(z) gsub("\t", "", sapply(strsplit(z, split=":"), "[", 2L))

# see if the contents of these meta fields are unique
  stopifnot(length(unique(getField(x[meta$data])))==1L) #  == "RAW DATA"))
  stopifnot(length(unique(getField(x[meta$dispf])))==1L) # == "%.2f"))
  stopifnot(length(unique(getField(x[meta$micrf])))==1L) #== "Corning 384 Square Opaque PS"))
  meth = unique(getField(x[meta$method]))
  if(length(meth)!=1L)  #if(!all(meth=="CellTiter"))
    message(sprintf("*** Found different methods! *** \n%s ", paste("\t", meth, collapse="\n")))
  units = unique(getField(x[meta$units]))
  if(length(units)!=1L) #rlu
    message(sprintf("*** Found different units: *** \n%s", paste("\t", units, collapse="\n")))

  #bc = as.integer(sapply(strsplit(getField(x[meta$barcode]), split="[XT]"), "[", 2L))
                   ##more versatile to different barcodes:
  bc = as.integer(sapply(strsplit(getField(x[meta$barcode]), split=sprintf("[%s]", paste(LETTERS, collapse=""))), "[", 2L))

  whichunexpected <- which(is.na(bc)|(bc<1L)|(bc>nrPlates))
  if(length(whichunexpected)>0)
    message(sprintf("*** Unexpected barcodes: *** \n%s",
                    paste("\t", bc[whichunexpected], collapse="\n")))
  whichmissing = which(!(seq_len(nrPlates) %in% bc))
  if(length(whichmissing)>0)
    message(sprintf("*** Missing barcodes: %s ***",
                    paste(whichmissing, collapse=", ")))
  whichdup = which(duplicated(bc))
  if(length(whichdup)>0)
    stop(sprintf("Duplicated barcode: %s ***",
                  paste(whichdup, collapse=", ")))

  ## determine the limits of each plate:
  ## We expect that the next line after "Display format" contains the column numbers 1:ncol
  datastart = meta$dispf+1L
  sp = strsplit(x[datastart], split="\t")
  ## We expect that the previous line before "Load Time" (or the first field of the file) contains the last row of data of each plate
  dstop <- sapply(strsplit(x[1], split=":"), "[", 1L) # first line of the input file
  datastop <-  grep(dstop, x)
  if(length(datastop)!=nrPlates) stop(sprintf("Field '%s' not found for all of the '%d' plates!", dstop, nrPlates))
  
# find end of file
lf = length(x)
while(x[lf]=="") {
 x <- x[-lf]
 lf <- length(x)
}

 datastop <- c(datastop[-1]-1, lf)
 # Automatically determines the plate dimensions:
 ncolPlate <- unique(sapply(sp, length)-1)
 if(length(ncolPlate)!=1L) stop(sprintf("Found different plate formats in file %s!\n",
                   basename(filename)))

nrowPlate <- unique(datastop-datastart)
if(length(nrowPlate)!=1L) stop(sprintf("Found different plate formats in file %s!\n",
                   basename(filename)))

  xraw <- array(as.numeric(NA), dim=c(nrowPlate*ncolPlate, nrPlates))
  for(i in seq(along=bc)) {
    sp = strsplit(x[(datastart[i] + 1):datastop[i]], split="\t")
    let <- sapply(sp, "[", 1L)
    notEq <- which(let!=LETTERS[1:nrowPlate])
    if(length(notEq)) stop(sprintf("Format of file %s differs from what is expected (name of row %d of plate %d).\n", basename(filename), notEq, i))

    for(r in seq(along=sp)){
#       if(!identical(sp[[r]][1L], LETTERS[r])) 
#          stop(sprintf("Format of file %s differs from what is expected (name of row %d of plate %d).\n",
#                       filename, r, i))
      nums = as.numeric(sp[[r]][-1L])
      if(any(is.na(nums)))
        stop(sprintf("Non-numeric data in file %s (plate %d).\n",
                     basename(filename), i))
      ## note that plates can come in different order than 1, 2, 3, .. hence
      ##  we explicitely use the plate number (barcode) in 'bc'
      xraw[ncolPlate*(r-1L)+seq_len(ncolPlate), bc[i]] = nums
    }
  }
  
  intensityFiles = vector(mode="list", length=length(bc))
  names(intensityFiles) = sprintf("%03d", bc)
  for(i in seq(along=intensityFiles))
    intensityFiles[[i]] = x[datastart[i]+(0:nrowPlate)]
  
  return(list(xraw=xraw, intensityFiles=intensityFiles, plateDim=c("nrow"=nrowPlate, "ncol"=ncolPlate)))
}
