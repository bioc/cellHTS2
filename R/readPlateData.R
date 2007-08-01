## (C) Michael Boutros and Wolfgang Huber, Nov 2005

readPlateData = function(filename, path=dirname(filename), name, importFun, verbose=interactive(), plateType)
{
  file = basename(filename)
  dfiles = dir(path)

  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")

  pd = read.table(file.path(path, file), sep="\t", header=TRUE, as.is=TRUE)

  checkColumns(pd, file, mandatory=c("Filename", "Plate", "Replicate"),
               numeric=c("Plate", "Replicate", "Channel", "Batch"))

  ## consistency check for "importFun"
  if (!missing(importFun)) {
    if (!is(importFun, "function")) stop("'importFun' should be a function to use to read the raw data files")
  } else {
    ## default function (compatible with the file format of the plate reader)
    importFun = function(f) {
      txt = readLines(f)
      sp  = strsplit(txt, "\t")
      well = sapply(sp, "[", 2)
      val  = sapply(sp, "[", 3)
      out = list(data.frame(well=I(well), val=as.numeric(val)),
        txt = I(txt))
      return(out)
    }
  }

  ## check if the data files are in the given directory
  a = unlist(sapply(pd$Filename, function(z) grep(z, dfiles, ignore.case=TRUE)))
  if (length(a)==0) stop(sprintf("None of the files were found in the given 'path': %s", path))
  
  f = file.path(path, dfiles[a])

  ## check if 'importFun' gives the output in the desired form
  aux = importFun(f[1])
  if (which(unlist(lapply(aux, is, "data.frame"))) != 1 | !all(c("val", "well") %in% names(aux[[1]])) | length(aux)!=2)
    stop("The output of 'importFun' must be a list with 2 components; the first component should be a 'data.frame' with slots 'well' and 'val'.")


  ## auto-determine the plate format
  well = as.character(importFun(f[1])[[1]]$well)
  let = substr(well, 1, 1)
  num = substr(well, 2, 3)
  let = match(let, LETTERS)
  num = as.integer(num)
  if(any(is.na(let))||any(is.na(num)))
    stop(sprintf("Malformated column 'well' in input file %s", f[1]))
  
  pdim = c(nrow=max(let), ncol=max(num))
  if(verbose)
    cat(sprintf("Found data in %d x %d (%d well) format.\n", pdim[1], pdim[2], prod(pdim)))

  ## Should we check whether these are true?
  ##     "96"  = c(nrow=8, ncol=12),
  ##     "384" = c(nrow=16, ncol=24),

  nrRep   = max(pd$Replicate)
  nrPlate = max(pd$Plate)

  combo = paste(pd$Plate, pd$Replicate)

  ## Channel: if not given, this implies that there is just one
  if("Channel" %in% colnames(pd)) {
    nrChannel = max(pd$Channel)
    channel = pd$Channel
    combo = paste(combo, pd$Channel)
  } else {
    nrChannel = 1
    channel = rep(as.integer(1), nrow(pd))
    pd$Channel = channel	
  }


  ## Batch: if not given, this implies that there is just one.  Currently, we
  ## can only deal with situations where all replicates and channels from one
  ## plate belong to the same batch. Test if this is the case and cast an
  ## error otherwise
  batch = rep(as.integer(1), nrPlate)
  if("Batch" %in% colnames(pd)) {
    sp = split(pd$Batch, pd$Plate)
    plate = as.numeric(names(sp))
    stopifnot(!any(is.na(plate)), setequal(plate, 1:nrPlate))
    for(j in seq(along=sp)) {
      b = as.integer(unique(sp[[j]]))
      if(length(b)!=1)
      stop(paste("Result files from plate", plate[j], "occur in multiple batches: ",
                 paste(b, collapse=", "), 
                 "\ncurrently this program does not know how to handle this."))
      batch[plate[j]] = b
    }
  } else {
    pd$Batch = rep(as.integer(1), nrow(pd))
    sp = split(pd$Batch, pd$Plate)
    plate = as.numeric(names(sp))
    stopifnot(setequal(plate, 1:nrPlate))
  }


  whDup = which(duplicated(combo))
  if(length(whDup)>0) {
    idx = whDup[1:min(5, length(whDup))]
    msg = paste("The following rows are duplicated in the plateList table:\n",
      "\tPlate Replicate Channel\n",
      paste(idx, combo[idx], sep="\t", collapse="\n"),
      if(length(whDup)>5) sprintf("\n...and %d more.\n", length(whDup)-5), "\n", sep="")
    stop(msg)
  }

  xraw = array(as.numeric(NA), dim=c(prod(pdim), nrPlate, nrRep, nrChannel))
  intensityFiles = vector(mode="list", length=nrow(pd))
  names(intensityFiles) = pd[, "Filename"]

  status = character(nrow(pd))

  for(i in 1:nrow(pd)) {
    if(verbose)
      cat(" Reading file ", i, ": ", pd[i, "Filename"], ";", sep="")

    ff = grep(pd[i, "Filename"], dfiles, ignore.case=TRUE)

    if (length(ff)!=1) {
      f = file.path(path, pd[i, "Filename"])
      status[i] = sprintf("File not found: %s", f)

    } else {
      f = file.path(path, dfiles[ff])
      names(intensityFiles)[i] = dfiles[ff]

      status[i] = tryCatch({
        out = importFun(f)
        pos = pos2i(out[[1]]$well, pdim)
        intensityFiles[[i]] = out[[2]]
        xraw[pos, pd$Plate[i], pd$Replicate[i], channel[i]] = out[[1]]$val
  	"OK"
      },
              warning = function(e) {
                paste(class(e)[1], e$message, sep=": ")
              },
              error = function(e) {
                paste(class(e)[1], e$message, sep=": ")
              }) ## tryCatch
    } ## else
  } ## for

  if(verbose)
    cat("\nDone.\n\n")

  res = new("cellHTS", 
    name = name, 
    xraw = xraw,
    pdim = pdim,
    batch = batch,
    plateList = cbind(pd[,1,drop=FALSE], status=I(status), pd[,-1,drop=FALSE]), 
    intensityFiles=intensityFiles,
    state = c("configured"=FALSE, "normalized"=FALSE, "scored"=FALSE, 
    "annotated" = FALSE)
)
  return(res)
}
