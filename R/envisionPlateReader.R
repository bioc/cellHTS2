# Data import function compatible with the EnVision plate reader file format.
# Ligia Bras (August 2007)
getEnVisionCrosstalkCorrectedData <- function(f, p)
{
    out <- do.call(envisionPlateReader, list(f, TRUE))
    return(out)
}



getEnVisionRawData <- function(f, p)
{
    out <- do.call(envisionPlateReader, list(f, FALSE))
    return(out)
}



envisionPlateReader <- function(f, crosstalk=FALSE)
{
    txt <- readLines(f)
    sp <- strsplit(txt, split=",")
    meta <- list(nrRows = grep("^Number of rows", sp),
                 nrColumns = grep("^Number of columns", txt),
                 rotated = grep("^Rotated plate", txt)
                 )
    if(length(unique(listLen(meta)))!=1L)
        stop("Could not find all expected metadata fields.\n")
    meta <- lapply(meta, function(i)
               {
                   a <- unlist(strsplit(sp[[i]], split=" "))
                   a[length(a)]
               })
    meta[1:2] <- lapply(meta[1:2], as.numeric)
    
    ## currently, only implemented for the case where "Rotated plate"=="No".
    stopifnot(meta$rotated=="No")
    
    ## use crosstalk data or background data?
    pos <- grep("esults", sp)
    
    if(crosstalk)
    {
        plateStart <- pos[grep("rosstalk", sp[pos])] + 1
        plateEnd <- plateStart + meta$nrRows 
    }
    else
    {
        plateStart <- pos[-grep("rosstalk", sp[pos])] + 1
        plateEnd <- plateStart + meta$nrRows 
    }
    
    ## first line should correspond to column identifiers:
    stopifnot(as.numeric(sp[[plateStart]])[-1]==1:meta$nrColumns)
    dat <- sp[(plateStart+1) : plateEnd]
    stopifnot(length(unique(lapply(dat, length)))==1L)
    
    ## first column should correspond to the row identifiers:
    stopifnot(sapply(dat, "[", 1) ==LETTERS[1:meta$nrRows])
    ## put as a row-wise vector
    dat <- as.vector(as.numeric(sapply(dat, "[", -1)))
    nums <- rep(1:meta$nrColumns, meta$nrRows)
    nums <- sapply(nums, function(i) if(i<10) paste(0, i, sep="") else i) 
    lets <- rep(LETTERS[1:meta$nrRows], each=meta$nrColumns)
    wells <- paste(lets, nums, sep="")
    
    out <- data.frame(well=I(wells), val=dat)
    info <- cbind(basename(f), out)
    tfile <- tempfile(pattern = "file", tmpdir = tempdir())
    write.table(info, tfile, col.names=FALSE, row.names=FALSE, sep="\t", quote=FALSE)
    info <- readLines(tfile)
    unlink(tfile)
    out <- list(out, info) 
    return(out)
}
