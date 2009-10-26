importData <- function(fname, ...)
{
    d <- count.fields(fname, sep = ",", quote = "",
                      skip = 0, blank.lines.skip = TRUE,
                      comment.char = "")
    nCol <- max(d)
    pp <- min(which(d==nCol))
    
    x <- scan(fname, what = as.list(rep("", nCol)), skip = pp-1,
              sep=",", quote="", fill = TRUE, strip.white = TRUE, quiet=TRUE)
    
    
    ## Matrix reproducing the plate
    x <- sapply(x[-1], function(y) as.numeric(y[-1]))
    
    nums <- rep(1:ncol(x), nrow(x))
    nums <- sapply(nums, function(i) if(i<10) paste(0, i, sep="") else i) 
    lets <- rep(LETTERS[1:nrow(x)], each=ncol(x))
    wells <- paste(lets, nums, sep="")
    ## put as a row-wise vector
    x <- as.vector(t(x))
    out <- data.frame(well=I(wells), val=as.numeric(x))
    info <- cbind(basename(fname), out)
    tfile <- tempfile(pattern = "file", tmpdir = tempdir())
    write.table(info, tfile, col.names=FALSE, row.names=FALSE, sep="\t", quote=FALSE)
    info <- readLines(tfile)
    unlink(tfile)
    out <- list(out, info) 
    return(out)
}
