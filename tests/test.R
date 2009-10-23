## cat tests/test.R | R --vanilla
## cellHTS2 crash test on various conditions
library(cellHTS2)
path <- system.file("testscreen", package="cellHTS2")

testPlatelist=function(platelist, normalize=TRUE)
{
    x <- readPlateList(platelist, name="test", path=path)
    x <- configure(x, descripFile="description.txt", confFile="plateconf.txt",
                   logFile="screenlog.txt", path=path)
    
    if (normalize)
    {
        ## normalize results
        xn <- normalizePlates(x, scale="multiplicative", log=FALSE, method="median",
                              varianceAdjust="none")
        
        ## score and summarize replicates
        xsc <- scoreReplicates(xn, sign="-", method="zscore")
        xsc <- summarizeReplicates(xsc, summary="mean")
    }
    
    ## write reports
    outdir <- file.path(tempdir(),platelist,'raw')
    mainScriptFile <-  system.file("scripts/dummy.R", package="cellHTS2")
    writeReport(raw=x, force=TRUE, plotPlateArgs = TRUE,imageScreenArgs = list(zrange=c( -4, 8), ar=1),
                map=TRUE, outdir=outdir, mainScriptFile=mainScriptFile)
    if (interactive()) browseURL(file.path(outdir,'index.html'))
    if (normalize)
    {
        outdir <- file.path(tempdir(),platelist,'norm')
        writeReport(raw=x, normalized=xn, scored=xsc, force=TRUE, plotPlateArgs = TRUE,
                    imageScreenArgs = list(zrange=c( -4, 8), ar=1), map=TRUE, outdir=outdir,
                    mainScriptFile=mainScriptFile)
        if (interactive()) browseURL(file.path(outdir,'index.html'))
    }
}

######
## 2 plates, 2 replicates, 1 channel
testPlatelist('platelist221.txt')

######
## 2 plates, 1 replicate, 2 channels
testPlatelist('platelist212.txt', normalize=FALSE)

######
## 2 plates, 1 replicate, 3 channels
testPlatelist('platelist213.txt', normalize=FALSE)

######
## 2 plates, 2 replicates, 2 channels
testPlatelist('platelist222.txt', normalize=FALSE)

######
## 2 plates, 1 replicates, 1 channel
testPlatelist('platelist211.txt')

######
## 2 plates, 3 replicates, 3 channels
testPlatelist('platelist233.txt', normalize=FALSE)
