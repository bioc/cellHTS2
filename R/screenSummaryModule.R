## The workhorse function for the 'Screen Summary' module: an image plot of the results
## for the whole screen, possibly with an underlying HTML imageMap to allow for drill-down
## to the quality report page of the respective plates. Also a Normal Q-Q plot of the scores
## data.
writeHtml.screenSummary <- function(cellHTSList, module, overallState,
                                  nrPlate, con)
{
    outdir <- dirname(module@url)
    if(overallState[["scored"]])
    {
        imgList <- list()
        ttInfo <- 
            if(overallState["annotated"]) "Table of scored <br/> and annotated probes" else
        "Table of scored probes"
        xsc <- cellHTSList$scored
        settings <- chtsGetSetting(c("screenSummary", "scores"))
        res <- makePlot(outdir, name="imageScreen", w=settings$size, h=settings$size,
                        font=settings$font, thumbFactor=settings$thumbFactor,
                        psz=settings$fontSize, thumbPsz=settings$thumbFontSize,
                        pngArgs=list(map=settings$map), ar=settings$aspect,
                        zrange=settings$range, anno=settings$anno, col=settings$col,
                        nbImageBins=settings$nbImageBins, nbLegendBins=settings$nbLegendBins, 
                        fun=function(x=xsc, map=FALSE, ar, zrange, anno, col,
                          nbImageBins, nbLegendBins, ...)
                        {
                          imageScreen(object=x, map=map, ar=ar, col=col,
                                      nbImageBins=nbImageBins, nbLegendBins=nbLegendBins, ...)
                        })
        imgList[["Scores"]] <-
            chtsImage(data.frame(title="Screen-wide image plot of the scored values",
                                 thumbnail="imageScreen.png", 
                                 fullImage="imageScreen.pdf",
                                 map=if(!is.null(res)) screenImageMap(object=res$obj,
                                 tags=res$tag, "imageScreen.png",
                                 cellHTSlist=cellHTSList)
                                 else NA))
        
        settings <- chtsGetSetting(c("screenSummary", "qqplot"))
        wcols <- chtsGetSetting("controls")$col
        types <- setdiff(levels(wellAnno(xsc)), "sample")
        freqs <- table(wellAnno(xsc))
        addCode <- sprintf("<div class=\"scatterLegend\">%s</div>",
                           paste(sprintf("<font color=\"%s\">%s: %d</font>", wcols[names(freqs)],
                                         names(freqs), freqs), collapse="&nbsp&nbsp&nbsp"))
        qqn <- makePlot(outdir, name="qqplot", w=settings$size, h=settings$size,
                        font=settings$font, thumbFactor=settings$thumbFactor,
                        psz=settings$fontSize, thumbPsz=settings$thumbFontSize,
                        pdfArgs=list(main="Normal Q-Q Plot"),
                        fun=function(x=xsc, main=NULL, ...)
                    {
                        par(mai=c(0.8,0.8,0.2,0.2))
                        vals <- Data(x)
                        ann <- wellAnno(x)
                        srt <- order(ann)
                        qqnorm(Data(x)[srt], main=main, cex.lab=1.3,
                               ylim=range(Data(x), na.rm=TRUE, finite=TRUE),
                               col=wcols[as.character(ann[srt])], ...)
                        qqline(Data(x), col="darkred", lty=3, lwd=2)
                    })
        imgList[["Q-Q Plot"]] <- chtsImage(data.frame(title="Normal Q-Q Plot",
                                                      thumbnail="qqplot.png",
                                                      fullImage="qqplot.pdf",
                                                      additionalCode=addCode))
       
        settings <- chtsGetSetting(c("screenSummary", "distribution"))
        dens <- makePlot(outdir, name="density",
                         w=settings$size, h=settings$size,
                         font=settings$font, thumbFactor=settings$thumbFactor,
                         psz=settings$fontSize, thumbPsz=settings$thumbFontSize,
                         pdfArgs=list(main="Density distribution"),
                         fun=function(x=xsc, main="", ...)
                     {
                         par(mai=c(0.8,0.8,0.2,0.2))
                         plot(density(Data(x), na.rm=TRUE), main=main, cex.lab=1.3)
                         for(t in types)
                         {
                             xval <- Data(x)[wellAnno(x) == t]
                             points(xval, rep(par("usr")[3]/2, length(xval)), col=wcols[t],
                                    pch=20)
                         }
                                    
                     },
                         print=FALSE)
        imgList[["Distribution"]] <- chtsImage(data.frame(title="Density Distribution",
                                                          thumbnail="density.png",
                                                          fullImage="density.pdf",
                                                          additionalCode=addCode))
        
        stack <- chtsImageStack(list(imgList), id="imageScreen",
                                tooltips=addTooltip(names(imgList)))        
        writeHtml.header(con)
        writeHtml(stack, con)
        writeHtml.trailer(con)
        return(NULL)
    }
    else
    {
        return(NA)
    }
}



## This function is used to split the Screen-wide image plot of the
## scored values into rectangle areas for a HTML imageMap in order
## that clicking on a plate will link to its quality report.
screenImageMap <- function(object, tags, imgname, cellHTSlist=cellHTSlist)
{			
    ## imageScreen configuration, same as in file imagescreen.R
    xsc <- cellHTSlist$scored	
    nr <- pdim(xsc)[1] ## number of rows for the plate
    nc <- pdim(xsc)[2] ## number of columns for the plate
    ## 'ar' is the aspect ratio for the image plot
    ##(i.e. number columns divided by the number of rows)
    ar <- chtsGetSetting(c("screenSummary", "scores"))$aspect	
    nrPlates <- getNrPlateColRow(ar, xsc)$nrPlates ## number of plates
    nrRow <- getNrPlateColRow(ar, xsc)$nrRow ## number of plates per row in imageScreen.png
    nrCol <- getNrPlateColRow(ar, xsc)$nrCol ## number of plates per column in imageScreen.png
	
    ## beginning of the html code
    mapname <- paste("map", gsub(" |/|#", "_", imgname), sep="_") 	
    outin <- sprintf("usemap=\"#%s\"><map name=\"%s\" id=\"%s\">\n", mapname, mapname, mapname)
	
    ## links to the plate report	
    plateCounter <- 1
    remainingPlates <- nrPlates
    out <- ""	
    for(i in (1:nrRow))
    {
        ## initialization; useful for the last row which may contain less than nrCol plates
        tempCol <-  if(remainingPlates<nrCol) remainingPlates else nrCol
       				
        ## coords
        xi <- object[(0:(tempCol-1))*nc+1,1]
        xf <- object[(0:(tempCol-1))*nc+(nc-1),3]		
        yi <- rep(object[1+(i-1)*nc*nrCol*nr,2], tempCol)
        yf <- rep(object[1+(i-1)*nc*nrCol*nr+nc*tempCol*(nr-1),4], tempCol)
        toAdd <- matrix(c(xi,yi,xf,yf),ncol=4)				
        for(j in 1:tempCol)
        {
            newLine <- paste(paste("<area shape=\"rect\" coords=\"",
                                   paste(toAdd[j,], collapse=","),"\"", sep=""),
                             paste(" ", paste(names(tags), "=\"",
                                              c(paste('Plate', plateCounter,sep=' '),
                                                paste("..", plateCounter, 'index.html',
                                                      sep='/')),
                                              "\"", sep=""), collapse=" "), " alt=\"\"/>\n",
                             sep="")
            out <- paste(out, newLine)	
            plateCounter <- plateCounter+1
        }			
        remainingPlates <- remainingPlates-tempCol			
    }
    out <- paste(outin, out, "</map",sep="")
    return(out)
} 



