## Add the screen plot with plate configuration in the html report
## (only based on the content of plate configuration file! No updates based on screen log file.)
writePlateConf = function(outdir, con, xr, nrPlate, posControls, negControls) {		
	if(state(xr)[["configured"]]) {
		
		## Create a data.frame for the screen plot with plate configuration
		res <- makePlot(outdir, con=con, name="configurationAsScreenPlot", w=7, h=7*pdim(xr)["nrow"]/pdim(xr)["ncol"]*ceiling(nrPlate/6)/6+0.5, psz=8,
				fun = function() {
					do.call("configurationAsScreenPlot", 
							args=list(x=xr, verbose=FALSE, posControls=unlist(posControls), negControls=negControls))
				},
				print=FALSE, isImageScreen=FALSE)
		
		## do plot with the legend	
		makePlot(outdir, con=con, name="colLeg", w=5, h=2, psz=8, fun=function(){ image(matrix(1:length(res)), axes=FALSE, col=res, add = !TRUE, ylab="", xlab="Color scale"); axis(1, at = seq(0,1,length=length(res)), tick = !TRUE, labels=names(res)) }, print=FALSE, isImageScreen=FALSE)
		confTable = data.frame(matrix(data = NA, nrow = 2, ncol = 1))
		names(confTable) = "Plate configuration"		
		confTable[1,1] <- hwrite(hwriteImage("colLeg.png", image.border=0), center = TRUE, br= TRUE)    
		confTable[2,1] <- hwrite(hwriteImage("configurationAsScreenPlot.png", image.border=2), link = "configurationAsScreenPlot.pdf", center = TRUE, br=TRUE)
		writeHTMLtable4plots(confTable, con=con) 
	}
}


##  Write the Per experiment QC plots in the html report
writePerExperimentReportQC = function(xr, xn, outdir, con, allControls, allZfac) {	
	plotTable = QMexperiment(xr, xn, outdir, con, allControls, allZfac)		
	writeHTMLtable4plots(plotTable, con=con)
}


## write imageScreenImage in the HTML report if overallState[["scored"]] == TRUE 
writeImageScreenTable = function(outdir, con, cellHTSlist, imageScreenArgs, overallState, progressReport, timeCounter, totalTime, steps2Do, nsteps, timePerStep, nrPlate) {
	if(overallState[["scored"]]) {  
		if(overallState["annotated"]) ttInfo = "Table of scored <br/> and annotated probes" else ttInfo = "Table of scored probes"
		
		##   Step 6 :  topTable
		out <- getTopTable(cellHTSlist, file=file.path(outdir, "topTable.txt"), verbose=FALSE)
		timeCounter = myUpdateProgress(progressReport,timeCounter, totalTime, match("step6", steps2Do)+1, nsteps, timePerStep["step6"])			
					
		##  Step 7 : Screen-wide image plot 	
		xsc = cellHTSlist$scored
		res <- makePlot(outdir, con=con, name="imageScreen", w=7, h=7, psz=8,fun = function(map=imageScreenArgs$map) do.call("imageScreen", args=append(list(object=xsc, map=map), imageScreenArgs[!names(imageScreenArgs) %in% "map"])),print=FALSE, isImageScreen=TRUE)
		screenWideImageMatrix=matrix("", nrow=2, ncol=2)
		screenWideImageMatrix[1, 2] = hwrite("Screen-wide image plot of the scored values")	
		screenWideImageMatrix[2, 1] = hwrite(ttInfo, link = "topTable.txt", br = TRUE) 
		if (is.null(res)) {
			screenWideImageMatrix[2, 2] = hwrite(hwriteImage("imageScreen.png", image.border=2),link = "imageScreen.pdf",  center = TRUE) 
		} else {			
			res <- myPlateMap(object=res$obj, tags=res$tag, "imageScreen.png", cellHTSlist = cellHTSlist, imageScreenArgs=imageScreenArgs)				
			screenWideImageMatrix[2, 2] = paste(hwrite("", br=TRUE), hwrite(res, center = TRUE, br = TRUE), hwrite("enlarged version", link = "imageScreen.pdf", center = TRUE, br = TRUE))
		}
		writeHTMLtable4plots(screenWideImageMatrix, con=con)
	} 	
}


## this function is used to split the Screen-wide image plot of the scored values into several areas so that clicking on a plate will lead to the quality report of the plate.
myPlateMap <- function(object, tags, imgname,cellHTSlist = cellHTSlist, imageScreenArgs=imageScreenArgs) {			
	
	## imageScreen configuration, same as in file imagescreen.R
	xsc = cellHTSlist$scored	
	nr = pdim(xsc)[1] ## number of rows for the plate
	nc = pdim(xsc)[2] ## number of columns for the plate
	ar = imageScreenArgs$ar	## 'ar' is the aspect ratio for the image plot (i.e. number columns divided by the number of rows)
	nrPlates = getNrPlateColRow(ar, xsc)$nrPlates ## number of plates
	nrRow = getNrPlateColRow(ar, xsc)$nrRow ## number of plates per row in imageScreen.png
	nrCol = getNrPlateColRow(ar, xsc)$nrCol	## number of plates per column in imageScreen.png
	
	## beginning of the html code
	mapname <- paste("map", gsub(" |/|#", "_", imgname), sep="_") 	
	outin <- paste("<img src=\"", imgname, "\" usemap=\"#", mapname, "\" border=\"2\" alt=\"\"/>\n", "<map name=\"", mapname, "\" id=\"",mapname," \">\n", sep="")   
	
	## links to the plate report	
	plateCounter = 1
	remainingPlates = nrPlates
	out=''	
	for(i in (1:nrRow)) {
		## initialization; useful for the lat row which may contain less than nrCol plates
		tempCol = nrCol
		if(remainingPlates<nrCol) {tempCol=remainingPlates}				
		## coords
		xi=object[(0:(tempCol-1))*nc+1,1]
		xf=object[(0:(tempCol-1))*nc+(nc-1),3]		
		yi= rep(object[1+(i-1)*nc*nrCol*nr,2], tempCol)
		yf= rep(object[1+(i-1)*nc*nrCol*nr+nc*tempCol*(nr-1),4], tempCol)
		toAdd=matrix(c(xi,yi,xf,yf),ncol=4)				
		for(j in 1:tempCol) {
			newLine = paste(paste("<area shape=\"rect\" coords=\"", paste(toAdd[j,], collapse=","),"\"", sep=""), paste(" ", paste(names(tags), "=\"",c(paste('Plate', plateCounter,sep=' '), paste(plateCounter, 'index.html', sep='/')),"\"", sep=""), collapse=" "), " alt = \"\"/>\n", sep="")
			out=paste(out, newLine)	
			plateCounter = plateCounter+1
		}			
		remainingPlates=remainingPlates-tempCol			
	}
	
	## end of the html code
	out <- paste(outin, out, "</map>",sep="") 	
} 


## this function writes the main index.html file 
writeMainIndexHtml = function(outdir, indexFile, url, cellHTSlist, xr, xn, overallState, nm, exptab, imageScreenArgs, nrChannel, nrReplicate, progressReport, timeCounter, timePerStep, totalTime, nsteps, steps2Do, nrPlate, posControls, negControls, allControls, allZfac){
	
	## opening the file, calling wz_tooltip.js for the mouseover bubbles javascript, and writing the title (in the body of the html document))
	con = openPage(indexFile, title = paste("Experiment report for ", name(xr)))
	hwrite('<script type="text/javascript" src="wz_tooltip.js"></script>', con)	
	if(overallState["configured"]) {hwrite(paste("Experiment report for ", hwrite(name(xr), link=nm),'\n'),con, center = TRUE, br= TRUE, heading=1)} 
	else {hwrite(paste("Experiment report for", name(xr)),con, center = TRUE, br= TRUE, heading = 1)}		
	hwrite("",con, br=TRUE)
	
	## write table with overall CQ results 
	wh = which(plateList(xr)$status=="OK")
	nm = file.path("in", names(intensityFiles(xr)))
	for(w in wh) {url[w, "Filename"] = nm[w]}
	writeHTMLtable(exptab, url=url, con=con, center = TRUE, glossary=createGlossary())
	timeCounter = myUpdateProgress(progressReport, timeCounter, totalTime, match("step4", steps2Do)+1, nsteps, 4*nrChannel*nrReplicate)
		
	## Step 5 : per experiment QC	
	writePlateConf(outdir, con, xr, nrPlate, posControls, negControls) ## Plate configuration	
	writePerExperimentReportQC(xr, xn, outdir, con, allControls, allZfac) ## per experiment report qc
	timeCounter = myUpdateProgress(progressReport, timeCounter, totalTime, match("step5", steps2Do)+1, nsteps, timePerStep["step5"])
		
	## steps 6 and 7 : if overallState[["scored"]] == TRUE then write the "Screen-wide image plot of the scored values" plot in the html report
	writeImageScreenTable(outdir, con, cellHTSlist, imageScreenArgs, overallState, progressReport, timeCounter, totalTime, steps2Do, nsteps, timePerStep, nrPlate)
	timeCounter = myUpdateProgress(progressReport, timeCounter, totalTime, match("step5", steps2Do)+1, nsteps, timePerStep["step6"]+timePerStep["step7"])
	
	## End : close page
	closePage(con)
}