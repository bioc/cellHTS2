## The writeReport function creates the results folder. To see them, open "index.html" with a web browser


## this functions shows the update progress
## si : step i
## s max : step max
myUpdateProgress = function(progressReport, timeCounter, tmax, si, smax, additionalTime){
	if (progressReport) {
		timeCounter = timeCounter + additionalTime
		updateProgress(timeCounter/tmax*100, sub=sprintf("step %s of %s", si, smax), autoKill=TRUE)
		return(timeCounter)
	}
	else {
		return(0)
	}
}


## cellHTSlist verification
## stop if the verification fails
cellHTSlistVerification = function(cellHTSlist){	
	xr <- cellHTSlist[["raw"]]
	xn <- cellHTSlist[["normalized"]]
	xsc <- cellHTSlist[["scored"]] 
	## cellHTSlist verifications
	allowedListNames <- c("raw", "normalized", "scored")
	if(!is.list(cellHTSlist)) {
		stop("Argument 'cellHTSlist' should be a list containing one or a maximum of 3 'cellHTS' objects.")
	} else {
		if(!all(sapply(cellHTSlist, class)=="cellHTS")){
			stop("Argument 'cellHTSlist' should be a list of cellHTS objects!")
		}
		nm <- names(cellHTSlist)
		if(!("raw" %in% nm)) {
			stop("Argument 'cellHTSlist' should be a list containing at least one component named 'raw' that corresponds to a 'cellHTS' object containing unnormalized data.")
		}
		if(length(cellHTSlist)>3 | any(duplicated(nm))) {
			stop("Argument 'cellHTSlist' can only have a maximum of 3 components named 'raw', 'normalized' and 'scored'!")
		}
		if(!all(nm %in% allowedListNames)){
			stop(sprintf("Invalid named component%s in argument 'cellHTSlist': %s", ifelse(sum(!(nm %in% allowedListNames))>1, "s", ""), nm[!(nm %in% allowedListNames)]))
		}
	}	
	## now check whether the given components of 'cellHTSlist' are valid cellHTS objects:	
	if(any(state(xr)[c("scored", "normalized")])) stop(sprintf("The component 'raw' of argument 'cellHTSlist' should be a 'cellHTS' object containing unnormalized data!\nPlease check its preprocessing state: %s", paste(names(state(xr)), "=", state(xr), collapse=", ")))  
	if(!is.null(xn)) {
		if(!(state(xn)[["normalized"]] & !state(xn)[["scored"]])) {
			stop(sprintf("The component 'normalized' of 'cellHTSlist' should be a 'cellHTS' object containing normalized data!\nPlease check its preprocessing state: %s", paste(names(state(xn)), "=", state(xn), collapse=", ")))
		}
		if(!compare2cellHTS(xr, xn)) {
			stop("'cellHTS' objects contained in dat[['raw']] and dat[['normalized']] are not from the same experiment!")
		}
	}
	if(!is.null(xsc)) {
		if(!state(xsc)["scored"]) {
			stop(sprintf("The component 'scored' of argument 'cellHTSlist' should be a 'cellHTS' object containing scored data!\nPlease check its preprocessing state: %s", paste(names(state(xsc)), "=", state(xsc), collapse=", ")))
		}
		if(!compare2cellHTS(xr, xsc)) {
			stop("Difference across 'cellHTS' objects! The scored 'cellHTS' object given in dat[['scored']] was not calculated from the data stored in 'cellHTS' object indicated in 'dat[['raw']]'!")
		}
		## If 'scored' component was given, than 'normalized' component should also be available!
		if(is.null(xn)) stop("Please add to 'cellHTSlist' list a component named 'normalized' corresponding to a cellHTS object containing the normalized data!") 
	}
}


## plotPlateArgs verification
plotPlateArgsVerification = function(plotPlateArgs, map) {
	if(is.logical(plotPlateArgs)) {
		if(plotPlateArgs){
			plotPlateArgs <- list(map=map)
		}
	} else {
		if(!is.list(plotPlateArgs)) {
			stop("'plotPlateArgs' must either be logical or a list.") 
		} else {
			if(!all(names(plotPlateArgs) %in% c("sdcol", "sdrange", "xcol", "xrange", "map"))) {
				stop("Only elements 'sdcol', 'sdrange', 'xcolx' and 'xrange' are allowed for 'plotPlateArgs' list!")
			}
			plotPlateArgs$map = map
		}
	}  	
	return(plotPlateArgs)
}


## imageScreenArgs verification
imageScreenArgsVerification = function(imageScreenArgs, map) {	
	if(is.list(imageScreenArgs)) {
		if(!("map" %in% names(imageScreenArgs))){
			imageScreenArgs$map = map
		}
		if(!("ar" %in% names(imageScreenArgs))){
			imageScreenArgs$ar = 1
		}
		if(!all(names(imageScreenArgs) %in% c("ar", "zrange", "map","anno"))) {
			stop("Only elements 'ar', 'zrange', 'map' and 'anno'are allowed for 'imageScreenArgs' list!")
		}
	} else {
		if(!is.null(imageScreenArgs)) {
			stop("'imageScreenArgs' must either be a list or NULL.")
		} else {
			imageScreenArgs=list(map=map)
		}
	}	
	return(imageScreenArgs)
}


## create Output folder after ensuring that there will not be accidental file deletion
createOutputFolder = function(outdir, xr, force) {
	## See if output directory exists. If not, create. If yes, check if it is empty,
	## and if not, depending on parameter 'force', throw an error or clean it up.	
	if(missing(outdir)) {
		if(force){
			stop("To prevent accidental deletion of files, please specify 'outdir' explicitely if you want to use the 'force=TRUE' option.")
		}
		outdir = file.path(getwd(), name(xr))
	}
	if(file.exists(outdir)){
		if(!file.info(outdir)$isdir){
			stop(sprintf("'%s' must be a directory.", outdir))
		}
		outdirContents = dir(outdir, all.files = TRUE)
		outdirContents = setdiff(outdirContents, c(".", ".."))  
		if(!force && length(outdirContents)>0) {
			stop(sprintf("'%s' is not empty.", outdir))
		}
		if(force || length(outdirContents)==0){
			message(sprintf("The report will be written in '%s'. ", outdir))
		}	
	} else {
		dir.create(outdir, recursive=TRUE)
	}
	## create "in" folder
	dir.create(file.path(outdir, "in"))
	return(outdir)
}


##  Step 4 : Add plate result files and write with overall QC results 
writePlateResultFiles = function(outdir, xr, progressReport, timeCounter, totalTime, steps2Do, nsteps) {	
	wh = which(plateList(xr)$status=="OK")
	nm = file.path("in", names(intensityFiles(xr)))
	for(w in wh) {
		txt = intensityFiles(xr)[[w]]		
		if(is.null(txt)){
			stop(sprintf("CellHTS object is internally inconsistent, plate %d (%s) is supposedly OK but has no raw data file.",	as.integer(w), nm[w]))
		}
		writeLines(txt, file.path(outdir, nm[w]))	
	} 
}


## save the script for the whole experience in the 'in' folder of the compendia
saveMainScript = function(mainScriptFile, outputFile) {
	if(is.na(mainScriptFile)) {
		message('WARNING : The main script of this cellHTS2 computing has not been saved into the html report.')
	} else {
		file.copy(from = mainScriptFile, to = outputFile, overwrite=TRUE)
	}
}


## Main function
## NOTE: 'writeReport' can be called on different cellHTS objects at different preprocessing stages
## Arguments:
## 'cellHTSlist'  should be a list of cellHTS object(s) obtained for the same experimental data. Allowed components are:
## 'raw' - (mandatory) cellHTS object containing raw experiment data.
## 'normalized' (mandatory only if component 'scored' is given)- cellHTS object containing normalized data.
## 'scored' - cellHTS object comprising scored data.
## e.g. cellHTSlist = list("raw" = xr, "normalized"=xn, "scored"=xsc)
##
## Steps inside writeReport:
## Step 1 - creating the output directory
## Step 2 - Controls annotation (only if overallState["configured"]=TRUE)
## Step 3 - QC per plate & channel (only if overallState(x)["configured"]=TRUE)
## Step 4 - Add plate result files, main script, and write the overall QC results in the 'in' folder of the report' 
## Step 5 - Per experiment QC
## Step 6 - topTable  (only if scored data are available)
## Step 7 -  Screen-wide image plot (only if scored data are available)	
writeReport = function(cellHTSlist,outdir,force=FALSE, map=FALSE,plotPlateArgs=FALSE,imageScreenArgs=NULL,progressReport=interactive(),posControls,negControls, mainScriptFile = NA) {
	
	## Verification of the arguments
	cellHTSlistVerification(cellHTSlist)	
	if (!is.logical(progressReport)) {stop("'progressReport' must be a logical value.")	}
	if (!is.logical(map)){stop("'map' must be a logical value.")}
	
	##Initialization
	nm <- names(cellHTSlist)
	xr <- cellHTSlist[["raw"]]
	xn <- cellHTSlist[["normalized"]]
	xsc <- cellHTSlist[["scored"]] 	
	xraw <- Data(xr)  ## xraw should always be given!
	xnorm <- if(is.null(xn)) xn else Data(xn)
	
	## dimensions 
	d <- as.integer(dim(xraw))
	nrWell    <- prod(pdim(xr))
	nrPlate   <- max(plate(xr))
	nrReplicate <- as.numeric(d[2])
	nrChannel <- if(!is.null(xnorm)) as.integer(dim(xnorm)[3]) else d[3]  ## will be defined based on xnorm, if it exists
	objState <- sapply(cellHTSlist, function(i){ if(!is.null(i))state(i)})	
	overallState <- apply(objState, 1, any)
	whAnnotated <- colnames(objState)[objState["annotated",]]
	
	## initializations
	twoWay <- FALSE
	wAnno = as.character(wellAnno(xr))
	
	## the overview table of the plate result files in the experiment,
	##  plus the (possible) urls for each table cell
	exptab = plateList(xr)
	url = matrix(as.character(NA), nrow=nrow(exptab), ncol=ncol(exptab))
	colnames(url) = colnames(exptab)
	qmHaveBeenAdded = FALSE		
	plotPlateArgs=plotPlateArgsVerification(plotPlateArgs, map)
	imageScreenArgs=imageScreenArgsVerification(imageScreenArgs, map)
	
	## Progress bar : Rough estimation of the total computation time that the function will take
	## 1 = one time unit	
	if (progressReport){
		timeCounter=0
		timePerStep <- c(
				step1 = 5,
				step2 = 15,
				step3 = nrPlate*nrReplicate*nrChannel*(1 + if(is.list(plotPlateArgs)) 3 + plotPlateArgs$map else 0),
				step4 = 0.1*sum(plateList(xr)$status=="OK") + 2*nrChannel*nrReplicate,
				step5 = 8*nrChannel*nrReplicate, 
				step6 = 20*nrChannel*nrReplicate,
				step7 = nrPlate*(0.5)  
				)				
		steps2Do <- names(timePerStep)[c(TRUE, rep(overallState[["configured"]],2), TRUE, TRUE, rep(overallState[["scored"]],2))]
		totalTime <- sum(timePerStep[steps2Do])
		nsteps <- length(steps2Do)		
		require("prada")    
		progress(title="cellHTS2 is busy", message = sprintf("\nCreating HTML pages for '%s'. \nFound %s data.\nState:\n%s", name(xr), if(length(cellHTSlist)>1)  paste(paste(nm[-length(cellHTSlist)], collapse=", "), "and",  nm[length(cellHTSlist)],  collapse=" ") else nm , paste(paste("configured", overallState[["configured"]], sep=" = "), paste("annotated", overallState[["annotated"]], sep=" = "), sep=", ")),  sub=sprintf("step %s of %s", 1, nsteps))
		timeCounter = myUpdateProgress(progressReport, 0, totalTime, 1, nsteps, 0)
		on.exit(killProgress(), add=TRUE)
	}	
	
	
	## Step 1 : Creating the output directory	
	outdir = createOutputFolder(outdir, xr, force)	
	if(overallState["configured"]) {
		nm = file.path("in", "Description.txt")
		writeLines(screenDesc(xr), file.path(outdir, nm))
	} 			
	timeCounter = myUpdateProgress(progressReport, timeCounter, totalTime, match("step1", steps2Do)+1, nsteps, timePerStep["step1"])		
	
	
	## step 2 : Controls annotation
	if(overallState["configured"]) {	
		
		if (!missing(posControls)) {
			## checks, determine assay type and name of positive controls if assay is one-way
			namePos <- checkPosControls(posControls, nrChannel, wAnno, plateConf(xr)$Content)
			twoWay <- namePos$twoWay
			namePos <- namePos$namePos 
		}else{## if !missing assumes the screen is a one-way assay
			posControls <- as.vector(rep("^pos$", nrChannel))
			namePos <- "pos"
		}    
		if (!missing(negControls)) {
			checkControls(y=negControls, len=nrChannel, name="negControls") #check
		} else {  
			negControls <- as.vector(rep("^neg$", nrChannel))
		}		
		
		## Define the bins for the histograms (channel-dependent)
		brks = apply(if(overallState["normalized"]) { xnorm } else { xraw }, 3, range, na.rm=TRUE)
		brks = apply(brks, 2, function(s) pretty(s, n=ceiling(nrWell/10))) 
		if(!is(brks, "list")) {brks=split(brks, col(brks))}
		
		## put as list also for the case ch=1 or for the case when brks have equal length for each channel 		
		## Correct wellAnno information:
		## ... by taking into account the wells that were flagged in the screen log file, 
		## or even by the user manually in xraw. Besides the categories in wellAnno(x), it contains the category "flagged".
		xrawWellAnno = getArrayCorrectWellAnno(xr)
		
		## put as array with dimensions nrWells x nrPlates x nrReplicates x nrChannels
		xrawWellAnno = array(xrawWellAnno, dim=c("Wells"=nrWell, "Plates"=nrPlate, nrReplicate, dim(xrawWellAnno)[3])) ## don't use variable 'nrChannel' because it may be different when defined based on xnorm data!
		
		## Create geneAnnotation info for the image maps:
		if(overallState["annotated"]){
			## follow the order 'scored' - 'normalized' - 'raw'
			for(i in c("scored", "normalized", "raw")) {
				if(i %in% whAnnotated) {
					screenAnno <- fData(cellHTSlist[[i]]) 
					break
				}
			}      
			geneAnnotation <- if ("GeneSymbol" %in% names(screenAnno))  screenAnno$GeneSymbol else screenAnno$GeneID
		} else { ##else if annotated
			geneAnnotation <- well(xr)
		} ##else annotated
		## data
		if(overallState["normalized"]) {
			dat <- xnorm
			whatDat = "normalized"
		} else {
			dat <- xraw
			whatDat <- "unnormalized"
		}		
		
		## which of the replicate plates has not just NA values
		datPerPlate <- array(dat, dim=c("Wells"=nrWell, "Plates"=nrPlate, nrReplicate, nrChannel)) 
		hasData <- apply(datPerPlate, 2:4, function(z) !all(is.na(z))) # nrPlates x nrReplicates x nrChannels		
		
		##   -------  Get controls positions (for all plates) ---------------
		allControls <- getControlsPositions(posControls, negControls, twoWay, namePos, nrChannel, wAnno)
		actCtrls <- allControls$actCtrls
		inhCtrls <- allControls$inhCtrls
		posCtrls <- allControls$posCtrls
		negCtrls <- allControls$negCtrls		
		
		## get controls positions for each plate
		act <- lapply(actCtrls, function(i) if(is.null(i)) NULL else ctrlsPerPlate(i, nrWell))
		inh <- lapply(inhCtrls, function(i) if(is.null(i)) NULL else ctrlsPerPlate(i, nrWell))
		neg <- lapply(negCtrls, function(i) if(is.null(i)) NULL else ctrlsPerPlate(i, nrWell))
		pos <- vector("list", length=nrChannel)
		for (ch in 1:nrChannel) {
			notNull <- !sapply(posCtrls[[ch]], is.null)
			if(any(notNull)) {
				pp <- posCtrls[[ch]][notNull]
				pos[[ch]] <- lapply(pp, ctrlsPerPlate, nrWell)
			} 
		}		
		
		## Get per-plate dynamic range,  per-plate repeatability standard deviation (plate replicates), Z'-factor for each replicate and channel (needed as input for QMexperiment later on)
		if(whatDat=="normalized") {
			dr <- getDynamicRange(xn, verbose=FALSE, posControls=posControls, negControls=negControls)
			repMeasures <- getMeasureRepAgreement(xn, corr.method="spearman")
			allZfac <- getZfactor(xn, verbose=FALSE, posControls=posControls, negControls=negControls) 
		} else { #use cellHTS object containing raw data
			dr <- getDynamicRange(xr, verbose=FALSE, posControls=posControls, negControls=negControls)
			repMeasures <- getMeasureRepAgreement(xr, corr.method="spearman")
			allZfac <- getZfactor(xr, verbose=FALSE, posControls=posControls, negControls=negControls) 			
		}		
		if(all(is.null(names(dr)))) {names(dr) <- namePos}
		
		## Define well colors and comment on them.
		## (to avoid having the legend for 'pos' when we have 'inhibitors' and 'activators' or vice-versa)
		wellTypeColor <- if(twoWay) c(neg="#2040FF", act="#E41A1C", inh="#FFFF00", sample="#000000", controls="#43FF00",
							other="#999999", empty="#FA00FF", flagged="#000000") else c(pos="#E41A1C", neg="#2040FF", sample="#000000", controls="#43FF00", other="#999999", empty="#FA00FF", flagged="#000000")
		
		## assign common arguments for the plate plots
		if(is.list(plotPlateArgs)) {
			
			## Currently, it does not allows to use different colors for different channels
			if(is.null(plotPlateArgs$sdcol)) 
				plotPlateArgs$sdcol = brewer.pal(9, "YlOrRd")
			if(is.null(plotPlateArgs$xcol))
				plotPlateArgs$xcol = rev(brewer.pal(9, "RdBu"))
			
			## set this argument as a list with the same length as the number of channels
			if(is.null(plotPlateArgs$xrange)) { 
				plotPlateArgs$xrange <- vector("list", length=dim(dat)[3])
			} else {
				if (!is.list(plotPlateArgs$xrange)) {
					plotPlateArgs$xrange <- list(plotPlateArgs$xrange)
					length(plotPlateArgs$xrange)=dim(dat)[3]} 
			}
			
			## set this argument as a list with the same length as the number of channels
			if(is.null(plotPlateArgs$sdrange)) {
				plotPlateArgs$sdrange <- vector("list", length=dim(dat)[3])
			} else {
				if (!is.list(plotPlateArgs$sdrange)) {
					plotPlateArgs$sdrange <- list(plotPlateArgs$sdrange)
					length(plotPlateArgs$sdrange) = dim(dat)[3]
				} 
			}	
		} 			
		timeCounter = myUpdateProgress(progressReport, timeCounter, totalTime, match("step2", steps2Do)+1, nsteps, timePerStep["step2"])			
	}
	
	
	##  Step 3 : QC per plate & channel
	## writes a report for each Plate, and prepare argument for the writing of the table with overall CQ results
	if(overallState["configured"]) {		
		allmt <- match(wAnno, names(wellTypeColor))		
		for(p in 1:nrPlate){
			wh = with(plateList(xr), which(Plate==p & status=="OK"))
			if(length(wh)>0) {
				dir.create(file.path(outdir, p))
				## QMbyPlate also writes the QC report for the current plate with making res
				res <- QMbyPlate(platedat=datPerPlate[, p,,, drop=FALSE], 
						pdim=pdim(xr), 
						name=sprintf("Plate %d (%s)", p, whatDat),
						basePath=outdir, 
						subPath=p, 
						genAnno=geneAnnotation[nrWell*(p-1)+(1:nrWell)], 
						mt=allmt[nrWell*(p-1)+(1:nrWell)],
						plotPlateArgs=plotPlateArgs, 
						brks = brks,
						finalWellAnno = xrawWellAnno[,p,,, drop=FALSE], 
						activators=act, inhibitors=inh, positives=pos, negatives=neg, 
						isTwoWay=twoWay, namePos=namePos, wellTypeColor=wellTypeColor,
						plateDynRange=lapply(dr, function(i) i[p,,,drop=FALSE]), 
						plateWithData=hasData[p,,, drop=FALSE], repMeasures=repMeasures)
				url[wh, "status"] = res$url				
				if(!qmHaveBeenAdded) {
					if(twoWay){
						TableNames = c(paste("Replicate dynamic range", c("(Activators)", "(Inhibitors)"), sep=" "), paste("Average dynamic range", c("(Activators)", "(Inhibitors)"), sep=" "), "Spearman rank correlation")
					} else {## if twoWay						
						if(length(namePos)==1 && namePos=="pos"){ 
							TableNames = c("Replicate dynamic range", "Average dynamic range", "Repeatability standard deviation", sprintf("Spearman rank correlation %s", ifelse(nrReplicate==2, "", "(min - max)")))
							
						} else {
							TableNames = c(sprintf("Replicate dynamic range (%s)", namePos), 
									sprintf("Average dynamic range (%s)", namePos), "Repeatability standard deviation", 
									sprintf("Spearman rank correlation %s", ifelse(nrReplicate==2, "", "(min - max)")))
						}
					}## else twoWay
					url = cbind(url,  matrix(as.character(NA), nrow=nrow(url), ncol=length(TableNames)))					
					for (j in TableNames) exptab[, j] = rep("", nrow(exptab))
					qmHaveBeenAdded = TRUE
				}## if !qmHaveBeenAdded
				whh = split(wh, exptab$Channel[wh])
				for(ch in 1:length(res$qmsummary)) { ## Channels
					resCh = res$qmsummary[[ch]]
					whCh = whh[[ch]]
					selrep= exptab$Replicate[whCh]
					if(twoWay){
						for (jj in 1:length(TableNames)){
							exptab[whCh, TableNames[jj]] = resCh[unique((jj<3)*(selrep+nrReplicate*(jj-1))) + (jj>2)*(nrReplicate*2 + jj-2)]
						}
						##"Replicate dynamic range (Activators)"
						##"Replicate dynamic range (Inhibitors)"
						##TableNames[3] "Average dynamic range (Activators)"
						##TableNames[4] "Average dynamic range (Inhibitors)"
						##TableNames[5] "Repeatability standard deviation"
						##TableNames[6] "Spearman rank correlation"				
					} else { ##oneway
						for (jj in 1:(length(TableNames)-2)){ #exclude "Repeatability standard deviation" and "Spearman rank correlation"
							exptab[whCh, TableNames[jj]] = resCh[unique((jj<(length(namePos)+1))*(selrep + (nrReplicate+1)*(jj-1))) + (jj>length(namePos))*(nrReplicate + 1)*(jj-length(namePos))]
						}
						exptab[whCh, TableNames[length(TableNames)-1]] = resCh[length(resCh)-1]
						exptab[whCh, TableNames[length(TableNames)]] = resCh[length(resCh)]
					}## else twoWay
				}## for channel
			}## if length w			
			## update the progress bar each time a plate is completed. Once the computation has been done for every Plate, step 3 is completed
			timeCounter = myUpdateProgress(progressReport,timeCounter, totalTime, match("step3", steps2Do), nsteps, timePerStep["step3"]/nrPlate)				
		}## for p plates				
		timeCounter = myUpdateProgress(progressReport, timeCounter, totalTime, match("step3", steps2Do)+1, nsteps, 0)			
	}	
	
	
	## step 4 : write the plate results files and also the main script into the "in" folder
	writePlateResultFiles(outdir, xr, progressReport, timeCounter, totalTime, steps2Do, nsteps)  
	saveMainScript(mainScriptFile, outputFile = file.path(outdir, file.path("in", "mainScript.txt")	)) 	
	timeCounter = myUpdateProgress(progressReport, timeCounter, totalTime, match("step4", steps2Do), nsteps, 0.2*length(which(plateList(xr)$status=="OK")))
	
	
	## steps 5, 6, 7 :
	
	## saving javascript file for mouseover bubbles	
	file.copy(from = system.file(file.path("extdata",'wz_tooltip.js'), package="cellHTS2"), to =file.path(outdir, "wz_tooltip.js"), overwrite = TRUE)
	
	## saving the glossary as a web page. createGlossary() returns a glossary with all the definitions 	
	saveHtmlGlossary(createGlossary(), file.path(outdir,'glossary.html'))	
	
	## write index.html
	indexFile = file.path(outdir, "index.html")
	writeMainIndexHtml(outdir, indexFile, url, cellHTSlist, xr, xn, overallState, nm, exptab, imageScreenArgs, nrChannel, nrReplicate, progressReport, timeCounter, timePerStep, totalTime, nsteps, steps2Do, nrPlate, posControls, negControls, allControls, allZfac)
	timeCounter = myUpdateProgress(progressReport, totalTime, totalTime, match("step7", steps2Do), nsteps, 0)	## end : timeCounter = 	totalTime
	
	## finally, return indexFile
	return(indexFile)
	
}



