## Ligia P. Bras (September 2007)
## Function that shows the plate configuration as an image screen
## The default color code is similar to that used in writeReport
configurationAsScreenPlot <- function(x, verbose=interactive(), posControls, negControls){
	## optional inputs: names of 'pos' and 'neg' controls given as vectors of regular expressions
	## initial checks:
	if(!inherits(x, "cellHTS")) stop("'x' should be of class 'cellHTS'.")
	## Check the status of the 'cellHTS' object
	if(!state(x)[["configured"]])
		stop("Please configure 'x' (using the function 'configure') before normalization.")
	require("splots") || stop("Package 'splots' was not found and
					needs to be installed.")
	wellAnnotation <- as.character(wellAnno(x))
	wellCols <- c(sample="#999999", other="#000000", empty="#FFFFFF")
	## 'sample' - grey "#999999"
	## 'other' - black "#000000"
	## 'empty' - white "#FFFFFF"
	## 'pos' - red "#E41A1C"
	## 'neg' - blue "#2040FF"
	## uncovered annotation - terrain.colors()
	
	if(missing(negControls)) negControls <- "^neg$"
	if(missing(posControls)) posControls <- "^pos$"
	
	negInd <- findControls(negControls, as.character(wellAnno(x)))  ##this gives the index in wellAnno(x)
	posInd <- findControls(posControls, as.character(wellAnno(x)))
	
	if(is.list(posInd))  aux <- sapply(posInd, length)==0 else aux <- (length(posInd)==0)
	if(any(aux) & verbose) sprintf("'%s' not found among the well annotation!\n", posControls[which(aux)])
	
	if(is.list(negInd))  aux <- sapply(negInd, length)==0 else aux <- (length(negInd)==0)
	if(any(aux) & verbose) sprintf("'%s' not found among the well annotation!\n", negControls[which(aux)])
	
	namePos <- unique(sapply(posInd, function(i) unique(wellAnnotation[i])))
	nameNeg <- unique(sapply(negInd, function(i) unique(wellAnnotation[i])))
	
	if(length(namePos)) {
		##update well colors and update well colors with pos and neg controls and remove unused well annotation:
		cols.pos <- if(length(namePos)==1) "#E41A1C" else colorRampPalette(c("red", "orange"), space="Lab")(length(namePos))
		names(cols.pos) <- namePos
		wellCols <- c(wellCols, cols.pos)
	}
	
	if(length(nameNeg)){
		cols.neg <- if(length(nameNeg)==1) "#2040FF" else colorRampPalette(c("darkblue",  "blue"), space="Lab")(length(nameNeg))
		names(cols.neg) <- nameNeg
		wellCols <- c(wellCols, cols.neg)
	}
	
	wellCols <- wellCols[names(wellCols) %in% unique(wellAnnotation)]
	
	mtW <- match(wellAnnotation, names(wellCols))
	
	if(any(is.na(mtW))) {
		wh <- is.na(mtW)
		notCovered <- unique(wellAnnotation[wh])
		notCov <- rainbow(length(notCovered))
		names(notCov) <- notCovered
		wellCols <- c(wellCols, notCov)
		mtW[wh] <- match(wellAnnotation[wh], names(wellCols))
	}
	
	## simple configuration based exclusively on plate configuration file (i.e. without any update for flagged wells over plates, replicates or channels)
	## just a single image plot of all plates:
	ldat = split(mtW, plate(x))
	grid.newpage()
	
	plotScreen(ldat, zrange=range(mtW), fill=wellCols, ##na.fill="yellow", 
			nx=pdim(x)[["ncol"]], ny=pdim(x)[["nrow"]], main="Plate configuration", ncol=ifelse(max(plate(x))<6L, max(plate(x)), 6L))
	
	## put correct names:
	aux <- sapply(names(wellCols), function(i) plateConf(x)$Content[match(i, tolower(plateConf(x)$Content))]) ##name given in the conf file
	if(any(is.na(aux))) {
		aux[is.na(aux)] <- names(wellCols)[is.na(aux)]
	}  
	names(wellCols) <- aux
	invisible(wellCols)
}
