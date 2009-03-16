## Ligia P. Bras (September 2007)
## Function that shows the plate configuration as an image screen
## The default color code is similar to that used in writeReport
configurationAsScreenPlot <- function(x, verbose=interactive(), posControls, negControls)
{
    ## optional inputs: names of 'pos' and 'neg' controls given as vectors of
    ## regular expressions
    ## initial checks:
    if(!is(x, "cellHTS"))
        stop("'x' should be of class 'cellHTS'.")
    ## Check the status of the 'cellHTS' object
    if(!state(x)[["configured"]])
        stop("Please configure 'x' (using the function 'configure') before normalization.")
    require("splots") || stop("Package 'splots' was not found and needs to be installed.")
    wellAnnotation <- as.character(wellAnno(x))
    wellCols <- c(sample="#999999", other="#000000", empty="#FFFFFF")
    pcolPal <- rev(brewer.pal(9, "Reds")[-c(1,2,9)])
    ncolPal <- rev(brewer.pal(9, "Blues")[-c(1,2,9)])
    if(missing(negControls)) negControls <- "^neg$"
    if(missing(posControls)) posControls <- "^pos$"
    ##this gives the index in wellAnno(x)
    negInd <- findControls(negControls, as.character(wellAnno(x)))  
    posInd <- findControls(posControls, as.character(wellAnno(x)))
    aux <- if(is.list(posInd)) sapply(posInd, length)==0 else length(posInd)==0
    if(any(aux) & verbose)
        warning(sprintf("'%s' not found among the well annotation!\n",
                        posControls[which(aux)]))
    aux <- if(is.list(negInd)) sapply(negInd, length)==0 else length(negInd)==0
    if(any(aux) & verbose)
        warning(sprintf("'%s' not found among the well annotation!\n",
                        negControls[which(aux)]))
    namePos <- unique(sapply(posInd, function(i) unique(wellAnnotation[i])))
    nameNeg <- unique(sapply(negInd, function(i) unique(wellAnnotation[i])))
    ## update well colors and update well colors with pos and neg controls
    if(length(namePos))
    {
        cols.pos <- if(length(namePos)==1) pcolPal[1] else colorRampPalette(pcolPal,
                             space="Lab")(length(namePos))
        names(cols.pos) <- namePos
        wellCols <- c(wellCols, cols.pos)
    }
    if(length(nameNeg))
    {
        cols.neg <-
            if(length(nameNeg)==1) ncolPal[1] else colorRampPalette(ncolPal,
                     space="Lab")(length(nameNeg))
        names(cols.neg) <- nameNeg
        wellCols <- c(wellCols, cols.neg)
    }
    ## remove unused well annotation
    wellCols <- wellCols[names(wellCols) %in% unique(wellAnnotation)]
    mtW <- match(wellAnnotation, names(wellCols))
    if(any(is.na(mtW)))
    {
        wh <- is.na(mtW)
        notCovered <- unique(wellAnnotation[wh])
        notCov <- substr(rainbow(length(notCovered)), 1, 7)
        names(notCov) <- notCovered
        wellCols <- c(wellCols, notCov)
        mtW[wh] <- match(wellAnnotation[wh], names(wellCols))
    }
    ## simple configuration based exclusively on plate configuration file (i.e. without any
    ## update for flagged wells over plates, replicates or channels)
    ## just a single image plot of all plates:
    ldat <- split(mtW, plate(x))
    grid.newpage()
    fill <- if(length(wellCols)>1) wellCols else rep(wellCols, 2)
    zrange <- if(length(wellCols)>1) range(mtW) else c(0, 1)
    plotScreen(ldat, zrange=zrange, fill=fill, #na.fill="yellow", 
               nx=pdim(x)[["ncol"]], ny=pdim(x)[["nrow"]], main=" ",
               ncol=ifelse(max(plate(x)) < 6L, max(plate(x)), 6L))
    ## put correct names (name given in the conf file)
    aux <- sapply(names(wellCols), function(i)
                  plateConf(x)$Content[match(i, tolower(plateConf(x)$Content))]) 
    if(any(is.na(aux)))
        aux[is.na(aux)] <- names(wellCols)[is.na(aux)]
    names(wellCols) <- aux
    invisible(wellCols)
}
