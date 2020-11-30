# functions previously imported from the now-deprecated package prada

######################################################################################
#################################### plotPlate #######################################
######################################################################################

plotPlate <- function(x,nrow = 8, ncol = 12, col=c("red", "blue"),
                      ind = 1:(ncol*nrow), xrange=function(y) range(y, na.rm=TRUE),
                      na.action = "zero", main, char,
                      desc = character(2), add=FALSE,
                      gridFun="default", funArgs=NULL, ...){

  ## this is the interface to plotPlate. It checks for parameter validity and 
  ## performs some preparation of the data. Subsequent calls to .gridPlot
  ## and .defaultPlot do the actual plotting. It then calculates the coordinates
  ## for the imageMap


  ############################## parameter validation ################################
  ## ncol
  if (!is.numeric(ncol) || length(ncol) != 1)   
    stop("'ncol' must be a numeric vector of length 1")
  
  ## nrow
  if (!is.numeric(nrow) || length(nrow) != 1)
    stop("'nrow' must be a numeric vector of length 1")
  nrwell <- ncol * nrow

  ## gridFun
  default <- FALSE
  if(length(gridFun)!=1 || !is.character(gridFun)){
    stop("'gridFun' must be character of length 1")
  }else{
    if(gridFun=="default")
      default <- TRUE
  }#end else
  
  ## char
  info <- character(nrwell)
  if(!missing(char)){
    if (!is.vector(char) || length(char) != length(ind) ||
       !all(nchar(char, keepNA=FALSE)<=2))
       stop(paste("\n'char' must be a  vector of length 'ncol*nrow'",
                 "\nor of length equal to inf with vector items nchar<=2",
                 "or 'NA'.\nYou might want to include indices for ",
                 "missing wells."))
    char[is.na(char)] <- ""
    info[ind] <- char
  }#end if
  
  ## xrange
  if(is.function(xrange))
      xrange <- xrange(x)
  if (!is.numeric(xrange) || length(xrange) != 2 || any(is.na(xrange)))
    stop("'xrange' must be a numeric vector of length 2 with no NA or ",
         "a function producing such a vector.")
  
  ## desc
  if(default)
    if(!is.character(desc) || length(desc) != 2)
      stop("'desc' must be a character vector of length 2")
  
  ## x (transform to matrix)
  if(!is.numeric(x))
    stop("'x' must be numeric.")
  if(is.matrix(x)){
    if(nrow(x) != length(ind))
      stop("'nrow(x)' must be equal to 'length(ind)'. If you have missing wells,",
           "please use the argument 'ind' to indicate these")
  } else {
    if(length(x) != length(ind))
      stop("'length(x)' must be equal to 'length(ind)'. If you have missing wells,",
           "please use the argument 'ind' to indicate these")
    x = matrix(x, ncol=1)
  }#end else
  data <- matrix(NA, ncol=ncol(x), nrow=nrwell)
  
  ## ind (deal with missing wells)
  if (any(duplicated(ind)) || (max(ind, na.rm = TRUE) > nrwell))
    stop("'ind' must be vector of unique indices for vector 'x'")
  data[ind, ] <- x
  
  ## funArgs
  if(!is.null(funArgs)){
    if(default)
      warning("'funArgs' are ignored for default plotting function")
    if(!is.data.frame(funArgs) || nrow(funArgs)!=nrow(x))
      stop("'funArgs' must be data frame with same number of rows as 'x'")
  }#end if

  ## default plotting arguments
  defArgs <- list(cex.main=1.8, cex.lab=1.6, cex.char=1.8, cex.legend=1,
                  cex.desc=1.4)
  usrArgs <- list(...)
  if(length(usrArgs))
    for(i in 1:length(usrArgs)){
      if(!is.null(names(usrArgs)[i])){
        arg <- match.arg(names(usrArgs)[i], names(defArgs))
        defArgs[arg] <- usrArgs[i]
      }#end if
  }#end for

  ## add
  if(!is.logical(add) || length(add)!=1)
    stop("'add' must be logical of length 1")
  
  ############################ getting device info  #################################
  ## device size and resolution ##
  res <- devRes()
  
  ## reinitialize plot
  device <- names(dev.cur())
  if(!add && !device %in% c("pdf", "postscript")){
    mar <- par("mar")
    par(mar=rep(0,4))
    plot.new()
    par(mar=mar)
  }

  ## setting up aspect ratio
  devWidth <- par("fin")[1]*res[1]
  devHeight <- par("fin")[2]*res[2]
  outerFrame <- vpLocation()
  if(ncol>nrow)
  {
      outerFrame$size[2] <- min(outerFrame$size[2], outerFrame$size[1]/
                                (((ncol+1)*0.1+ncol+1)/((nrow+1)*0.1+nrow+1)))
  }else{
      outerFrame$size[1] <- min(outerFrame$size[1], outerFrame$size[2]/
                                (((nrow+1)*0.1+nrow+1)/((ncol+1)*0.1+ncol+1)))
  }
  outerVp <- viewport(width=unit(outerFrame$size[1]*72/res[1], "bigpts"),
                      height=unit(outerFrame$size[2]*72/res[2], "bigpts"))
  pushViewport(outerVp)  # this vp makes sure we plot in the correct aspect ratio
  innerVp <- viewport(width=0.95, height=0.95)
  pushViewport(innerVp)
  innerFrame <- vpLocation()

  ## The optimal fontsizes
  availSize <- min(((innerFrame$isize*c(0.9, ifelse(missing(main), 1, 0.9)))/
                    c(ncol*nchar(ncol), nrow*((nrow%/%26)+1)) * 0.8))*72
  fontsize <- ceiling(12*outerFrame$size[1]/900)
  defArgs$fontsize.lab <- min(fontsize, defArgs$cex.lab * availSize, availSize)
  defArgs$fontsize.char <- min(fontsize, defArgs$cex.char * availSize, availSize)
  

 

  ########################### call plotting functions ################################
  if(default)
    tp <- .defaultPlot(data, col, xrange, fontsize, info, desc, main, na.action,
                 ncol, nrow, nrwell, ind, defArgs)
  else
    tp <- .arrayPlot(data, gridFun, funArgs, fontsize, info, main, na.action,
               ncol, nrow, nrwell, ind, defArgs)
  popViewport(2)
  
  ############################# imageMap coordinates  ################################
  dx = dy = 0.45
  xlim = c(0, ncol + 1)
  ylim = c(0, nrow + 1)
  fw <- diff(xlim)/0.9
  fh = diff(ylim)/0.9
  u2px = function(x) (x - xlim[1])/fw * innerFrame$size[1]
  u2py = function(y) (y - ylim[1])/fh * innerFrame$size[2]
  x0 = 1.5 + (tp$wh - 1)%%ncol
  y0 = 0.1 * diff(ylim) + 0.6 + (tp$wh - 1)%/%ncol
  x1 = u2px(x0 - dx) + innerFrame$location[1]
  x2 = u2px(x0 + dx) + innerFrame$location[1]
  y1 = u2py(y0 - dy) + devHeight - innerFrame$location[4]
  y2 = u2py(y0 + dy) + devHeight - innerFrame$location[4]

  return(invisible(list(which = tp$wh,
                        coord = floor((cbind(x1, y1, x2, y2)+0.5)),
                        width = outerFrame$size[1], height = outerFrame$size[2])))
}#end function


######################################################################################
################################ helper functions ####################################
######################################################################################

devDims <- function(width, height, ncol=12, nrow=8, res=72){
 f <- (((ncol+1)*0.1+ncol+1)/((nrow+1)*0.1+nrow+1))
 if((missing(width) & missing(height) || !missing(width) & !missing(height)))
   stop("Need either argument 'width' or argument 'height'")
 if(missing(height))
   return(list(width=width, height=width/f, pwidth=width*res, pheight=width/f*res))
 else
   return(list(width=height*f, height, pwidth=height*f*res, pheight=height*res))
}



devRes <- function(){
  ## find R's resolution for the current device
  if(current.viewport()$name != "ROOT"){
    vpt <- current.vpTree()
    depth <- upViewport(0)
    xres <- abs(as.numeric(convertWidth(unit(1, "inches"), "native")))
    yres <- abs(as.numeric(convertHeight(unit(1, "inches"), "native")))
    downViewport(depth)
  }else{
    xres <- abs(as.numeric(convertWidth(unit(1, "inches"), "native")))
    yres <- abs(as.numeric(convertHeight(unit(1, "inches"), "native")))
  }
  retval <- c(xres, yres)
  names(retval) <- c("xres", "yres")
  return(retval)
}


## Functions for dealing with alphanumeric identifiers for larger well plates
## There may be one or two letters in the string
getAlphaNumeric = function(horizontal, vertical) {
    if (any(horizontal>702)) stop(sprintf("Indices of 'horizontal' well must not exceed %d", 26*27))
    if (any(vertical>99)) stop(sprintf("Indices of 'horizontal' well must not exceed %d.", 99))
    
    alpha1 <- c("", LETTERS) [(horizontal - 1)%/%length(LETTERS) + 1]
    alpha2 <- LETTERS[(horizontal-1) %% length(LETTERS) +1]
    id.num <- sprintf('%02d', vertical)
    id.alpha <- paste(alpha1, alpha2, sep='')
    id <- paste(id.alpha, id.num, sep='')
    return(list(id=id, id.alpha=id.alpha, id.num=id.num))
}
