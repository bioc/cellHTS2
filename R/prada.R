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
################################## .defaultPlot ######################################
######################################################################################

.defaultPlot <- function(data, col, xrange, fontsize, info, desc, main, na.action,
                         ncol, nrow, nrwell, ind, defArgs){
    
  ## this function is used for creating default plate plots. It used an optimized
  ## algorithm for plotting grid circles thus it is much faster than the generic
  ## array plotting function. Since this function also creates a legend, the device
  ## dimensions are a bit different

  ############################ create false colors  ##################################
  nrcolors = 256
  thepalette = colorRampPalette(col)(nrcolors)
  z2icol <- function(z) {
    #if(length(unique(z))==1) #if all z values are the same
    #  return(ceiling(nrcolors/2))
    res = round((z - xrange[1])/diff(xrange) * (nrcolors - 1)) + 1
    res[res > nrcolors] = nrcolors
    res[res < 1] = 1
    return(res)
  }
  icol2z <- function(i) {
    (i - 1)/(nrcolors - 1) * diff(xrange) + xrange[1]
  }
  stopifnot(length(unique(xrange))!=0 ||
                   all(z2icol(icol2z(1:nrcolors)) == 1:nrcolors))
  circcol <- matrix(thepalette[z2icol(data)], ncol=ncol(data), nrow=nrow(data))
  
  ############################## deal with NA values #################################
  wh <- (1:nrwell)[ind]
  switch(na.action,
         zero = {circcol[is.na(circcol)] <- thepalette[z2icol(0)]},
         omit = {wh <- which(!is.na(circcol))},
         xout = {nawell <- which(is.na(circcol))
                 sel <- nawell[which((nawell %in% ind))]
                 circcol[is.na(circcol)] <- "lightgray"},
         stop(paste("Invalid value of 'na.action':", na.action)))
 
  ############################# create grid graphic ##################################
  vp1 <- viewport(width=0.9, x=0, just="left") #main well vp
  ##plot legend
  vp2 <- viewport(width=0.1, x=0.9, just="left") #main legend vp
  pushViewport(vp2)
  vp3 <- viewport(height=0.85, width=0.8) #legend desc vp
  pushViewport(vp3)
  grid.text(y=c(0.95, 0.05), x=0.1, just="left", desc,
            gp=gpar(fontsize=fontsize, cex=defArgs$cex.desc,
            fontface="bold", col=c(col[length(col)], col[1])))
  vp4 <- viewport(height=0.8, width=0.1, yscale=c(xrange), xscale=c(0,1),
                  x=0.1, just="left") #legend bar vp
  pushViewport(vp4)
  nb <- 100
  cols <- colorRampPalette(col)(nb)
  i <- 1:nb
  grid.rect(y=unit(0+i/nb, "npc"), height=unit(1/nb, "npc"),
            gp=gpar(fill=cols, col=cols), just="top")
  grid.rect(gp=gpar(fill=NA))
  at <- signif(seq(xrange[1], xrange[2], length=6)[2:5],2)
  grid.yaxis(at=at, gp=gpar(fontsize=fontsize, cex=1), main=FALSE, label=FALSE)
  grid.text(x=unit(3.5, "native"), y=unit(at, "native"), at, rot=90,
            gp=gpar(fontsize=fontsize, cex=defArgs$cex.legend))
  popViewport(3)

  ##plot wells
  pushViewport(vp1)
  vp5 <- viewport(height=0.9, y=0, just="bottom", xscale=c(0, ncol+1),
                  yscale=c(0, nrow+1)) #outer well vp
  pushViewport(vp5)
  x0 <- ((0:(nrwell - 1))%%ncol)
  y0 <- (nrow - (0:(nrwell - 1))%/%ncol)-1
  xpos <- x0[wh]
  ypos <- y0[wh]
  xdat <- as.matrix(data[wh,])
  vp6 <- viewport(width=unit(1-(1/(ncol+1)), "npc"),
                  height=unit(1-(1/(nrow+1)), "npc"),
                  x=unit(1/(ncol+1), "npc"), y=unit(1/(nrow+1), "npc"),
                  just=c("left","bottom"), xscale=c(0, ncol),
                  yscale=c(0, nrow)) #inner well vp
  pushViewport(vp6)
  grid.rect()
  radius = 0.495  
  x0 = (0:(nrwell - 1)%%ncol) + radius
  y0 = (nrow - (0:(nrwell - 1))%/%ncol) - 1 + radius
  grid.circle(x=unit(x0[wh], "native"), y=unit(y0[wh], "native"),
              r=unit(radius-0.02, "native"),
              gp=gpar(fill=circcol[wh]))
  if(na.action=="xout" && length(sel))
  {
      grid.segments(x0=unit(x0[sel]-0.39, "native"),
                    x1=unit(x0[sel]+0.39, "native"),
                    y0=unit(y0[sel]-0.39, "native"),
                    y1=unit(y0[sel]+0.39, "native"), gp=gpar(lwd=2, col="darkgray"))
      grid.segments(x0=unit(x0[sel]-0.39, "native"),
                    x1=unit(x0[sel]+0.39, "native"),
                    y0=unit(y0[sel]+0.39, "native"),
                    y1=unit(y0[sel]-0.39, "native"), gp=gpar(lwd=2, col="darkgray")) 
  }#end if
  grid.text(x=unit(x0[wh], "native"), y=unit(y0[wh], "native"), info[wh],
            gp=gpar(fontsize=defArgs$fontsize.char, cex=defArgs$cex.char))
  popViewport(1)
  
  ##plot well description
  vp7 <- viewport(width=unit(1-(1/(ncol+1)), "npc"), height=unit(1/(nrow+1),
            "npc"), x=unit(1/(ncol+1), "npc"), y=unit(1/(nrow+1), "npc"),
            just=c("left","top"), xscale=c(0, ncol), yscale=c(0, 1)) #horiz. text vp
  pushViewport(vp7)
  rn <- getAlphaNumeric(horizontal=1:nrow, vertical=1)$id.alpha
  grid.text(x=unit(unique(x0), "native"), y=unit(0.9, "native"),
            seq_len(ncol), just="top", gp=gpar(fontsize=defArgs$fontsize.lab,
                                       cex=defArgs$cex.lab,
                                  fontface="bold"))
  popViewport(1)
  vp8 <- viewport(width=unit(1/(ncol+1), "npc"), height=unit(1-(1/(nrow+1)),
            "npc"), x=unit(1/(ncol+1), "npc"), y=unit(1/(nrow+1), "npc"),
            just=c("right","bottom"), xscale=c(0, 1),yscale=c(0, nrow)) #vert. text vp
  pushViewport(vp8)
  grid.text(x=unit(0.9, "native"), y=unit(unique(y0), "native"),
            rn, gp=gpar(fontsize=defArgs$fontsize.lab, cex=defArgs$cex.lab,
                               fontface="bold"), just="right")
  popViewport(3)
  
  
  if (!missing(main)){
    vp9 <- viewport(height=0.1, y=0.9, just="bottom") #well header vp
    pushViewport(vp9)
    grid.text(main, gp=gpar(fontsize=fontsize, cex=defArgs$cex.main,
                      fontface="bold"))
    popViewport()
  }#end i
  return(list(x0=x0[wh]-radius, y0=y0[wh]-radius, wh=wh))
}#end function










######################################################################################
################################### .arrayPlot #######################################
######################################################################################

.arrayPlot <- function(data, gridFun, funArgs, fontsize, info, main, na.action,
                       ncol, nrow, nrwell, ind, defArgs){

  ## This is the generic plotting function to create any plots in a plate array
  ## format. Its first argument is a matrix with values for each well per row.
  ## The second argument is the name of a grid plotting function defined before
  ## that gets passed on to doCall. The third argument is a data frame with further
  ## arguments for the plotting function. Every row contains the parameters
  ## for one well. No legend is plotted by this function

  ##plot wells
  wh <- (1:nrwell)[ind]
  vp0 <- viewport(width=0.9, x=0, just="left") #main well vp
  pushViewport(vp0)
  vp1 <- viewport(height=0.9, y=0, just="bottom", xscale=c(0, ncol+1),
                  yscale=c(0, nrow+1)) #outer well vp
  pushViewport(vp1)
  x0 <- ((0:(nrwell - 1))%%ncol)
  y0 <- (nrow - (0:(nrwell - 1))%/%ncol)-1
  xpos <- x0[wh]
  ypos <- y0[wh]
  xdat <- as.matrix(data[wh,])
  vp2 <- viewport(width=unit(1-(1/(ncol+1)), "npc"),
                  height=unit(1-(1/(nrow+1)), "npc"),
                  x=unit(1/(ncol+1), "npc"), y=unit(1/(nrow+1), "npc"),
                  just=c("left","bottom"), xscale=c(0, ncol),
                  yscale=c(0, nrow)) #inner well vp
  pushViewport(vp2)
  
 
  for(i in c(1,1:length(xpos))){ #need to plot 1st well twice?!
    vptemp <- viewport(height=unit(1, "native"), width=unit(1, "native"),
                       x=unit(xpos[i], "native"), y=unit(ypos[i], "native"),
                       just=c("left", "bottom")) #individual well vp
    pushViewport(vptemp)
    thisArgs <- c(list(data=xdat[i,]), as.list(funArgs[i, ]))
    if(!(na.action=="omit" & all(is.na(xdat[i,])))){
      do.call(gridFun, thisArgs) #call plotting function
      grid.text(x=0.5, y=0.5, info[i], gp=gpar(fontsize=fontsize,
                                         cex=defArgs$cex.char))
      if(na.action=="xout" & all(is.na(xdat[i,]))){
        grid.lines(unit(c(0.1, 0.9), "native"), unit(c(0.1,
                   0.9), "native"), gp=gpar(lwd=2, col="darkgray")) 
        grid.lines(unit(c(0.9, 0.1), "native"), unit(c(0.1,
                   0.9), "native"), gp=gpar(lwd=2, col="darkgray"))
      }#end if
    }#end if
    popViewport(1)
    grid.rect(gp=gpar(fill=NA))
  }#end for  
  popViewport(1)
  
  ##plot well description
  vp3 <- viewport(width=unit(1-(1/(ncol+1)), "npc"), height=unit(1/(nrow+1),
            "npc"), x=unit(1/(ncol+1), "npc"), y=unit(1/(nrow+1), "npc"),
            just=c("left","top"), xscale=c(0, ncol), yscale=c(0, 1)) #horiz. text vp
  pushViewport(vp3)
  grid.text(x=unit(unique(x0)+0.5, "native"), y=unit(0.9, "native"),
            1:ncol, just="top", gp=gpar(fontsize=fontsize, cex=defArgs$cex.lab,
                                  fontface="bold"))
  popViewport(1)
  vp4 <- viewport(width=unit(1/(ncol+1), "npc"), height=unit(1-(1/(nrow+1)),
            "npc"), x=unit(1/(ncol+1), "npc"), y=unit(1/(nrow+1), "npc"),
            just=c("right","bottom"), xscale=c(0, 1),yscale=c(0, nrow)) #vert. text vp
  pushViewport(vp4)
  grid.text(x=unit(0.9, "native"), y=unit(unique(y0)+0.5, "native"),
            LETTERS[1:nrow], gp=gpar(fontsize=fontsize, cex=defArgs$cex.lab,
                               fontface="bold"), just="right")
  popViewport(2)
  if (!missing(main)){
    vp5 <- viewport(height=0.1, y=0.9, just="bottom") #well header vp
    pushViewport(vp5)
    grid.text(main, gp=gpar(fontsize=fontsize, cex=defArgs$cex.main,
                      fontface="bold"))
    popViewport(1)
  }#end if
  popViewport(1)
  return(list(x0=x0[wh], y0=y0[wh], wh=wh))
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


vpLocation <- function(){
  xres <- devRes()[1]
  yres <- devRes()[2]
  ## find location and pixel-size of current viewport
  devloc1 <- c(convertX(unit(0, "npc"), "inches"),
              convertY(unit(0, "npc"), "inches"), 1)  %*% current.transform()
  devloc2 <- c(convertX(unit(1, "npc"), "inches"),
              convertY(unit(1, "npc"), "inches"), 1)  %*% current.transform()
  x1 <- (devloc1/devloc1[3])[1]*xres
  y1 <- (devloc1/devloc1[3])[2]*yres
  x2 <- (devloc2/devloc2[3])[1]*xres
  y2 <- (devloc2/devloc2[3])[2]*yres
  loc <- c(x1,y1,x2,y2)
  names(loc) <- c("x1", "y1", "x2", "y2")
  size <- c(x2-x1, y2-y1)
  names(size) <- c("width", "height")
  iloc <- c(x1/xres, y1/yres, x2/yres, y2/yres)
  names(iloc) <- c("x1", "y1", "x2", "y2")
  isize <- size/c(xres,yres)
  names(size) <- c("width", "height")
  return(list(location=loc, size=size, ilocation=iloc,
              isize=isize))
}
 



















######################################################################################
##################### some example grid plotting functions ###########################
######################################################################################


.drawCircle <- function(data){
  ## draws circles with radius according to data
  if(!is.na(data))
    grid.circle(0.5, 0.5, r=max(0.1, min(data[1], 0.45)), gp=gpar(fill="red"))
  else
    grid.rect(height=0.6, width=0.6, gp=gpar(fill="gray")) 
}



.drawPie <- function(data, ...){
  ## draws pie charts for multifactorial data
  xpos <- ypos <- 0.5
  r=0.45
  col <- c(...)
  rad <- c(0, cumsum(data)/sum(data)*2)
  nredges <- 180
 
  for(i in 2:length(rad)){
    phi <- seq(rad[i-1] * pi , rad[i] * pi, len=ceiling(nredges*rad[i]))
    x <- c(xpos, r * cos(phi)+xpos, xpos)
    y <- c(ypos, r * sin(phi)+ypos, ypos)
    grid.polygon(x,y, gp=gpar(fill=col[i-1]))  
  }
}


.drawLegend <- function(col=c("red", "blue"), xrange, legend=c("act", "inh"),
                        cex.legend=1, cex.desc=1.4){
  vp3 <- viewport(height=0.85, width=0.8) #legend desc vp
  pushViewport(vp3)
  grid.text(y=c(0.95, 0.05), x=0.1, just="left", legend,
            gp=gpar(fontsize=7, cex=cex.desc,
            fontface="bold", col=c(col[length(col)], col[1])))
  vp4 <- viewport(height=0.8, width=0.1, yscale=c(xrange), xscale=c(0,1),
                  x=0.1, just="left") #legend bar vp
  pushViewport(vp4)
  nb <- 100
  cols <- colorRampPalette(col)(nb)
  i <- 1:nb
    grid.rect(y=unit(0+i/nb, "npc"), height=unit(1/nb, "npc"),
              gp=gpar(fill=cols[i], col=cols[i]), just="top")
  grid.rect(gp=gpar(fill=NA))
  at <- signif(seq(xrange[1], xrange[2], length=6)[2:5],2)
  grid.yaxis(at=at, gp=gpar(fontsize=7, cex=1), main=FALSE, label=FALSE)
  grid.text(x=unit(3.5, "native"), y=unit(at, "native"), at, rot=90,
            gp=gpar(fontsize=7, cex=cex.legend))
  popViewport(2)
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
