## Function to convert between the different ways of specifying well coordinate, e.g.:
##   "B02" ->    c("B", "02")  ->     26

convertWellCoordinates <- function(x, pdim, type="384"){

  if(!missing(pdim)){
    if(!missing(type))
      stop("Please specify either 'pdim' or 'type' but not both.")

    storage.mode(pdim) = "integer"
    
    if(!(all(names(pdim) %in% c("nrow", "ncol")) && (length(names(pdim))==2L)))
      stop("'pdim' should be a vector of length 2 with names 'nrow' and 'ncol'.")
    if(any(is.na(pdim)))
      stop("'pdim' contains invalid values: %s", paste(as.character(pdim),
                                                       collapse="\n"))
  } else {
    if(!(is.character(type)&&(length(type)==1L)))
      stop("'type' must be a character of length 1.")
    pdim = switch(type,
      "24"  = c(nrow= 4L, ncol= 6L),
      "96"  = c(nrow= 8L, ncol=12L),
      "384" = c(nrow=16L, ncol=24L),
      stop("Invalid 'type': %s", type))
  }
    

  if(is.character(x)) {
    ## x is of the form A01, B13, ... 
    if(is.matrix(x))
      x = apply(x, 1L, paste, collapse="") 

    ## Add a zero if necessary
    s = (nchar(x)==2L)
    x[s] = paste(substr(x[s], 1L, 1L), "0", substr(x[s], 2L, 2L), sep="")
    
    if(any(nchar(x)!=3L))
      stop("Elements of 'x' must have length 3: 1 letter and 2 digits.")

    let = substr(x, 1L, 1L)
    num = substr(x, 2L, 3L)
    let.num = cbind(let, num)
    letnum = x

    
    irow = match(let, LETTERS)
    icol = as.integer(num)
    if( any(is.na(irow)) || any(irow>pdim["nrow"]) || any(irow<1L) ||
        any(is.na(icol)) || any(icol>pdim["ncol"]) || any(icol<1L) )
      stop("Invalid position IDs in 'x'.")
    num = (irow-1L) * pdim["ncol"] + icol
    
  } else if(is.numeric(x)) {
    ## x is of the form 1, 14, 18, ...
    num = as.integer(x)
    if(any(num<1L)||any(num>prod(pdim))) 
      stop(sprintf("Invalid values in 'x', must be between 1 and %d.", prod(pdim)))

    irow = 1L + (num-1L) %/% pdim["ncol"]
    icol = 1L + (num-1L) %%  pdim["ncol"]
    let.num = cbind(LETTERS[irow], sprintf("%02d", icol))
    letnum  = apply(let.num, 1L, paste, collapse="")

  } else if(!length(x)){
      letnum <- let.num <- num <- NULL
  }else{
    stop("'x' must be either a character vector with alphanumeric well IDs ",
         "(e.g. 'B03' or c('B', '03'))\n or a vector of integers with position ",
         "IDs within a plate (e.g. 27).")
  }

  return(list(letnum = letnum, let.num = let.num, num = num))
}
