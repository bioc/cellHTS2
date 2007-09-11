## Function to convert between the different ways of specifying well coordinate, e.g.:
##   "B02" ->    c("B", "02")  ->     26

convertWellCoordinates <- function(x, pdim){

if(!is.character(x) & !is.integer(x)) stop("'x' must be either a character vector with alphanumeric well IDs (e.g. 'B03' or c('B', '03'))\n or a vector of integers with position IDs within a plate (e.g. 27).")

if(!all(names(pdim) %in% c("nrow", "ncol")) | length(pdim)!=2 | !is.integer(pdim)) stop("'pdim' should be a vector of length 2 with names 'nrow' and 'ncol' giving the number of rows
    and columns in a plate (integer values). E.g. set it to 'c(nrow=16L, ncol=24L)' for 384-well plates.")

# see which is the type of the well ID:
if(is.integer(x)){
   num=TRUE

} else {
num=FALSE

# put as alphanumeric vector:
if(is.matrix(x)) x = apply(x, 1, paste, collapse="") 

if (length(x)==2)  {
    if(nchar(x)[1]==1) { 
      if(!(x[1] %in% LETTERS[1:pdim[["nrow"]]]) & !(nchar(x[2])==2)) 
        stop("Invalid position IDs in 'x'.") 

    x = paste(x, collapse="") 
    } 
 }

if(any(nchar(x)!=3))
    stop("Well IDs must contain 1 letter and 2 digits. E.g., c('A', '02') or c('A02').")
}



if(num) {

  num = x
# 'x' contains numeric position IDs that should be converted into alphanumeric well IDs:
 if(!all(x %in% 1:prod(pdim))) 
   stop("Invalid position IDs in 'x'.")

 row <- 1 +(x-1) %/% pdim[["ncol"]]
 col <- 1 + (x-1) %% pdim[["ncol"]]
 letnum <- sprintf("%s%02d",LETTERS[row], col) 
 let.num <- cbind(substr(letnum,1,1), substr(letnum, 2,3)) #LETTERS[row], col)
} else {
# convert from alphanumeric well ID to numeric well ID (positions):
  letnum=x
  let = substr(x, 1, 1)
  num = substr(x, 2, 3)
  let.num= cbind(let, num)
  let = match(let, LETTERS)
  num = as.integer(num)
  inv = is.na(let) | (let>pdim[["nrow"]]) | is.na(num) | (num>pdim[["ncol"]])
  if(any(inv))
    stop("Invalid position IDs in 'x'.")
  num <- (let-1)*pdim[["ncol"]]+num

} 

return(out=list(letnum=letnum, let.num=let.num, num=num))
}
