## Ligia Brás, August 2006
## Given as input an array (e.g. rawdata(object) or normdata(object)) with dimensions wells x plates x replicates x channels, this function creates a matrix with the data of each replicate of a chosen "channel" in columns. By default, the dimension of the output matrix is (wells*plates) x replicates, and the first channel of 'y' is considered.
## if 'na.rm' is set to TRUE, the rows with at least one entry missing are removed, so that the output matrix have no missing entries.

getReplicatesMatrix = function(y, channel=1, na.rm=FALSE) {
 ## Check y
 if(!is(y, "array") | length(dim(y))!=4)
   stop("'y' must be an array with four dimensions, such as rawdata(object) or normdata(object).") 

 if ((length(channel)>1) | (channel > dim(y)[4]))
   stop(sprintf("'channel' can only be one of the value(s) %s \n indicating which of the channels of 'y' should be considered", paste(1:dim(y)[4], collapse=", ")))

 xmat <- matrix(y[,,,channel], nrow=prod(dim(y)[1:2]), ncol=dim(y)[3])
 if(na.rm) {
   rem <- rowSums(is.na(xmat))>0
   xmat = xmat[!rem,] 
 }
 return(xmat)
}
