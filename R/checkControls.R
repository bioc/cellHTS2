# Auxiliary functions that perform common tests for the controls:
checkControls <- function(y, len ,name="negControls") {

   if(!is(y, "vector") | length(y)!=len | mode(y)!="character") 
      stop(sprintf("'%s' should be a vector of regular expressions with length %d", name, len))
}

#-------------------------------------------------------------------

## function to check consistency of positive controls for a two-way assay:
checkControls2W <- function(y, len, name="posControls"){
      if (length(y)!=2 || !identical(sort(names(y)), c("act", "inh")) ||
          any(sapply(y, length)!=len) ||
          any(sapply(y, mode)!="character"))#* 
        stop(cat(sprintf("'%s' should be a list with 
             two components: 'act' and 'inh'.\n These components 
             should be vectors of regular expressions with length %d \n", name, len)))
}


# ---------------------------------------------------------------------------
findControls <- function(y, anno){
if(length(y)==1) {
which(regexpr(y, anno, perl=TRUE)>0)
} else {
sapply(y, function(i) which(regexpr(i, anno, perl=TRUE)>0))
}
}

## -------------------------------------------------------------------------------
emptyOrNA <- function(y) {
y %in% c(NA, "")
}
## --------------------------------------------------------------------------------




