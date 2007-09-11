# Auxiliary functions that perform common tests for the controls:
## -------------------------------------------------------------------------------
checkControls <- function(y, len ,name="negControls") {

   if(!is(y, "vector") | length(y)!=len | mode(y)!="character") 
      stop(sprintf("'%s' should be a vector of regular expressions with length %d", name, len))
}
## -------------------------------------------------------------------------------

## -------------------------------------------------------------------------------
emptyOrNA <- function(y) {
y %in% c(NA, "")
}
## --------------------------------------------------------------------------------


# ---------------------------------------------------------------------------
findControls <- function(y, anno){
if(length(y)==1) {
which(regexpr(y, anno, perl=TRUE)>0)
} else {
sapply(y, function(i) which(regexpr(i, anno, perl=TRUE)>0))
}
}
## --------------------------------------------------------------------------------
