checkColumns = function(x, name, mandatory, numeric) {
  
  missingColumns = setdiff(mandatory, colnames(x))
  if(length(missingColumns)>0)
    stop(paste("Column", ifelse(length(missingColumns)>1, "s "," "),
               paste(missingColumns, collapse=", "),
               ifelse(length(missingColumns)>1, " are"," is"),
               " missing from ", name, "\n"))
  
  for(j in intersect(numeric, colnames(x)))
    if(!is.numeric(x[,j]) || any(is.na(x[,j])))
      stop(sprintf("Column %s in %s must be numeric and not contain missing values.\n",
                   j, name))

  return(TRUE)
}
