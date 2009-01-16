## Check the colums 
## returns TRUE if all the steps are successful. Stop the computation otherwise.
checkColumns = function(x, name, mandatory, numeric) {  
	missingColumns = setdiff(mandatory, colnames(x))
	if(length(missingColumns)>0L){
		stop(paste("Column", ifelse(length(missingColumns)>1, "s "," "),paste("'",missingColumns,"'",collapse=", ",sep=""),ifelse(length(missingColumns)>1," are"," is")," missing from ",name,"\n", sep=""))
	}
	for(j in intersect(numeric, colnames(x))) {
		if(!is.numeric(x[,j])) {
			stop(sprintf("The column '%s' in file %s must be 'numeric', but it is of class '%s'.\n",j,name,class(x[,j])))
		}
		wna = which(is.na(x[,j]))
		if(length(wna)>0L) {
			plural = if(length(wna)>1L) "s" else ""
			lnes = paste(if(length(wna)>5L) paste(wna[1:5], "...") else wna, collapse=", ")
			stop(sprintf("The column '%s' in file %s must not contain missing values, but it has %d missing value%s in line%s %s\n", j, name, length(wna), plural, plural, lnes))
		}
	}  
	return(TRUE)
}
