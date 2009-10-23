## Sanity checking of mandatory column names and types in a data frame or a list,
## which is expected to consist of vectors of equal length. This is supposed to
## mimick the old behaviour of data.frames to allow for arbitrary vectors as columns.
checkColumns <- function(x, name=as.character(substitute(x)), mandatory, numeric)
{
    ## one or more mandatory columns are missing
    missingColumns <- setdiff(mandatory, names(x))
    if(length(missingColumns)>0L)
        stop(paste("Column", ifelse(length(missingColumns)>1, "s "," "),
                   paste("'", missingColumns, "'", collapse=", ", sep=""),
                   ifelse(length(missingColumns)>1, " are", " is"),
                   " missing from ", name, "\n", sep=""))
    if(length(unique(listLen(x))) != 1)
        stop("This is not a data.frame-like object. All vectors must be of equal length.")
    
    ## one or more mandatory columns are not numeric
    for(j in intersect(numeric, names(x)))
    {
        if(!is.numeric(x[[j]]))
            stop(sprintf(paste("The column '%s' in file '%s' must be 'numeric', but it is",
                               "of class '%s'.\n"), j, name, class(x[[j]])))
        wna <- which(is.na(x[[j]]))
        if(length(wna)>0L)
        {
            plural <- if(length(wna)>1L) "s" else ""
            lnes <- paste(if(length(wna)>5L) paste(wna[1:5], "...") else wna, collapse=", ")
            stop(sprintf(paste("The column '%s' in file %s must not contain missing values,",
                               "but it has %d missing value%s in line%s %s\n"),
                         j, name, length(wna), plural, plural, lnes))
        }
    }
    return(TRUE)
}
