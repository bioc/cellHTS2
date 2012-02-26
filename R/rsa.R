## Perform RSA summarization on a scored cellHTS object

## -----------------------------------------------------------------------------------
## This has been taken from the original RSA R implementation and slightly modified
handleOneGroup <- function(i, dataset, lowerBound=0, upperBound=1, reverse=FALSE) 
{
    if(reverse)
    {
        i_max <- sum(dataset$Score[i]>=lowerBound)
        i_min <- max(1, sum(dataset$Score[i]>=upperBound))
    }else{
        i_max <- sum(dataset$Score[i]<=upperBound)
        i_min <- max(1, sum(dataset$Score[i]<=lowerBound))
    }
    r <- OPIScore(i, nrow(dataset), i_min, i_max)
    return(cbind(LogP=r["logp"],
                 OPI_Hit=as.numeric(seq(length(i))<=r["cutoff"]),
                 "#hitWell"=i_max,
                 "#totalWell"=length(i),
                 rank = i))
}

OPIScore <- function(I_rank, N, i_min=1, i_max=-1)
{
    n_drawn <- length(I_rank) # number of black
    if(i_max == -1)
    	i_max <- n_drawn
    r1 <- c(logp=1.0,cutoff=0)
    if(i_max < i_min)
        return (r1)
    ## phyper(x, lower.tail = F), x = x-1, when lower.tail = F
    logp <- apply(cbind(seq(i_min,i_max),I_rank[i_min:i_max]), 1, function(x){
        phyper(x[1]-1,x[2] ,N-x[2], n_drawn,lower.tail = F,log.p=T)})
    logp <- logp/log(10)
    logp[logp<(-100)] <- -100
    if(all(is.na(logp))) return(r1) else return(c(logp=min(logp),cutoff = i_min-1+which.min(logp)))
}

OPI <- function(Groups, Scores, lowerBound=0, upperBound=1, reverse=FALSE, Data=NULL)
{
    t <- data.frame(Gene_ID=Groups, Score=Scores, stringsAsFactors=FALSE)
    Sorted_Order <- order(t$Score,decreasing=reverse);
    Data <- Data[Sorted_Order,]
    t <- t[Sorted_Order,]
    t <- do.call("rbind", tapply(seq(nrow(t)), list(t$Gene_ID), handleOneGroup, dataset=t, lowerBound=lowerBound,
                                 upperBound=upperBound, reverse=reverse))
    t <- cbind(Data, t[order(t[,"rank"]),])

    ## add OPI_Rank
    t <- t[order(t$LogP, t$Score*ifelse(reverse, -1, 1)),]
    t$OPI_Rank <- cumsum(t$OPI_Hit)
    t$OPI_Rank[t$OPI_Hit == 0] <- 999999
	
    ## add Cutoff_Rank
    t <- t[order(t$Score*(ifelse(reverse,- 1, 1)), t$LogP),]
	
    tmp <- if(reverse) t$Score>=lowerBound else t$Score<=upperBound
    t$Cutoff_Rank <- cumsum(tmp)
    t$Cutoff_Rank[!tmp] <- 999999
	
    ## add EXP_Rank
    t$EXP_Rank <- pmin(t$OPI_Rank,t$Cutoff_Rank)
    t$EXP_Rank <- pmin(t$OPI_Rank,t$Cutoff_Rank)
    if(reverse) {
        return(t[order(t$OPI_Rank, -t$Score),])
    } else {
        return(t[order(t$OPI_Rank, t$Score),])
    }
}	
## -----------------------------------------------------------------------------------


rsa <- function(x, geneColumn="GeneID", lowerBound=0, upperBound=1, reverse=FALSE, drop=FALSE)
{
    if(!is(x, "cellHTS"))
        stop("'x' needs to be of class 'cellHTS'")
    if(length(channelNames(x))>1)
        stop("The input cellHTS object needs to be single channel.\nConsider calling summarizeChannels(",
             substitute(x), ") first")
    if(!state(x)["scored"])
        stop("The input cellHTS object must be scored.\nConsider calling scoreReplicates(",
             substitute(x), ") first.")
    if(dim(x)[2]>1)
        stop("The input cellHTS object must be summarized.\nConsider calling summarizeReplicates(",
             substitute(x), ") first.")
    if(!state(x)["annotated"])
        stop("The input cellHTS object must be scored.\nConsider calling annotate(",
             substitute(x), ") first.")

    resData <- getTopTable(list(scored=x), file=NULL, verbose=FALSE)
    if(!geneColumn %in% colnames(resData))
        stop("Can't find the gene identifier column '", geneColumn, "'.\n",
             "Available columns are: ", paste(setdiff(colnames(resData),
                                                       c("plate", "position", "well", "score",
                                                         "wellAnno", "finalWellAnno")), collapse=", "))
    inTab <- data.frame("Gene_ID"=resData[,geneColumn], "Well_ID"=paste(resData$plate, resData$well, sep="_"),
                   Score=resData$score, stringsAsFactors=FALSE)
    ## For missing GeneIDs we fake a grouping identifier
    if(!drop)
    {
        nasel <- is.na(inTab$Gene_ID) | inTab$Gene_ID==""
        inTab[nasel, "Gene_ID"] <- make.unique(rep("NA", sum(nasel)))
    } else {
        nasel <- FALSE
    }
    t <- subset(inTab, !is.na(inTab$Gene_ID) & inTab$Gene_ID != "" & !is.na(inTab$Score))
    r <- OPI(Groups=t$Gene_ID, Scores=t$Score, lowerBound=lowerBound, upperBound=upperBound, reverse=reverse, Data=t)
    ## Some cleaning up and output of a saner data.frame than what we get from RSA directly...
    results <- as.data.frame(matrix(ncol=11, nrow=nrow(inTab), dimnames=list(inTab$Well_ID,
                                                                            c(geneColumn, "Plate", "Well", "Score",
                                                                              "RSARank", "ScoreRank", "PValue", "RSAHit",
                                                                              "#HitWell", "#TotalWell", "%HitWell"))),
                             stringsAsFactors=FALSE)
    r[r$Gene_ID %in% inTab[nasel, "Gene_ID"], "Gene_ID"] <- NA
    r[r$OPI_Hit==0, c("OPI_Rank", "Cutoff_Rank")] <- sum(r$OPI_Hit==1)+1
    results[r$Well_ID,] <- data.frame(r$Gene_ID, t(sapply(strsplit(r$Well_ID, "_"), c)), r[, c("Score", "OPI_Rank", "Cutoff_Rank")],
                                 pmin(1, 10^r$LogP), r[, c("OPI_Hit", "#hitWell", "#totalWell")],
                                 signif((r$"#hitWell"/r$"#totalWell")*100, 2), stringsAsFactors=FALSE)
    results <- results[order(results$RSARank, results$Score),]
    if(drop)
        results <- results[!is.na(results$Score),]
    rownames(results) <- NULL
    return(results)
}



