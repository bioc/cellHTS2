## The workhorse function for the 'Plate List' module: this is a matrix of quality metrics
## for the different plates and links to the per plate quality reports. 
writeHtml.gseaModule <- function(cellHTSList, module, gmod, outdir, con, ...)
{
    xs <- cellHTSList$scored
    tt <- getTopTable(cellHTSList, file=NULL)
    if(!is.null(xs) && state(xs)[["scored"]] && !is.null(tt$GeneID))
    {
        ## evaluate the gseaModule with scores either manualy supplied or fetched from the
        ## cellHTS object
        scores <- gmod@scores
        if(!length(scores))
        {
            scores <- as.matrix(tt$score, ncol=1)
            rownames(scores) <- tt$GeneID
            scores <- scores[!is.na(scores),]
        }
        outcome <- evalGseaModule(gmod, scores)
        stats <- outcome$stats
        stats <- stats[order(stats[,1], decreasing=TRUE),]
        rn <- rownames(stats)
        vals <- outcome$values

        ## create per category pages
        ann <- gmod@annotation
        nameCol <- match("name", tolower(names(ann)))
        setNames <- if(!is.na(nameCol)) paste(" (", ann[, nameCol], ")", sep="")
        else rep("", nrow(ann))
        od <- file.path(outdir, "gsea")
        if(!file.exists(od))
            dir.create(od, recursive=TRUE)
        sapply(seq_along(vals), function(x) perCatPage(vals[[x]], names(vals)[x], od, tt,
                                                       setNames[x]))
        

        ## create boxplots with imageMaps for all computed statistics
        imgList <- list()
        for(i in colnames(outcome$stats))
        {
            res <- makePlot(file.path(outdir, "html"), con=con, name=i,
                            w=max(7, min(12, 0.12*nrow(stats))),
                            h=4, psz=8,
                            fun=function()
                            do.call("barplotWithImageMap",
                                    args=list(values=stats[,i],
                                    tags=data.frame(title=I(sprintf("Gene Set %s%s: value=%s",
                                                    rn,
                                                    if(!is.na(nameCol)) paste(" (",
                                                                              ann[rn, nameCol], ")",
                                                                              sep="") else "",
                                                    as.character(signif(stats[,i], 2)))),
                                    href=I(sprintf("../gsea/%s.html", rn))),imgname=i)),
                            print=FALSE)
            imgList[[i]] <- chtsImage(data.frame(title=sprintf("Gene Set Statistic '%s'",i),
                                                    thumbnail=sprintf("%s.png",i), 
                                                    fullImage=sprintf("%s.pdf",i),
                                                    map=res))
        }
    
        ## Now produce the HTML output
        stack <- chtsImageStack(list(imgList), id="gseaStats")
        writeHtml.header(con)
        writeHtml(stack, con)
        writeLines(sprintf(paste("<br><br><div class=\"download\"%s><a href=\"%s\" target=\"_new\"><img",
                                 "src=\"textfileIcon.jpg\"><br>txt version</a></div>"),
                           addTooltip("downloadStatsTable"), "statsTable.txt"), con)
        stats <- signif(stats,3)
        colnames(stats) <- names(gmod@statFuns)
        stats <- data.frame(tmp0="&nbsp&nbsp&nbsp", GeneSet=rn, gmod@annotation[rn,,drop=FALSE], stats,
                            check.names=FALSE, stringsAsFactors=FALSE)
        newHeader <- c("", colnames(stats)[-1])
        stats <- rbind(newHeader, stats)
        classes <- plateListClass(stats, rep(1, nrow(stats)))
        classes[1,] <- NA
        links <- matrix(NA, nrow=nrow(stats), ncol=ncol(stats))
        links[-1,1] <- sprintf("linkToFile('../gsea/%s.html')\" %s", rn,
                               addTooltip(sprintf("Assay scores for gene set %s.", rn),
                                          fromGlossary=FALSE))
        classes[1,1] <- "sorttable_nosort"
        classes[-1,1] <- "details"
        if(length(stats) > 20000)
        {
            writeLines("<div class=\"alert\">Result table too big to render.<br>
                         Please download txt version using the link to the left.</div>", con)
        }
        else
        {
            hwrite(stats, table.class="sortable gsea", border=FALSE, center=TRUE, page=con,
                   onClick=links, class=classes, row.names=FALSE, col.names=FALSE)
        }             
        writeHtml.trailer(con)

        ## Finally we write the result table into an ASCII file
        write.table(stats[-1,-1], file=file.path(outdir, "html", "statsTable.txt"),
                    quote=FALSE, sep="\t", row.names=FALSE)
        
    }
    return(NULL)
}


## Produce a barplot with an imageMap 
barplotWithImageMap <- function(values, space=0.2, tags, imgname="barplot")
{
    checkClass(values, "numeric")
    checkClass(space, "numeric", 1)
    checkClass(tags, "data.frame")
    barplot(as.vector(values), space=space)
    usr <- par("usr")
    lr <- usr[1:2]
    ds <- dev.size()
    borders <- par("mai") 
    xr <- seq_along(values) + seq_along(values)*space
    xl <- xr-1
    psx <- diff(usr[1:2])
    psxi <- ds[1]-sum(borders[c(2,4)])
    u2ix <- psxi/psx
    psy <- diff(usr[3:4])
    psyi <- ds[2]-sum(borders[c(1,3)])
    u2iy <- psyi/psy
    mh <- psy/20
    yt <- pmax(values, mh)
    yb <- pmin(values, 0)
    xli <- u2ix*xl+borders[2]-u2ix*usr[1]
    xri <- u2ix*xr+borders[2]-u2ix*usr[1]
    ybi <- u2iy*yb+borders[1]-u2iy*usr[3]
    yti <- u2iy*yt+borders[1]-u2iy*usr[3]
    dr <- devRes()
    dsp <- dev.size("px")
    object <- cbind(xli*dr[1], dsp[2]-yti*dr[2], xri*dr[1], dsp[2]-ybi*dr[2])
    mapname <- paste("map", gsub(" |/|#", "_", imgname), sep = "_")
    outin <- sprintf("usemap=\"#%s\"><map name=\"%s\" id=\"%s\">\n", 
        mapname, mapname, mapname)
    out <- lapply(1:nrow(object), function(i)
              {
                  paste(paste("<area shape=\"rect\" coords=\"", paste(object[i, ],
                                                                      collapse=","),
                              "\"", sep=""), paste(" ", paste(colnames(tags), "=\"",
                                                              tags[i,], "\"",
                                                   sep=""), collapse=" "), " alt = \"\"/>\n",
                  sep="")
              })
    out <- paste(unlist(out), collapse = "")
    out <- paste(outin, out, "</map", sep = "")
    return(out)
}


## Create per-category pages
perCatPage <- function(scores, filename, outdir, annotation, setName)
{
    gsCol <- match("genesymbol", tolower(names(annotation)))
    giCol <- match("geneid", tolower(names(annotation)))
    geneSym <- if(!is.na(gsCol)) annotation[match(rownames(scores), annotation[,giCol]), gsCol] else NULL
    ## FIXME: Need to generalize for score matrices (or do we want that?!?)
    scores <- data.frame(GeneID=rownames(scores), GeneSymbol=geneSym, Score=scores,
                         stringsAsFactors=FALSE)
    con <- file(file.path(outdir, paste(filename, "html", sep=".")), open="w")
    on.exit(close(con))
    subt <- if(!is.na(gsCol)) paste(" (", geneSym, ")", sep="") else ""
    res <- makePlot(outdir, con=con, name=filename, w=max(7, min(12, 0.12*nrow(scores))), h=4, psz=8,
                    fun=function()
                    do.call("barplotWithImageMap",
                            args=list(values=scores$Score,
                            tags=data.frame(title=I(sprintf("GeneID %s%s: value=%s",
                                            scores$GeneID, subt,
                                            as.character(signif(scores$Score, 2))))),
                            imgname=filename)), print=FALSE)
    img <- chtsImage(data.frame(title=sprintf("Assay Scores for Gene Set '%s'", filename),
                                thumbnail=sprintf("%s.png",filename), 
                                fullImage=sprintf("%s.pdf",filename),
                                caption=setName,
                                map=res))
    writeHtml.header(con, path="../html")
    writeHtml(img, con)
    scores$Score <- signif(scores$Score,3)
    classes <- plateListClass(scores, rep(1, nrow(scores)))
    writeLines("<br><br>", con)
    hwrite(scores, table.class="sortable gsea", border=FALSE, center=TRUE, page=con, row.names=FALSE,
           class=classes)
    writeHtml.trailer(con)
}
