\name{annotate}
\docType{methods}
\alias{annotate}
\alias{annotate,cellHTS-method}
\title{Annotates the reagents (probes) of a cellHTS object}
\description{
  Annotate the reagents (probes) of a \code{\linkS4class{cellHTS}}
  object. In RNAi-screens, there is a often a 1:1 correspondence
  between reagents and intended target genes, hence in this
  software package the term gene ID is used as a synonym.
}

\usage{
\S4method{annotate}{cellHTS}(object, geneIDFile, path)
}

\arguments{
  
  \item{object}{a \code{\linkS4class{cellHTS}} object.}
  
  \item{geneIDFile}{the name of the file with the gene IDs (see
    details).  This argument is just passed on to the
    \code{\link{read.table}} function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too. Must contain
    one row for each well in each plate.}
  
  \item{path}{a character of length 1 indicating the path in which to
    find the gene annotation file (optional).}
  
}

\details{

  \describe{
    
    \item{geneIDFile}{This file is expected to be a tab-delimited file
      with at least three columns, and column names \code{Plate},
      \code{Well} and \code{GeneID}. The contents of \code{Plate} are
      expected to be integer. Further columns are allowed.}

  }
}

\value{

  An S4 object of class \code{\linkS4class{cellHTS}}, which is obtained
  by copying \code{object} and updating the following slots:

  \item{featureData}{the contents of the annotation file are stored
    here.}
    
  \item{state}{the processing status of the \code{cellHTS}
    object is updated to \code{state["annotated"]= TRUE}.}
  
}

\seealso{
  \code{\link[cellHTS2:readPlateList]{readPlateList}},
  \code{\link[cellHTS2:configure]{configure}}
}

\author{Wolfgang Huber, Ligia Bras}

\references{
  
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi
screens, \emph{Genome Biology} \bold{7}, R66.

}

\examples{

    datadir <- system.file("KcViabSmall", package = "cellHTS2")
    x <- readPlateList("Platelist.txt", path=datadir, name="KcViabSmall")
    x <- configure(x, "Description.txt", "Plateconf.txt", "Screenlog.txt", path=datadir)
    x <- annotate(x, "GeneIDs_Dm_HFAsubset_1.1.txt", path=datadir)

}

