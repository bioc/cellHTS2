\name{annotate}
\docType{methods}
\alias{annotate}
\alias{annotate,cellHTS-method}
\title{Annotates the gene IDs of a given cellHTS object}
\description{
  Annotate the gene IDs of a given \code{\linkS4class{cellHTS}} object.  
}

\usage{
\S4method{annotate}{cellHTS}(object, geneIDFile, path, \dots)
}

\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object.}
  \item{geneIDFile}{the name of the file with the gene IDs (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too. Must contain one row for each well and each plate.}
  \item{path}{a character of length 1 indicating the path in
    which to find the gene annotation file. By default, 
    it can extract the path from \code{geneIDFile}.}
  \item{\dots}{additional parameters - ignored.}
}
\details{
  \item{geneIDFile}{This file is expected to be a tab-delimited file with at least three columns, and column names \code{Plate}, \code{Well} and \code{GeneID}. The contents of \code{Plate} are expected to be integer. Further columns are allowed.}
}

\value{An S4 object of class \code{\linkS4class{cellHTS}}, which extends the argument \code{object} by the following element:
  \item{geneAnno}{a \code{\link{data.frame}} containing what was read from input file \code{geneIDFile}. 
  The number of rows is equal to the product between the number of wells in each plate and the number of plates.}

Moreover, the processing status of the \code{cellHTS} object is updated in the slot \code{state} to \code{state["annotated"]= TRUE}.
There are methods \code{show}, \code{configure}, \code{annotate}, \code{writeTab} and \code{scores}.}

\seealso{
  \code{\link[cellHTS2:readPlateData]{readPlateData}},
  \code{\link[cellHTS2:configure]{configure}}
}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\examples{

    datadir <- system.file("KcViabSmall", package = "cellHTS2")
    x <- readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
    x <- configure(x, "Plateconf.txt", "Screenlog.txt", "Description.txt", path=datadir)
    x <- annotate(x, "GeneIDs_Dm_HFAsubset_1.1.txt", path=datadir)

}
\keyword{methods}
