\name{bdgpbiomart}
\alias{bdgpbiomart}
\non_function{}
\title{Dataset with annotation of CG identifiers}
\usage{data(bdgpbiomart)}
\format{
Dataframe with 21888 rows and 11 columns
\code{plate}, \code{well}, \code{controlStatus}, \code{HFAID}, \code{GeneID},
\code{chromosome_name}, \code{start_position}, \code{end_position}, \code{description},
\code{flybasename_gene}, \code{go}, \code{go_description}.
}
\description{
See the vignette \emph{End-to-end analysis of 
cell-based screens: from raw intensity readings 
to the annotated hit list}, 
Section \emph{Using biomaRt to annotate the target
genes online} for details. The annotations were obtained on
21 September 2007.
}
\source{
BioMart webinterface to Ensembl.
}
\examples{
    data("bdgpbiomart")
}
\keyword{datasets}
