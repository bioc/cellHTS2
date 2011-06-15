\name{rsa}
\alias{rsa}

\title{Perform RSA ranking on the screening results.}

\description{
The RSA method ranks the resulting hit list of a screening experiment,
taking into account the design of the screening library (i.e., multiple
probes targeting the same effector molecule).
}

\usage{
rsa(x, geneColumn="GeneID", lowerBound=0, upperBound=1, reverse=FALSE, drop=FALSE)
}

\arguments{
  \item{x}{Object derived from class \code{\linkS4class{cellHTS}}.}

  \item{geneColumn}{The name of the well annotation column to be used
    for the grouping of effector molecules and probes.}

  \item{lowerBound}{The lower boundary parameter for the RSA algorithm.}

  \item{upperBound}{The upper boundary parameter for the RSA algorithm.}

  \item{reverse}{Boolean. Reverse the ranking.}

  \item{drop}{Boolean. Drop all probes from the analysis for which no
    effector molecule is defined.}
  
}

\details{
  
  The input argument \code{x} has to be a \code{cellHTS2} object which
  has been scored, summarized and annotated. For details on the RSA
  algorithm please see the publication referenced below.
  
}

\value{
  
  A data.frame with the following columns:

  \item{Value of argument \code{geneColumn}:}{the target molecule
    identifier.}

  \item{Plate:}{the plate identifier.}

  \item{Well:}{the well identifier.}

  \item{Score:}{the probe score in the screen.}

  \item{RSARank:}{the computed RSA rank.}

  \item{ScoreRank:}{the rank based on a simple cutoff scheme.}

  \item{PValue:}{the computed RSA $p$-value.}

  \item{RSAHit:}{the RSA hit flag.}

  \item{#HitWell:}{the number of probes counted as positive RSA hits for
    a given target molecule.}

  \item{#TotalWell:}{the total number of probes for a given target molecule.}

  \item{\%HitWell:}{the percentage of postive hits for a given molecule.}
    
}

\author{Florian Hahne \email{florian.hahne@novartis.com}}

\references{
  
Renate Koenig, Chih-yuan Chiang, Buu P Tu, S Frank Yan, Paul D DeJesus,
Angelica Romero, Tobias Bergauer, Anthony Orth, Ute Krueger, Yingyao
Zhou & Sumit K Chanda: A probability-based approach for the analysis of
large-scale RNAi screens

\emph{NATURE METHODS | VOL.4 NO.10 | OCTOBER 2007 | 847}
}


\examples{

data(KcViabSmall)
KcViabSmall <- scoreReplicates(KcViabSmall, sign="-", method="zscore")
KcViabSmall <- summarizeReplicates(KcViabSmall, summary="mean")
ranking <- rsa(KcViabSmall)
head(ranking)

}

\seealso{
\code{\linkS4class{cellHTS}}
}

\keyword{manip}
