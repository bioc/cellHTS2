\name{templateDescriptionFile}
\alias{templateDescriptionFile}
\title{Creates a template description file for an RNAi experiment}
\description{
This function creates a template description file for an RNAi experiment with default entries compliant with MIAME class 
and with additional entries specific for a \code{\linkS4class{cellHTS}} object.
}
\usage{
  templateDescriptionFile(filename="Description.txt", path, force=FALSE)
}
\arguments{
 \item{filename}{the name of the output file. Default is "Description.txt".}
 \item{path}{a character of length 1 indicating the path in
    which to create the screen description file. 
    By default, it can extract the path from \code{filename}.}
 \item{force}{a logical value, determines the behaviour of the function
    if file \code{filename} exists.
    If \code{force} is \code{TRUE}, the function overwrites \code{filename}, otherwise it casts an error.}
}

\details{
This function can be called to generate a template file for the RNAi experiment. 
This file contains the fields that are compliant with the \code{\link[Biobase:class.MIAME]{MIAME}} class and also additional entries specific for the 
\code{\linkS4class{cellHTS}} class, which should be edited and completed by the user.

This file, which we call \emph{Screen description file}, is required to configure the \code{\linkS4class{cellHTS}} object via function \code{\link[cellHTS2:configure]{configure}}. It is intended to contain general information about the screen, such as its title, its goal, when and how it was
performed, which organism, which library, type of assay, references, and any other information that is pertinent to the
biological interpretation of the experiments. 
}

\value{
  The function returns a character with the full path and name of the file that was created.
}

\seealso{
  \code{\link[cellHTS2:configure]{configure}}
}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\examples{
  out <- templateDescriptionFile("Description.txt", path=tempdir())
  out
  readLines(out)
}
\keyword{manip}

