\name{readPlateList}
\alias{readPlateList}
\title{Read a collection of plate reader data files}
\description{
  
  Reads a collection of plate reader data files into a \code{cellHTS}
  object.  The names of the files, plus additional information (plate
  number, repeat number, assay/treatment/condition) is expected to be
  stored in a tab-delimited table specified by the argument
  \code{filename}. Alternatively, the data can be provided directly from
  non file-based sources, e.g. a data base (see details).
  
}
\usage{

readPlateList(filename, path=NA, name="anonymous", importFun,
dec=".", verbose=interactive(), ...)

}

\arguments{
  
  \item{filename}{the name of the plate list file (see details).  This
    argument is just passed on to the \code{\link{read.table}} function,
    so any of the valid argument types for the \code{file} argument of
    \code{\link{read.table}} are valid here, too. Alternatively, a
    user-defined function which is supposed to create a table-like R
    object of the plate list. Additional arguments to
    \code{readPlateList} in \dots will be passed on to this function.}

  \item{path}{a character of length 1 indicating the path in which to
    find the plate reader files. If the \code{importFun} argument is
    supplied, the value of \code{path} will not be automatically
    prepended to the file names. This has to be explicitely dealt with
    in the \code{importFun} function. See details.}

  \item{name}{a character of length 1 with the experiment name.}

  \item{importFun}{a function to read the data files.  The default
    function works for a certain file format, such as that of the
    example files provided with this package.  If your plate reader
    software produces files with a different format or if you want to
    import data from a non file-based source, the import function needs
    to be adapted. See details and examples.}

  \item{dec}{Optional argument that is passed to importFun, and can be
    used to accommodate for data files that use characters different
    from \code{.} to represent the decimal point (e.g., the comma \code{,}).}
  
  \item{verbose}{a logical value, if TRUE, the function reports some of
    its intermediate progress.}

  \item{\dots}{additional arguments that are being passed on to
    \code{filename} if it is a function.}
  
}

\details{

  The plate list is expected to be a tab-delimited file with at least
  three columns named \code{Filename}, \code{Plate}, and
  \code{Replicate}. The contents of the columns \code{Plate} and
  \code{Replicate} are expected to be integers. \code{Filename} should
  be a vector of file names of the respective raw data files. If the
  \code{path} argument is supplied and \code{importFun} is missing, its
  value will automatically be prepended to the file names. The optional
  \code{Batch} column can be used to supply batch information for an
  experiment, e.g., when a reagent has been changed or the experiment
  has been run over several days.

  Further columns are allowed, and can be used to denote, for example,
  different variants of the assay, treatments, incubation times,
  conditions, etc.

  Alternatively, the value of \code{filename} can be a user-defined
  function which creates a \code{data.frame} similar to the one
  described before. This is for instance useful if the plate list
  information is directly imported from a data base.  In order to allow
  for non-elementary data types, the output of the function can also be
  a named list, where each element has to be a vector of equal
  length. The aforementioned type restrictions still apply. The function
  will be called with all additional \dots arguments, which allows to
  pass in additional information like experiment identifiers or data
  base queries.

  \code{importFun} can be used to define a custom function to import
  data files. The \code{importFun} function should accept as its first
  argument names from the \code{Filename} column of the plate list
  (which in principle do not need to be individual files, they could
  also be handles for database entries, queries, or pointers into
  relevants parts of a file). As it second argument, the function should
  accept the value of the \code{path} argument, and the user needs to
  explicitely prepend this to the file names if needed. It should return
  a list with two components:

  \itemize{
    
    \item The first component should be a \code{data.frame} with the
    following columns

    \itemize{
  
      \item \code{well}, a character vector with the well identifier in
      the plate.

      \item \code{val}, the intensity values measured in that well.
   
    }
    
    and with as many rows as there are wells in the plate.

    \item The second component should be a character vector containing a
    copy of the imported input data file (such as the output of
    \code{\link[base:readLines]{readLines}}).  It should be suitable to
    be used as input for \code{\link[base:writeLines]{writeLines}},
    since it will be used to reproduce the intensity files that are
    linked in the HTML quality reports generated by
    \code{\link[cellHTS2:writeReport]{writeReport}}.
  
  }
 
  For example, to import plate data files from EnVision plate reader,
  set \code{importFun=getEnVisionRawData} or
  \code{importFun=getEnvisionCrosstalkCorrectedData}.  See function
  \code{\link[cellHTS2]{getEnVisionRawData}}.

  Direct data base import without the need for any flat files at all
  could for instance be archived by:

  \itemize{

    \item{Providing a user-defined function as the \code{filename}
      argument and an experiment identifier as an additional \dots
      argument. The function would have to query the data base for the
      plate information using this identifier and return a table as
      described above, where the \code{Filename} column contains
      identifiers needed to fetch the respective raw data for a single
      plate in a subsequent query. Alternatively, this could be a data
      base handle, or the query itself.}

    \item{Providing a second user-defined function as the
      \code{importFun} argument, which takes the value of the
      \code{Filename} column for a single plate and retrieves the
      respective raw data from the data base.}

  }
      
}

\value{
  
  An object of class \code{\linkS4class{cellHTS}}, which extends the
  class \code{\link[Biobase:class.NChannelSet]{NChannelSet}}.
  
  After calling this function, the content of the following slots is as
  follows:
  
  \item{assayData}{an object of class
    \code{\link[Biobase:class.AssayData]{AssayData}} containing the
    imported measurement data. Each matrix represents a single channel,
    and each run corresponds to a column. Thus, the total number of rows
    in each matrix corresponds to the product between the number of
    wells per plate and the number of assay plates.}
  
  \item{phenoData}{information about the runs, inferred from the
    \code{plateList} file: which replicate, which variant of the assay,
    treatment, incubation times etc.}
  
  \item{featureData}{the information about the plate and well
    identifiers for each plate measurement are stored in columns
    \code{plate} and \code{well} of this slot.}
  
  \item{plateList}{a data.frame containing what was read from input file
    \code{x}, plus a column \code{status} of type character: it contains
    the string "OK" if the data import appeared to have gone well, and
    the respective error or warning message otherwise.}
  
  \item{intensityFiles}{a list, where each component contains a copy of
    the imported input data files. Its length corresponds to the number
    of rows of \code{plateList}.}
  
}

\seealso{
  To read input files obtained in a HTanalyser plate reader, see
  \code{\link{readHTAnalystData}}. To build a cellHTS2 object from a
  data frame, see \code{\link{buildCellHTS2}}.
}

\author{

  W. Huber \email{huber@ebi.ac.uk}, Ligia Bras \email{ligia@ebi.ac.uk},
  Florian Hahne \email{florian.hahne@novartis.com}

}

\references{
  
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi
screens, \emph{Genome Biology} \bold{7}, R66.

}


\examples{

datadir <- system.file("KcViabSmall", package = "cellHTS2")
x <- readPlateList("Platelist.txt", "KcViabSmall", path=datadir)

## To read data files obtained from an EnVision plate reader:
datadir <- system.file("EnVisionExample", package = "cellHTS2")
x <- readPlateList("platelist.txt", name="EnVisionEx",
          importFun=getEnVisionRawData, path=datadir)

## to get the cross talk corrected data:
y <- readPlateList("platelist.txt", name="EnVisionEx",
          importFun=getEnVisionCrosstalkCorrectedData, path=datadir)

}

\keyword{manip}
