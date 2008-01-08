##------------------------------------------------------------------------------
## Function to create an empty Description file with default entries (compliant with MIAME class and with additional entries specific for an RNAi experiment
##------------------------------------------------------------------------------

templateDescriptionFile <- function(filename="Description.txt", path, force=FALSE) {
# If 'path' is given, check if it exists. If not, create it. If yes, check if the 'filename' file already exists and return an error message if yes.
if (!missing(path)) {
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")
} else {
  path=dirname(filename)
}

  file = basename(filename)
 
  f = file.path(path, file)

  if(file.exists(path)){
    if(file.exists(f)) 
          if(!force) stop(sprintf("'%s' already exists!", f))
  } else {
    dir.create(path, recursive=TRUE)
  }

desc=c("[Lab description]",
"Experimenter name: <put here the experimenter name>",
"Laboratory: <put here the name of the laboratory where the experiment was conducted>",
"Contact information: <put here contact information for lab and/or experimenter>",
"",
"[Screen description]",
 "Screen: <put here the screen name>",
 "Title: <put there the single-sentence giving the experiment title>",
 "Version: <put here the screen version>",
 "Date: <put here the date when the experiment was performed>",
 "Screentype: <put here the type of screen>",
 "Organism:",
 "Celltype:",
 "Library:",
 "Assay: <put here the name of the assay>",
 "Assaytype: <put here the type of assay>",
 "Assaydescription: <put here the description of the assay>",
 "",
 "[Publication description]",
 "Publicationtitle:",
 "Reference:",
 "PMIDs: <put here the PubMed identifiers of papers relevant to the dataset>",
 "URL: <put here the URL for the experiment>",
 "License:",
 "Abstract: <put here the abstract describing the experiment>",
 "",
 "[Files]",
 "plateList: <put the name of the plate result list file>",
 "annotation: <put the name of the screen library annotation file>",
 "plateConf: <put the name of the screen plate configuration file>",
 "screenLog: <put the name of screen log file, if available>"
)

 writeLines(desc, f)
 return(f)
}
#--------------------------------------------------------
