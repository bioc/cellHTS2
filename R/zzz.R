.onLoad <- function(lib, pkg) {
  ## load methods package
  require("methods")
  ## pDesc = packageDescription("cellHTS")
  ## cat(sprintf("\n\n\t\tcellHTS %s, %s\nPlease check at www.bioconductor.org for the most recent version.\n\n",
  ## pDesc$Version, pDesc$Date))
}

.onAttach <- function(libname, pkgname) {
   ##set up menus -- windows only for now
   if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" )
      addVigs2WinMenu("cellHTS") ## in Biobase
}
