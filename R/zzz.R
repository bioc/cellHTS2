.onLoad <- function(lib, pkg) {
  ## cat("Type citation('cellHTS2') for how to cite cellHTS2.")
}

.onAttach <- function(libname, pkgname) {
   ## set up menus -- windows only for now
   if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" )
      addVigs2WinMenu("cellHTS2") ## in Biobase
}
