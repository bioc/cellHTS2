.onLoad <- function(lib, pkg) {
  ## cat("Type citation('cellHTS2') for how to cite cellHTS2.")
}

.onAttach <- function(libname, pkgname) {
    msg <- sprintf(
        "Package '%s' is deprecated and will be removed from Bioconductor version %s.", pkgname, "3.20") |>
      paste("We recommend using tidy data structures, dplyr and its aggretation functions, and ggplot2 instead.")
    .Deprecated(msg = paste(strwrap(msg, exdent=2), collapse="\n"))
   ## set up menus -- windows only for now
   if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" )
      addVigs2WinMenu("cellHTS2") ## in Biobase
}
