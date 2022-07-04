#' Add directory of static resources to Shiny's web server
#'
# The function `addResourcePath()` makes static files in 'assets' available to app components.
#'
#' @param libname A character vector giving the library directory where the package defining the namespace was found.
#' @param pkgname A character svector giving the name of the package.
#'
#' @importFrom shiny addResourcePath
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ElectProtect!")
  addResourcePath("assets", system.file("assets", package = "ElectProtect"))
}
