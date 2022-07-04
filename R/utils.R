#' Remove Underscore and Convert to Title Case
#'
#' Function for formatting column names for display.
#'
#' @param str A length-one character vector to be formatted.
#'
#' @importFrom stringi stri_trans_totitle stri_replace_all_regex
#'
#' @return A length-one character vector.
#'
#' @export
str_format <- function(str) {
  stri_trans_totitle(stri_replace_all_regex(str, pattern = "_", replacement = " "))
}


#' Function for Accesses Files in the Current Package
#'
#' @param ... Character vectors, specifying subdirectory and file(s) within the package.
#'
#' @export
app_sys <- function(...) {
  system.file(..., package = "ElectProtect")
}
