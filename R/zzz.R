#' Title
#'
#' @param libname
#' @param pkgname
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to my package")
}

# .onLoad <- function(libname, pkgname) {
#     suppressPackageStartupMessages(libname)
#     suppressWarnings(suppressMessages(libname))
# }
