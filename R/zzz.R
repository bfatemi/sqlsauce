#' Internal onattach and onload
#'
#' @param libname name of library
#' @param pkgname name of package
.onAttach <- function(libname, pkgname) {
    suppressWarnings(suppressMessages(pkgname))
    suppressPackageStartupMessages(pkgname)
}

# .onLoad <- function(libname, pkgname) {
#     suppressPackageStartupMessages(libname)
#     suppressWarnings(suppressMessages(libname))
# }
