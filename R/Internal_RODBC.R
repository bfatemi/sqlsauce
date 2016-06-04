#' Internal Connection Functions
#'
#' These are functions that are not exposed to users. They are custom wrappers around connection functions from
#' the package RODBC. Additionally, they provide internal functionality to build connection strings, set and get
#' connection attributes, add connections to and clean up the connection pool, etc. These are abstracted from users/developers
#' in order to enable developers to efficiently build data products.

#' @section Internal Use Only:
#'  Internal functions not intended to be used by developers. To open and close connections to a preconfigured database,
#'  use: \code{\link{OpenDB}} or \code{\link{CloseDB}}.
#'
#' @seealso RODBC Vignette: \url{https://cran.r-project.org/web/packages/RODBC/RODBC.pdf}
#'
#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @return Object of class RODBC or NULL in the case of .odbcClose()
#' @export
.odbcOpen <- function(db){
    nFrame <- sys.nframe()
    tryCatch({
        return(RODBC::odbcDriverConnect(ConnString(db)))
    }, error = function(e){
        Clean(db)
        DebugInfo("Check network or access", nFrame)
    })
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
.odbcClose <- function(db) {
    cnObj <- GetConn(db)
    nFrame <- sys.nframe()
    tryCatch({
        RODBC::odbcClose(cnObj)
    }, error=function(e){
        Clean(db)
        DebugInfo("Could not close. Cleaning conn environment", nFrame)
    })
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
.odbcReOpen <- function(db) {
    cnObj <- GetConn(db)
    nFrame <- sys.nframe()
    tryCatch({
        return(RODBC::odbcReConnect(cnObj))
    }, error = function(e){
        Clean(db)
        DebugInfo("Check network or access", nFrame)
    })
}


