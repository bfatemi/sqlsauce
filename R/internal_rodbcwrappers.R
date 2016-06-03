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
        odbcClose(cnObj)
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

#' @export
cn.env <- new.env(parent = emptyenv())

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
GetConn <- function(db){
    if(exists("cn.env", mode="environment")){
        if(exists(paste0("cn.", db), cn.env)){
            get(paste0("cn.", db), cn.env)
        }
    }else{
        assign("cn.env", new.env(parent = as.environment("package:sqlsauce")))
    }
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
SetConn <- function(object, name,
                    status     = NULL,
                    openedby   = NULL,
                    reopenedby = NULL,
                    tOpen      = NULL,
                    request    = NULL,
                    tRequest   = NULL,
                    closedby   = NULL,
                    tClose     = NULL,
                    count      = NULL){
    if(missing(object) | missing(name))
        stop("Missing connection object or name argument")

    ### Set object attributes
    attr(object, "Status")        <- status
    attr(object, "OpenedBy")      <- "Global" #openedby
    attr(object, "ReOpenedBy")    <- "Global" #reopenedby
    attr(object, "TimeOpened")    <- tOpen
    attr(object, "RequestedBy")   <- "Global" #request
    attr(object, "TimeRequested") <- tRequest
    attr(object, "ClosedBy")      <- "Global" #closedby
    attr(object, "TimeClosed")    <- tClose
    attr(object, "AccessCount")   <- count

    # Assign to connection environment
    assign(name, object, envir = cn.env)
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
CleanAll <- function(){
    RODBC::odbcCloseAll()
    rm(cn.env, envir = parent.env())
    return(1)
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
Clean <- function(db){
    cnName <- paste0("cn.",db)
    # cnObj <- GetConn(db)
    # .odbcClose(cnObj)
    rm(list = cnName, envir = cn.env)
    return(1)
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
CheckDB <- function(db=NULL){
    if(is.null(db))
        stop("No database specified to connect to")
    valid_DBs <- sapply(Databases(),"[[","database")

    tryCatch({
        as.character(match.arg(db, valid_DBs))
    }, error = function(e){
        stop(paste("Check name! Selected database must be one of:",
                   paste(valid_DBs,collapse=", ")), call. = F)
    })
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
ConnString <- function(db=NULL){
    cnstr <- substitute(paste0("Driver=",    driver,
                               ";Server=",   server,
                               ";database=", database, usr, pw))
    eval(cnstr, get(paste0("ll_", db), Databases()))
}

#' @describeIn InternalConnHandling An internal function to used by connection functions that provide
#'      all the information used to build the connection string. This is where new connections will be
#'      configured
#' @export
Databases <- function(){
    ll_all <- list(
        ll_Morpheus =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "Morpheus",
                 usr      = ";trusted_connection=true",
                 pw       = NULL),
        ll_MorpheusDev =
            list(driver   = "SQL Server",
                 server   = "sccdsql01.dv.local,1515",
                 database = "Morpheus",
                 usr      = ";uid=MorpheusAdmin",
                 pw       = ";pwd=MorpheusAdmin2015"),
        ll_RemoteFE =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "RemoteFE",
                 usr      = ";trusted_connection=true",
                 pw       = NULL),
        ll_ProcedureCategorization =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "ProcedureCategorization",
                 usr      = ";trusted_connection=true",
                 pw       = NULL),
        ll_USMOperationsDB =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "USMOperationsDB",
                 usr      = ";uid=USMUser",
                 pw       = ";pwd=USMUser2016#!"))
    return(ll_all)
}
