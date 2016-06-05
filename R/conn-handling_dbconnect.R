#' ConnectionHandlers
#'
#' Functions to get/set connection objects and attributes.
#'
#' @rdname InternalConnHandling
#' @inheritParams OpenDB
cn.env <- new.env(parent = emptyenv())

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
GetConn <- function(db){
    # if(exists("cn.env", mode="environment")){
    #     if(exists(paste0("cn.", db), cn.env)){
    #         get(paste0("cn.", db), cn.env)
    #     }
    # }else{
    #     assign("cn.env", new.env(parent = as.environment("package:sqlsauce")))
    #
    # }

    # updated logic because cn.env will always exist and there will
    # never be a situation where it will need to be assigned here.
    if(exists(paste0("cn.", db), cn.env)){
        get(paste0("cn.", db), cn.env)
    }else{
        NULL
    }
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
SetConn <- function(object     = NULL,
                    name       = NULL,
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
    attr(object, "OpenedBy")      <- openedby
    attr(object, "ReOpenedBy")    <- reopenedby
    attr(object, "TimeOpened")    <- tOpen
    attr(object, "RequestedBy")   <- request
    attr(object, "TimeRequested") <- tRequest
    attr(object, "ClosedBy")      <- closedby
    attr(object, "TimeClosed")    <- tClose
    attr(object, "AccessCount")   <- count

    # Assign to connection environment
    assign(name, object, envir = cn.env)
}

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
#' @importFrom RODBC odbcCloseAll
CleanAll <- function(){
    odbcCloseAll()
    rm(list = ls(cn.env), envir = cn.env)
    return(1)
}



#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export
Clean <- function(db){
    cnName <- paste0("cn.",db)

    if(exists(cnName, envir = cn.env)){
        if(IsOpen(db))
            CloseDB(db)

        rm(list = cnName, envir = cn.env)
        return(1)
    }
    return(0)
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
                   paste(valid_DBs, collapse=", ")), call. = F)
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
