#' ConnectionHandlers
#'
#' Functions to get/set connection objects and see attributes.
#'
#' @rdname InternalConnHandling
#' @export



#' @rdname InternalConnHandling
#' @export


#' @rdname InternalConnHandling
#' @inheritParams OpenDB


#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @export




#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' @importFrom RODBC odbcCloseAll odbcClose
#' @export
#' If db is null, will close and clean all. If exists, close and clean else return 0
Clean <- function(db=NULL){
    if(length(ls(cn.env))){
        if(is.null(db)){
            odbcCloseAll()
            rm(list = ls(cn.env), envir = cn.env)
            return(1)
        }
        if(ConnExists(db)){
            odbcClose(GetConn(db))
            rm(list = paste0("cn.", db), envir = cn.env)
            return(1)
        }
    }
    return(0)
}


######################################################################################
# Get Connections, set/see attributes
######################################################################################

#' @rdname InternalConnHandling
#' This will initiate the connection environment when package is attached
cn.env <- new.env(parent = emptyenv())

#' @rdname InternalConnHandling
#' @inheritParams OpenDB
#' An internal function to retrieve a connection object from the connection pool.
#' This function checks config db and existance of connection. Any function calling
#' this to retrieve a connection will (1) get the connection back; OR (2) get an error
#' This does not check connection status or any other attributes
GetConn <- function(db){
    db <- CheckDB(db)
    if(!ConnExists(db))
        stop("No connection has been initiated", call. = FALSE)
    get(paste0("cn.", db), cn.env)

}


SetConn <- function(conn            = NULL,
                    Name            = NULL,
                    Database        = NULL,
                    Status          = NULL,
                    Initiator       = NULL,
                    Opener          = NULL,
                    Closer          = NULL,
                    Requestor       = NULL,
                    TimeInitiated   = NULL,
                    TimeRequested   = NULL,
                    TimeOpened      = NULL,
                    TimeClosed      = NULL,
                    DurSinceInit    = NULL,
                    DurSinceRequest = NULL,
                    DurationOpen    = NULL,
                    DurationClosed  = NULL,
                    AccessCount     = NULL){

    ### Set object attributes
    attr(conn, "Name")            <- Name
    attr(conn, "Database")        <- Database
    attr(conn, "Status")          <- Status
    attr(conn, "Initiator")       <- Initiator
    attr(conn, "Opener")          <- Opener
    attr(conn, "Closer")          <- Closer
    attr(conn, "Requestor")       <- Requestor
    attr(conn, "TimeInitiated")   <- TimeInitiated
    attr(conn, "TimeRequested")   <- TimeRequested
    attr(conn, "TimeOpened")      <- TimeOpened
    attr(conn, "TimeClosed")      <- TimeClosed
    attr(conn, "DurSinceInit")    <- DurSinceInit
    attr(conn, "DurSinceRequest") <- DurSinceRequest
    attr(conn, "DurationOpen")    <- DurationOpen
    attr(conn, "DurationClosed")  <- DurationClosed
    attr(conn, "AccessCount")     <- AccessCount

    # Assign to connection environment
    assign(name, object, envir = cn.env)
}


InitConn <- function(db, initiator, timeinit){
    cnName <- paste0("cn.", db)
    cnObj <- .odbcOpen(db)

    attr(cnObj, "Database")        <- db
    attr(cnObj, "Status")          <- "Open"
    attr(cnObj, "Initiator")       <- initiator
    attr(cnObj, "Opener")          <- initiator
    attr(cnObj, "Closer")          <- NA
    attr(cnObj, "Requestor")       <- NA
    attr(cnObj, "TimeInitiated")   <- timeinit
    attr(cnObj, "TimeRequested")   <- NA
    attr(cnObj, "TimeOpened")      <- timeinit
    attr(cnObj, "TimeClosed")      <- NA
    attr(cnObj, "DurSinceInit")    <- xtimetaken(timeinit)
    attr(cnObj, "DurSinceRequest") <- NA
    attr(cnObj, "DurationOpen")    <- xtimetaken(timeinit)
    attr(cnObj, "DurationClosed")  <- 0
    attr(cnObj, "AccessCount")     <- 1

    assign(cnName, cnObj, envir = cn.env)
}


######################################################################################
# Access
######################################################################################



