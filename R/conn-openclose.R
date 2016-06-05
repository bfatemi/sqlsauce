#' @title Open and Close Connections
#'
#' @description A number of functions to handle connections. These functions are intended for developers that need
#' access to the connection environment as they develop data products. All functions within the morphr package
#' utilize these functions. If you are a developer would like more details, see the examples below or visit
#' the following tutorial pages:
#'
#' @details Any database where access to managing connections is required must be configured prior.
#'
#' A connection object can have multiple states:
#' \itemize{
#'  \item Open/closed
#'  \item Not Open/Not Closed
#' }
#'
#' The latter two ("Not Open" and "Not Closed") refer to states where a connection object may exist but has timeout,
#' or when the connection object does not exist at all. For example, the first time a connection is initialized, it
#' becomes "Open". Prior to that, it is not open or closed. After a request is received to close the connection, the
#' status is changed to "closed". From there on, the connection object exists in the pool until it is garbage collected
#' or explicitly removed.
#'
#' @section Internal Functions:
#' These internal functions are wrappers for package:RODBC functions. The wrappers are there primarily for custom
#' error handling and are not intended to be exposed to developers or users.
#'
#' @param db A character value naming the database to use
#' @param verbose A boolean indicating whether to print a status report on the console
#' @describeIn OpenDB Open connection to a given database. Returns Null
#' @export
OpenDB <- function(db=NULL, verbose=F){
    db        <- CheckDB(db)         # Check name against stored valid names
    cnName    <- paste0("cn.", db)    # name the connection object
    thisFrame <- sys.nframe()-1       # Get call stack in case of error

    if(IsOpen(db)){
        oldObj <- GetConn(db)
        SetConn(object     = oldObj,
                name       = cnName,
                status     = "Open",
                openedby   = attr(oldObj, which = "OpenedBy"),
                reopenedby = attr(oldObj, which = "ReOpenedBy"),
                tOpen      = attr(oldObj, which = "TimeOpened"),
                request    = .CallStack(thisFrame),
                tRequest   = Sys.time(),
                closedby   = NULL,
                tClose     = NULL,
                count      = attr(oldObj, which = "AccessCount") + 1)

    }else if(IsClosed(db)){
        oldObj <- GetConn(db)
        newObj <- .odbcOpen(db)
        time <- Sys.time()

        SetConn(object     = newObj,
                name       = cnName,
                status     = "Open",
                openedby   = attr(oldObj, which = "OpenedBy"),
                reopenedby = .CallStack(thisFrame),
                tOpen      = time,
                request    = .CallStack(thisFrame),
                tRequest   = time,
                closedby   = NULL,
                tClose     = NULL,
                count      = 1)
    }else{
        time <- Sys.time()
        SetConn(object     = .odbcOpen(db),
                name       = cnName,
                status     = "Open",
                openedby   = .CallStack(thisFrame),
                reopenedby = NULL,
                tOpen      = time,
                request    = .CallStack(thisFrame),
                tRequest   = time,
                closedby   = NULL,
                tClose     = NULL,
                count      = 1)
    }
    if(verbose) ConnStatus(db) # Print info if desired
    return(1)
}

#' @describeIn OpenDB Close a connection to given db. Returns Null
#' @export
CloseDB <- function(db=NULL, verbose=F) {
    cnName  <- paste0("cn.", db)      # name the connection object
    thisFrame <- sys.nframe()-1       # Get call stack in case of error
    if(IsClosed(db)){

        print(paste0("Connection to ", db, " already closed"))
        return(0)

    }else if(IsOpen(db)){
        oldObj <- GetConn(db)
        SetConn(object     = oldObj,
                name       = cnName,
                status     = "Closed",
                openedby   = attr(oldObj, which = "OpenedBy"),
                reopenedby = attr(oldObj, which = "ReOpenedBy"),
                tOpen      = attr(oldObj, which = "TimeOpened"),
                request    = attr(oldObj, which = "RequestedBy"),
                tRequest   = attr(oldObj, which = "TimeRequested"),
                closedby   = .CallStack(thisFrame),
                tClose     = Sys.time(),
                count      = attr(oldObj, which = "AccessCount"))
        .odbcClose(db)
    }else{
        print(paste0("No open or closed connections exist to ",db))
        return(0)
    }
    if(verbose) ConnStatus(db)
    return(1)
}

#' @describeIn OpenDB Internal RODBC wrapper
.odbcOpen <- function(db){
    #nFrame <- sys.nframe()
    emsg <- "CONNECTION FAILED"
    pat <- ".*(?<=])"

    RunCatch(
        RODBC::odbcDriverConnect(ConnString(db), readOnlyOptimize = T),
        emsg,
        emsg,
        pattern = pat
    )
}

#' @describeIn OpenDB Internal RODBC wrapper
.odbcClose <- function(db) {
    cnObj <- GetConn(db)

    if(is.null(cnObj)){
        msg <- paste0("Could not close. Connection to ", db, " does not exist")
        emsg <- "CLOSE CONNECTION FAILED"
        RunCatch(stop(msg), emsg)
    }else{
        if(IsClosed(db)){
            return(0)
        }else{
            emsg <- "UNK:CLOSE CONNECTION ERROR"
            RunCatch(RODBC::odbcClose(cnObj), emsg)
            return(1)
        }
    }
}

#' @describeIn OpenDB Internal RODBC wrapper
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
