#' ConnectionHandlers
#'
#' Internal functions to get/set connection objects and attributes
#'
#' @name InternalConnHandling
#' @param db internal database name
#' @param caller internal caller
#' @param ts internal timestamp
cn.env <- new.env(parent = emptyenv())


pw.env <- new.env(parent = emptyenv())

#' @describeIn InternalConnHandling internal function to initiate a connection
#' @importFrom RODBC odbcDriverConnect
#' @importFrom data.table setattr
initConn <- function(db, caller, ts){
    cnName <- paste0("cn.", db)
    cnString <- ConnString(db)

    cnObj <- RunCatch(
        odbcDriverConnect(cnString, readOnlyOptimize = T),
        "CONNECTION FAILED",
        "CONNECTION FAILED",
        pattern = ".*(?<=])"
    )

    setattr(cnObj, "Database", db)
    setattr(cnObj, "Status", "Open")
    setattr(cnObj, "Initiator", caller)
    setattr(cnObj, "Opener", caller)
    setattr(cnObj, "Closer", NA)
    setattr(cnObj, "Requestor", NA)
    setattr(cnObj, "TimeInitiated", as.character(ts))
    setattr(cnObj, "TimeRequested", as.character(ts))
    setattr(cnObj, "TimeRequested", NA)
    setattr(cnObj, "TimeOpened", as.character(ts))
    setattr(cnObj, "TimeClosed", as.character(ts))
    setattr(cnObj, "TimeClosed", NA)
    setattr(cnObj, "DurSinceInit", xtimetaken(as.character(ts)))
    setattr(cnObj, "DurSinceRequest", NA)
    setattr(cnObj, "DurationOpen", xtimetaken(as.character(ts)))
    setattr(cnObj, "DurationClosed", 0)
    setattr(cnObj, "AccessCount", 1)


    # attr(cnObj, "Database")        <- db
    # attr(cnObj, "Status")          <- "Open"
    # attr(cnObj, "Initiator")       <- caller
    # attr(cnObj, "Opener")          <- caller
    # attr(cnObj, "Closer")          <- NA
    # attr(cnObj, "Requestor")       <- NA
    # attr(cnObj, "TimeInitiated")   <- as.character(ts)
    #
    # attr(cnObj, "TimeRequested")   <- as.character(ts) #do this to ensure class of this attribute is date
    # attr(cnObj, "TimeRequested")   <- NA
    #
    # attr(cnObj, "TimeOpened")      <- as.character(ts)
    #
    # attr(cnObj, "TimeClosed")      <- as.character(ts) #do this to ensure class of this attribute is date
    # attr(cnObj, "TimeClosed")      <- NA
    #
    # attr(cnObj, "DurSinceInit")    <- xtimetaken(as.character(ts))
    # attr(cnObj, "DurSinceRequest") <- NA
    # attr(cnObj, "DurationOpen")    <- xtimetaken(as.character(ts))
    # attr(cnObj, "DurationClosed")  <- 0
    # attr(cnObj, "AccessCount")     <- 1


    assign(x = cnName, value = cnObj, envir = cn.env)

    # setattr(cnObj, "MemAddress", 1)
    # setattr(cnObj, "NumRefs", 1)
    return(1)
}

#' @describeIn InternalConnHandling internal function to request an open connection
#' @importFrom data.table setattr
#' @export
requestConn <- function(db, caller, ts){
    conn <- GetConn(db)

    count <- attr(conn, "AccessCount")
    setattr(conn, "Requestor", caller)
    setattr(conn, "TimeRequested", as.character(ts))
    setattr(conn, "AccessCount", count + 1)
    return(1)
}

#' @describeIn InternalConnHandling internal function to open a closed connection
#' @importFrom RODBC odbcReConnect
#' @importFrom data.table setattr
#' @export
openConn <- function(db, caller, ts){

    oldcnObj <- GetConn(db)
    cnName <- paste0("cn.", db)
    cnString <- ConnString(db)  # needs to be here because of bug in RODBC. See attribute "call" for conn object

    # reopening actually creates a new connection object
    # need to copy old connection objects attributes and update a select few
    #
    # get old attributes and attribute names
    attribs <- ListConnAttr()
    oldvals <- sapply(attribs, attr, x=oldcnObj)
    oldnames <- names(oldvals)

    # create new connection object
    newcnObj <- odbcReConnect(oldcnObj) #odbcReconnect will copy the object. We can clean it in the next line since we don't need it anymore
    Clean(db)

    # copy old attributes over
    f <- function(name, val){
        setattr(newcnObj, name, val)
    }
    mapply(f, oldnames, as.character(oldvals))

    # Update select attributes
    count <- as.numeric(attr(newcnObj, "AccessCount"))

    setattr(newcnObj, "Status", "Open")
    setattr(newcnObj, "Opener", caller)
    setattr(newcnObj, "TimeOpened", as.character(ts))
    setattr(newcnObj, "DurationOpen", xtimetaken(as.character(ts)))
    setattr(newcnObj, "AccessCount", count + 1)

    assign(cnName, newcnObj, envir = cn.env)

    return(1)
}

#' @describeIn InternalConnHandling internal function to close an open connection
#' @importFrom data.table setattr
#' @importFrom RODBC odbcClose
#' @export
closeConn <- function(db, caller, ts){
    conn <- GetConn(db)
    tryCatch({
        odbcClose(conn)
    }, error = function(c){
        warning("closeConn: connection already closed", call. = FALSE)
    },finally = {
        setattr(conn, "Status", "Closed")
        setattr(conn, "Closer", caller)
        setattr(conn, "TimeClosed", as.character(ts))
        setattr(conn, "DurationClosed", xtimetaken(as.character(ts)))
    })
    return(1)
}


# #' @describeIn InternalConnHandling test open connection
# #' @importFrom data.table setattr
# #' @importFrom RODBC odbcClose
# #' @export
# TestOpen <- function(db){
#     cn <- GetConn(db)
#     RODBC::odbcGetErrMsg()
#     odbcClose(conn)
#
#     setattr(conn, "Status", "Closed")
#     setattr(conn, "Closer", caller)
#     setattr(conn, "TimeClosed", as.character(ts))
#     setattr(conn, "DurationClosed", xtimetaken(as.character(ts)))
#     return(1)
# }




