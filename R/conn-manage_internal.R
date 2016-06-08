#' ConnectionHandlers
#'
#' Internal functions to get/set connection objects and attributes
#'
#' @name InternalConnHandling
#' @param db internal database name
#' @param caller internal caller
#' @param ts internal timestamp
cn.env <- new.env(parent = emptyenv())

#' @describeIn InternalConnHandling internal function to initiate a connection
#' @importFrom RODBC odbcDriverConnect
initConn <- function(db, caller, ts){
    cnName <- paste0("cn.", db)
    cnString <- ConnString(db)

    cnObj <- RunCatch(
        odbcDriverConnect(cnString, readOnlyOptimize = T),
        "CONNECTION FAILED",
        "CONNECTION FAILED",
        pattern = ".*(?<=])"
    )

    attr(cnObj, "Database")        <- db
    attr(cnObj, "Status")          <- "Open"
    attr(cnObj, "Initiator")       <- caller
    attr(cnObj, "Opener")          <- caller
    attr(cnObj, "Closer")          <- NA
    attr(cnObj, "Requestor")       <- NA
    attr(cnObj, "TimeInitiated")   <- as.character(ts)

    attr(cnObj, "TimeRequested")   <- as.character(ts) #do this to ensure class of this attribute is date
    attr(cnObj, "TimeRequested")   <- NA

    attr(cnObj, "TimeOpened")      <- as.character(ts)

    attr(cnObj, "TimeClosed")      <- as.character(ts) #do this to ensure class of this attribute is date
    attr(cnObj, "TimeClosed")      <- NA

    attr(cnObj, "DurSinceInit")    <- xtimetaken(as.character(ts))
    attr(cnObj, "DurSinceRequest") <- NA
    attr(cnObj, "DurationOpen")    <- xtimetaken(as.character(ts))
    attr(cnObj, "DurationClosed")  <- 0
    attr(cnObj, "AccessCount")     <- 1

    assign(cnName, cnObj, envir = cn.env)
    return(1)
}

#' @describeIn InternalConnHandling internal function to request an open connection
#' @importFrom data.table setattr
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
openConn <- function(db, caller, ts){

    oldcnObj <- GetConn(db)
    cnName <- paste0("cn.", db)
    cnString <- ConnString(db)  # needs to be here because of bug in RODBC. See attribute "call" for conn object

    # reopening actually creates a new connection object
    # need to copy old connection objects attributes and update a select few
    #
    # get old attributes and attribute names
    oldvals <- sapply(ListConnAttr(), attr, x=oldcnObj)
    oldnames <- names(oldvals)

    # create new connection object
    newcnObj <- odbcReConnect(oldcnObj)

    # copy old attributes over
    f <- function(name, val){
        setattr(newcnObj, name, val)
    }
    mapply(f, oldnames, oldvals)

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
closeConn <- function(db, caller, ts){
    conn <- GetConn(db)
    odbcClose(conn)

    setattr(conn, "Status", "Closed")
    setattr(conn, "Closer", caller)
    setattr(conn, "TimeClosed", as.character(ts))
    setattr(conn, "DurationClosed", xtimetaken(as.character(ts)))
    return(1)
}







