#' ConnectionHandlers
#'
#' Internal functions to get/set connection objects and attributes
#'
#' @name InternalConnHandling
#' @param db
#' @param caller
#' @param ts
cn.env <- new.env(parent = emptyenv())

#' @describeIn ManageConnections
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
    attr(cnObj, "TimeInitiated")   <- ts
    attr(cnObj, "TimeRequested")   <- NA
    attr(cnObj, "TimeOpened")      <- ts
    attr(cnObj, "TimeClosed")      <- NA
    attr(cnObj, "DurSinceInit")    <- xtimetaken(ts)
    attr(cnObj, "DurSinceRequest") <- NA
    attr(cnObj, "DurationOpen")    <- xtimetaken(ts)
    attr(cnObj, "DurationClosed")  <- 0
    attr(cnObj, "AccessCount")     <- 1

    assign(cnName, cnObj, envir = cn.env)
    return(1)
}

#' @describeIn ManageConnections
#' @importFrom data.table setattr
requestConn <- function(db, caller, ts){
    conn <- GetConn(db)
    count <- attr(conn, "AccessCount")

    setattr(conn, "Requestor", caller)
    setattr(conn, "TimeRequested", ts)
    setattr(conn, "AccessCount", count + 1)
    return(1)
}

#' @describeIn ManageConnections
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

    attributes(oldcnObj)

    # copy old attributes over
    f <- function(name, val){
        setattr(newcnObj, name, val)
    }
    mapply(f, oldnames, oldvals)

    # Update select attributes
    count <- as.numeric(attr(newcnObj, "AccessCount"))

    setattr(newcnObj, "Status", "Open")
    setattr(newcnObj, "Opener", caller)
    setattr(newcnObj, "TimeOpened", ts)
    setattr(newcnObj, "DurationOpen", xtimetaken(ts))
    setattr(newcnObj, "AccessCount", count + 1)

    assign(cnName, newcnObj, envir = cn.env)
    return(1)
}

#' @describeIn InternalConnHandling
#' @importFrom data.table setattr
#' @importFrom RODBC odbcClose
closeConn <- function(db, caller, ts){
    conn <- GetConn(db)
    odbcClose(conn)

    setattr(conn, "Status", "Closed")
    setattr(conn, "Closer", caller)
    setattr(conn, "TimeClosed", ts)
    setattr(conn, "DurationClosed", xtimetaken(ts))
    return(1)
}







