#' Managing Connections & Access:
#'
#' Functions to open/close and otherwise manage the connections to a configured
#' database, stored in a connection pool. These functions are intended for
#' developers that need access to the connection environment as they develop data
#' products.
#'
#' @section Note:
#' Any database where access to managing connections is required must be configured prior.
#' Future functionality will include the ability to auto-configurate a database.
#'
#' @name ManageConnections
#' @param db A character value representing the name of a database
NULL

#' @describeIn ManageConnections Open connection to a given database. Returns 1 on success
#' @export
OpenDB <- function(db=NULL, verbose=F){
    db     <- CheckDB(db)          # Check name against stored valid names
    nFrame <- sys.nframe()-1       # Get call stack in case of error

    caller   <- .CallStack(nFrame)
    calltime <- Sys.time()

    # need to initiate if does not exist
    # We know it's active. Need to only update attributes if already open

    if(!ConnExists(db))
        return(initConn(db, caller, calltime))

    status <- SeeConn(db, "Status")
    if(status == "Open")
        return(requestConn(db, caller, calltime))

    if(status == "Closed")
        return(openConn(db, caller, calltime))
}

#' @describeIn ManageConnections Close a connection to given db. Returns 1 on success
#' @export
CloseDB <- function(db=NULL) {
    db     <- CheckDB(db)          # Check name against stored valid names
    nFrame <- sys.nframe()-1       # Get call stack in case of error

    caller   <- .CallStack(nFrame)
    calltime <- Sys.time()

    # need to initiate if does not exist
    # We know it's active. Need to only update attributes if already open

    if(!ConnExists(db))
        stop("No connections exist to specified db", call. = F)

    status <- SeeConn(db, "Status")
    if(status == "Open")
        return(closeConn(db, caller, calltime))

    if(status == "Closed")
        return(0)
}

#' @describeIn ManageConnections
initConn <- function(db, initiator, timeinit){
    cnName <- paste0("cn.", db)
    cnString <- ConnString(db)

    cnObj <- RunCatch(
        RODBC::odbcDriverConnect(cnString, readOnlyOptimize = T),
        "CONNECTION FAILED",
        "CONNECTION FAILED",
        pattern = ".*(?<=])"
    )

    attr(cnObj, "Database")        <- db
    attr(cnObj, "Status")          <- "Open"
    attr(cnObj, "Initiator")       <- "global" #initiator
    attr(cnObj, "Opener")          <- "glob" #initiator
    attr(cnObj, "Closer")          <- NA
    attr(cnObj, "Requestor")       <- NA
    attr(cnObj, "TimeInitiated")   <- 2#timeinit
    attr(cnObj, "TimeRequested")   <- NA
    attr(cnObj, "TimeOpened")      <- 4#timeinit
    attr(cnObj, "TimeClosed")      <- NA
    attr(cnObj, "DurSinceInit")    <- 5#xtimetaken(timeinit)
    attr(cnObj, "DurSinceRequest") <- NA
    attr(cnObj, "DurationOpen")    <- 5#xtimetaken(timeinit)
    attr(cnObj, "DurationClosed")  <- 0
    attr(cnObj, "AccessCount")     <- 1

    assign(cnName, cnObj, envir = cn.env)
    return(1)
}

#' @describeIn ManageConnections
requestConn <- function(db, requestor, timerequest){
    conn <- GetConn(db)
    count <- attr(conn, "AccessCount")

    data.table::setattr(conn, "Requestor", requestor)
    data.table::setattr(conn, "TimeRequested", timerequest)
    data.table::setattr(conn, "AccessCount", count + 1)
    return(1)
}

#' @describeIn ManageConnections
openConn <- function(db, opener, timeopen){

    oldcnObj <- GetConn(db)
    cnName <- paste0("cn.", db)

    # reopening actually creates a new connection object
    # need to copy old connection objects attributes and update a select few
    #
    # get old attributes and attribute names
    oldvals <- sapply(ListConnAttr(), attr, x=oldcnObj)
    oldnames <- names(oldvals)

    # create new connection object
    newcnObj <- RODBC::odbcReConnect(oldcnObj)

    # copy old attributes over
    f <- function(name, val){
        data.table::setattr(newcnObj, name, val)
    }
    mapply(f, oldnames, oldvals)

    # Update select attributes
    count <- attr(newcnObj, "AccessCount")

    data.table::setattr(newcnObj, "Status", "Open")
    data.table::setattr(newcnObj, "Opener", opener)
    data.table::setattr(newcnObj, "TimeOpened", timeopen)
    data.table::setattr(newcnObj, "DurationOpen", xtimetaken(timeopen))
    data.table::setattr(newcnObj, "AccessCount", count + 1)

    assign(cnName, newcnObj, envir = cn.env)
    return(1)
}

#' @describeIn ManageConnections
closeConn <- function(db, closer, timeclose){
    conn <- GetConn(db)
    RODBC::odbcClose(conn)

    data.table::setattr(conn, "Status", "Closed")
    data.table::setattr(conn, "Closer", closer)
    data.table::setattr(conn, "TimeClosed", timeclose)
    data.table::setattr(conn, "DurationClosed", xtimetaken(timeclosed))
    return(1)
}

#' @describeIn ManageConnections See the current set of configured databases
#' @export
ValidDB <- function(){
    as.character(sapply(Databases(),"[[","database"))
}

#' @describeIn ManageConnections See all active (open or closed) connections in the pool
#' @export
ConnPool <- function(){
    gsub(pattern = "cn.", "", ls(cn.env))
}


#' @describeIn ManageConnections A function to check the argument against a list of
#'      valid DBs. Returns error on a none match. Performs approximate match.
#' @export
CheckDB <- function(db=NULL){
    valid_DBs <- ValidDB()

    tryCatch(
        as.character(match.arg(db, valid_DBs)),
        error = function(e)
            stop("Not valid DB! Use ValidDB() for configured DBs", call. = F)
    )
}

#' @describeIn ManageConnections A function to check whether a connection has been
#'      initiated. Returns TRUE if connection object exists, and FALSE otherwise. Error
#'      if argument is not the name of a configured db
#' @export
ConnExists <- function(db=NULL){
    db <- CheckDB(db)

    if(exists(paste0("cn.", db), cn.env))
        return(TRUE)
    return(FALSE)
}

#' @describeIn ManageConnections Returns the connection string associated with
#'      a configured database
#' @export
ConnString <- function(db=NULL){
    db <- CheckDB(db)
    cnstr <- substitute(paste0("Driver=",    driver,
                               ";Server=",   server,
                               ";database=", database, usr, pw))
    eval(cnstr, get(paste0("ll_", db), Databases()))
}

#' @describeIn ManageConnections
Databases <- function(){
    ll_all <- list(
        ll_Morpheus =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "Morpheus",
                 usr      = ";trusted_connection=true",
                 pw       = NULL),
        # ll_MorpheusDev =
        #     list(driver   = "SQL Server",
        #          server   = "sccdsql01.dv.local,1515",
        #          database = "Morpheus",
        #          usr      = ";uid=MorpheusAdmin",
        #          pw       = ";pwd=MorpheusAdmin2015"),
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
