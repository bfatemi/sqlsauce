#' Managing Connections
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

#' @describeIn ManageConnections See all active (open or closed) connections in the pool
#' @export
ConnPool <- function(){
    gsub(pattern = "cn.", "", ls(cn.env))
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

#' @describeIn ManageConnections A function to clean the connection pool
#' @importFrom RODBC odbcCloseAll odbcClose
#' @export
Clean <- function(db=NULL){
    if(length(ls(cn.env))){
        if(is.null(db)){
            odbcCloseAll()
            rm(list = ls(cn.env), envir = cn.env)
            return(1)
        }
        if(ConnExists(db)){
            try(odbcClose(GetConn(db)), silent = TRUE)
            rm(list = paste0("cn.", db), envir = cn.env)
            return(1)
        }
    }
    return(0)
}

#' @describeIn ManageConnections A function to get a connection object from the connection pool
#' @export
GetConn <- function(db){
    db <- CheckDB(db)
    if(!ConnExists(db))
        stop("No connection has been initiated", call. = FALSE)
    get(paste0("cn.", db), cn.env)

}
