#' Connection Information
#'
#' Functions to get more granular details about a connection object that exists within the connection pool.
#' For more general connection handling functions
#'
#' @details These functions are convenience wrappers that retrieve attribute information from a connection object that exists.
#' The functions presented here are intended for developers and adminstrators and are likely less common requests. For additional
#' connection management functions that are perhaps more common, see
#'
#' @describeIn OpenBy Returns a string indicating the custom query function or user that make the FIRST opened connection to
#' specified database
#' @inheritParams OpenDB
#' @export
OpenedBy <- function(db=NULL){
    if(!IsOpen(db))
        return("Connection not open")
    opener <- attr(GetConn(db), "OpenedBy")
    if(grepl("OpenDB",opener)){
        return("Global Connection (user script)")
    }else{
        return(opener)
    }
}

#' @describeIn OpenBy Returns a string indicating the custom query function or user that make the LAST opened connection to
#' specified database
#' @inheritParams OpenDB
#' @export
ReOpenedBy <- function(db=NULL){
    if(!IsOpen(db))
        return("Connection not open")
    opener <- attr(GetConn(db), "ReOpenedBy")
    if(grepl("OpenDB",opener)){
        return("Global Connection (user script)")
    }else{
        return(opener)
    }
}

#' @describeIn OpenBy Returns a string indicating the custom query function or user most recently closed a connection
#' to the specified database
#' @inheritParams OpenDB
#' @export
ClosedBy <- function(db=NULL){
    if(!IsClosed(db))
        return("Connection not closed")
    attr(GetConn(db), "ClosedBy")
}

#' @describeIn OpenBy Returns a string indicating the custom query function or user that sent a request to make a connection
#' to the specified database. A "request" simply means that an "open" request was made to a database that was already open.
#' @inheritParams OpenDB
#' @export
RequestedBy <- function(db=NULL){
    if(!IsOpen(db))
        return("Connection not open")
    attr(GetConn(db), "RequestedBy")
}

#' @describeIn OpenBy Returns the duration in seconds that an open connection has existed for a specified db
#' @inheritParams OpenDB
#' @export
OpenDuration <- function(db=NULL){
    if(!IsOpen(db))
        return("Connection not open")
    timetaken(attr(GetConn(db), "TimeOpened"))
}

#' @describeIn OpenBy Returns the duration in seconds since the last request was made to a specifed database
#' @inheritParams OpenDB
#' @export
TimeSinceRequest <- function(db=NULL){
    if(!IsOpen(db))
        return("Connection not open")
    timetaken(attr(GetConn(db), "TimeRequested"))
}

#' @describeIn OpenBy Returns the duration in seconds since the status of the specifed database was changed to "close"
#' @inheritParams OpenDB
#' @export
CloseDuration <- function(db=NULL){
    if(!IsClosed(db))
        return("Connection not closed")
    timetaken(attr(GetConn(db), "TimeClosed"))
}

#' @describeIn OpenBy Returns a numeric that represents the count of requests that were made to a specified db
#' @inheritParams OpenDB
#' @export
AccessCount <- function(db=NULL){
    if(!IsOpen(db))
        return("Connection not open")
    attr(GetConn(db), "AccessCount")
}
