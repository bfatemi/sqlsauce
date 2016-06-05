#' Connection Object Information
#'
#' Functions to get more granular details about a connection object that exists within the connection pool.
#' For more general connection handling functions
#'
#' @details These functions are convenience wrappers that retrieve attribute information from a connection object that exists.
#' The functions presented here are intended for developers and adminstrators and are likely less common requests. For additional
#' connection management functions that are perhaps more common, see
#'
#' @describeIn AboutConnection Prints information about access parameters to specified database. Returns NULL
#' @inheritParams OpenDB
#' @export
AccessInfo <- function(db){
    db      <- CheckDB(db)
    string  <- ConnString(db)
    bordLen <- 60

    ll.info <- sapply(strsplit(strsplit(string,";")[[1]],"="),
                      function(i){
                          obj <- list(i[2])
                          names(obj) <- i[1]
                          return(obj) })

    # Print info with title centered on border length
    title <- "Access Parameters"
    spaces <- paste(rep(" ", floor((bordLen - nchar(title))/ 2)), collapse="")
    cat(paste0(spaces, title, spaces, "\n"))
    .PrintInfo(ll.info, "Info", "")
}


#' @describeIn AboutConnection Prints verbose information on the current status of specified database. Returns Null
#' @inheritParams OpenDB
#' @param nFrame An internally used parameter to provide the developer a traceback of where a connection error occured
ConnStatus <- function(db=NULL, nFrame=NULL){

    # Identify who called for the status (tracing)
    if(is.null(nFrame)) nFrame <- sys.nframe()-1
    caller <- .Caller(nFrame)

    fnStatus <- function(x){
        cnObj <- GetConn(x)
        if(IsOpen(x)){
            timeO <- as.character(attr(cnObj, "TimeOpened"))
            timeR <- as.character(attr(cnObj, "TimeRequested"))
            list(Status          = "Open",
                 Database        = x,
                 OpenedBy        = attr(cnObj, "OpenedBy"),
                 TimeOpened      = timeO,
                 Dur_Open        = xtimetaken(attr(cnObj, "TimeOpened")),
                 LastRequestedBy = attr(cnObj, "RequestedBy"),
                 TimeLastRequest = timeR,
                 Dur_LastRequest = xtimetaken(attr(cnObj, "TimeRequested")),
                 AccessCount     = attr(cnObj, "AccessCount"),
                 Details         = as.list(RODBC::odbcGetInfo(cnObj)))
        }else if(IsClosed(x)){
            timeC <- as.character(attr(cnObj, "TimeClosed"))
            list(Status          = "Closed",
                 Database        = x,
                 ClosedBy        = attr(cnObj, "ClosedBy"),
                 TimeClosed      = timeC,
                 Dur_Closed      = xtimetaken(attr(cnObj, "TimeClosed")))
        }else{
            list(Status          = "Closed",
                 Database        = x,
                 ErrorMsg        = "Connection Does Not Exist")
        }
    }

    if(is.null(db)) db <- sapply(Databases(),"[[","database")
    else db <- CheckDB(db)
    ll <- lapply(db, fnStatus)

    # Print info on console with a title that is centered on border length
    title <- "Connection Status"
    bordLen <- 60
    spaces <- paste(rep(" ", floor((bordLen - nchar(title))/ 2)), collapse="")
    cat(paste0(spaces, title, spaces, "\n"))

    for(i in ll){
        dbName <- i$Database
        status <- i$Status
        i$Details$Data_Source_Name <- NULL
        i$Database <- NULL
        i$Status   <- NULL
        .PrintInfo(i, label = "", title=paste0(dbName, " : ", toupper(status)))
    }
}


#' @describeIn AboutConnection Convenience function to provide quick status for an open db
#' @inheritParams OpenDB
OpenConnStatus <- function(db=NULL){
    list(Open = IsOpen(db),
         Closed = IsClosed(db),
         Active = IsActive(db),
         InitiatedBy = OpenedBy(db),
         TimeInitiated = TimeOpened(db),
         LastOpenedBy = RequestedBy(db),
         TimeLastOpened = TimeLastRequest(db),
         DurationActive = OpenDuration(db),
         AccessCount = AccessCount(db))
}

#' @describeIn AboutConnection Convenience function to provide quick status for a closed db
#' @inheritParams OpenDB
CloseConnStatus <- function(db=NULL){
    list(ConnStatus = "Closed/Inactive",
         InitiatedBy = OpenedBy(db),
         TimeInitiated = TimeOpened(db),
         TimeClosed = TimeClosed(db),
         DurationClosed = CloseDuration(db))
}

#' @describeIn AboutConnection Convenience function to provide quick status for any db
#' @inheritParams OpenDB
#' @export
ConnAttr <- function(db=NULL){
    list(Open = IsOpen(db),
         Closed = IsClosed(db),
         Active = IsActive(db),
         InitiatedBy = OpenedBy(db),
         LastOpenedBy = RequestedBy(db),
         ClosedBy = ClosedBy(db),
         TimeInitiated = TimeOpened(db),
         TimeLastRequest = TimeLastRequest(db),
         TimeClosed = TimeClosed(db),
         DurationOpen = OpenDuration(db),
         DurationSinceRequest = DurSinceLastRequest(db),
         DurationClosed = CloseDuration(db),
         AccessCount = AccessCount(db))
}




#' @describeIn AboutConnection A function can be "inactive" which means it has never been initiated and neither open nor closed
#' @inheritParams OpenDB
#' @export
IsActive <- function(db=NULL){
    if(IsOpen(db) | IsClosed(db))
        return(TRUE)
    return(FALSE)
}

#' @describeIn AboutConnection Returns timestamp of when connection was last closed
#' @inheritParams OpenDB
#' @export
TimeClosed <- function(db=NULL){
    if(!IsClosed(db)){
        return(NA)
    }
    attr(GetConn(db), "TimeClosed")
}


#' @describeIn AboutConnection Returns timestamp of when connection was first opened
#' @inheritParams OpenDB
#' @export
TimeOpened <- function(db=NULL){
    attr(GetConn(db), "TimeOpened")
}

#' @describeIn AboutConnection Returns timestamp of when connection was last requested (last opened)
#' @inheritParams OpenDB
#' @export
TimeLastRequest <- function(db=NULL){
    attr(GetConn(db), "TimeRequested")
}


#' @describeIn AboutConnection Returns BOOL indicating whether status is "Open"
#' @inheritParams OpenDB
#' @export
IsOpen <- function(db=NULL){
    ifelse(!!sum(attr(GetConn(db), "Status") == "Open"),T,F)
}


#' @describeIn AboutConnection Returns BOOL indicating whether status is "Closed"
#' @inheritParams OpenDB
#' @export
IsClosed <- function(db=NULL){
    ifelse(!!sum(attr(GetConn(db), "Status") == "Closed"),T,F)
}

#' @describeIn AboutConnection Returns a string indicating the custom query function or user that make the FIRST opened connection to
#' specified database
#' @inheritParams OpenDB
#' @export
OpenedBy <- function(db=NULL){
    if(!IsOpen(db))
        return(NA)
    opener <- attr(GetConn(db), "OpenedBy")
    if(grepl("OpenDB",opener)){
        return("Global Connection (user script)")
    }else{
        return(opener)
    }
}

#' @describeIn AboutConnection Returns a string indicating the custom query function or user that make the LAST opened connection to
#' specified database
#' @inheritParams OpenDB
#' @export
ReOpenedBy <- function(db=NULL){
    if(!IsOpen(db))
        return(NA)
    opener <- attr(GetConn(db), "ReOpenedBy")
    if(grepl("OpenDB",opener)){
        return("Global Connection (user script)")
    }else{
        return(opener)
    }
}

#' @describeIn AboutConnection Returns a string indicating the custom query function or user most recently closed a connection
#' to the specified database
#' @inheritParams OpenDB
#' @export
ClosedBy <- function(db=NULL){
    if(!IsClosed(db))
        return(NA)
    attr(GetConn(db), "ClosedBy")
}

#' @describeIn AboutConnection Returns a string indicating the custom query function or user that sent a request to make a connection
#' to the specified database. A "request" simply means that an "open" request was made to a database that was already open.
#' @inheritParams OpenDB
#' @export
RequestedBy <- function(db=NULL){
    if(!IsOpen(db))
        return(NA)
    attr(GetConn(db), "RequestedBy")
}

#' @describeIn AboutConnection Returns the duration in seconds that an open connection has existed for a specified db
#' @inheritParams OpenDB
#' @export
OpenDuration <- function(db=NULL){
    if(!IsOpen(db))
        return(NA)
    xtimetaken(attr(GetConn(db), "TimeOpened"))
}

#' @describeIn AboutConnection Returns the duration in seconds since the last request was made to a specifed database
#' @inheritParams OpenDB
#' @export
DurSinceLastRequest <- function(db=NULL){
    if(!IsOpen(db))
        return(NA)
    xtimetaken(attr(GetConn(db), "TimeRequested"))
}

#' @describeIn AboutConnection Returns the duration in seconds since the status of the specifed database was changed to "close"
#' @inheritParams OpenDB
#' @export
CloseDuration <- function(db=NULL){
    if(!IsClosed(db))
        return(NA)
    xtimetaken(attr(GetConn(db), "TimeClosed"))
}

#' @describeIn AboutConnection Returns a numeric that represents the count of requests that were made to a specified db
#' @inheritParams OpenDB
#' @export
AccessCount <- function(db=NULL){
    if(!IsOpen(db))
        return(NA)
    attr(GetConn(db), "AccessCount")
}
