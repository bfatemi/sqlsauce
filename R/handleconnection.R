#' @title Connection Handling
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
#' @family connection handling functions
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

#' @describeIn OpenDB Prints information about access parameters to specified database. Returns Null
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


#' @describeIn OpenDB Prints verbose information on the current status of specified database. Returns Null
#' @param nFrame An internally used parameter to provide the developer a traceback of where a connection error occured
#' @export
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
                 Dur_Open        = timetaken(attr(cnObj, "TimeOpened")),
                 LastRequestedBy = attr(cnObj, "RequestedBy"),
                 TimeLastRequest = timeR,
                 Dur_LastRequest = timetaken(attr(cnObj, "TimeRequested")),
                 AccessCount     = attr(cnObj, "AccessCount"),
                 Details         = as.list(odbcGetInfo(cnObj)))
        }else if(IsClosed(x)){
            timeC <- as.character(attr(cnObj, "TimeClosed"))
            list(Status          = "Closed",
                 Database        = x,
                 ClosedBy        = attr(cnObj, "ClosedBy"),
                 TimeClosed      = timeC,
                 Dur_Closed      = timetaken(attr(cnObj, "TimeClosed")))
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


#' @describeIn OpenDB Returns BOOL indicating whether status is "Open"
#' @export
IsOpen <- function(db=NULL){
    ifelse(!!sum(attr(GetConn(db), "Status") == "Open"),T,F)
}


#' @describeIn OpenDB Returns BOOL indicating whether status is "Closed"
#' @export
IsClosed <- function(db=NULL){
    ifelse(!!sum(attr(GetConn(db), "Status") == "Closed"),T,F)
}
