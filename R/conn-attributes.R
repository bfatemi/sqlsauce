#' Connection Attribute Information
#'
#' Resources to manage connections, to test the status of connections, as well as functions to get details (attributes)
#' about a connection object that exists within the connection pool.
#'
#' @details As a developer creating a custom query function, you may need the ability to
#' know, for example, if a connection is open and how long it's been open. These are known
#' as attributes and all existing connection objects have the following set of attributes:
#'
#' \itemize{
#'  \item {\code{Database}}{Name of database}
#'  \item {\code{Status}}{Open/Closed}
#'  \item {\code{Initiator}}{The first opener of the connection}
#'  \item {\code{Opener}}{Who opened a closed connection}
#'  \item {\code{Closer}}{Who closed an open connection}
#'  \item {\code{Requestor}}{Who requested a connection (e.g. tried to open an already
#'          open connection)}
#'  \item {\code{TimeInitiated}}{When was the connection first opened (timestamp resets
#'          when connection is closed)}
#'  \item {\code{TimeOpened}}{When did the connection's status change from Closed to Open}
#'  \item {\code{TimeClosed}}{When did the connection's status change from Open to Closed}
#'  \item {\code{TimeRequested}}{When was the connection was last requested (timestamp)}
#'  \item {\code{DurSinceInit}}{Duration since connection was initiated}
#'  \item {\code{DurSinceRequest}}{Duration since a connection was last requested}
#'  \item {\code{DurationOpen}}{Duration that connection has been open}
#'  \item {\code{DurationClosed}}{Duration that connection has been closed}
#'  \item {\code{AccessCount}}{Number of times the connection has been requested/opened since init}
#' }
#'
#' @name ConnectionAttributes
#' @param db A character value representing the name of a database whose connection's attributes need to be accessed
#' @param what A character value representing the attribute name to access
#'
#' @examples
#'
#' \dontrun{
#' # Common developer helper functions #
#' db <- "a_database_name"
#' CheckDB(db)     # trys to find a matching database name, returns error if not
#' AccessInfo(db)  # access info for specified db
#' ValidDB()       # What DBs are configured in the package?
#' ConnString(db)  # Connection string for db?
#'
#' # Opening/Closing Connections #
#' OpenDB(db)      # Returns 1 on success
#' CloseDB(db)     # Returns 1 on success
#' CloseDB(db)     # Warning if closing db that is already closed
#' CloseDB("blah") # Error if db doesn't exist
#'
#' # Manage connections #
#' ConnExists(db)  # Returns TRUE for any open/closed connections
#' ConnPool()      # Returns all the connection (names) in the pool
#' ConnStatus()    # Returns a table showing status for all configured dbs
#'
#' # Use should be rare to not at all #
#' GetConn(db)     # Retrieve connection object
#' Clean(db)       # Removes connection from pool (usually because of error)
#' Clean()         # Removes ALL connections from pool (usually because of error)
#' }
NULL


#' @describeIn ConnectionAttributes A function to "see" a particular connection objects attributes.
#' @export
#' @importFrom data.table data.table copy
SeeConn <- function(db=NULL, what=NULL){
    cnObj <- GetConn(db)

    if(!is.null(what)){

        # Update durations only if attributes are requested
        #
        if(what == "DurSinceInit")
            return(as.character(xtimetaken(attr(cnObj, "TimeInitiated"))))

        if(what == "DurSinceRequest")
            return(as.character(xtimetaken(attr(cnObj, "TimeRequested"))))

        if(what == "DurationOpen"){
            if(attr(cnObj, "Status") == "Closed")
                return(0)
            else
                return(as.character(xtimetaken(attr(cnObj, "TimeOpened"))))
        }

        if(what == "DurationClosed"){
            if(attr(cnObj, "Status") == "Open")
                return(0)
            else
                return(as.character(xtimetaken(attr(cnObj, "TimeClosed"))))
        }
        # attributes(cnObj)
        att <- attr(cnObj, which = what)

        if(is.null(att))
            warning("Not a recorded attribute. Check attribute name")

    }else{

        # get all attribute names, and my current attribute values and create a data.table
        anames <- ListConnAttr()
        att <- attributes(cnObj)
        selected.att <- att[anames] # keep attributes that I chose with ListConnAttr()
        names(selected.att) <- anames

        # replace all NULL list elements with NA for conversion to dt in next step
        selected.att <- lapply(selected.att, function(a) if(is.null(a)) NA else a)
        att <- data.table(cbind(Names = anames, Values = unlist(selected.att)))
        # att <- data.table(Names = anames,
        #                   Values = data.table(t(data.frame(selected.att)))$V1)

        # update Duration attributes
        if(att[Names == "Status", Values] == "Open"){

            et <- substitute(Names %in% c("TimeInitiated", "TimeRequested", "TimeOpened"))
            ed <- substitute(Names %in% c("DurSinceInit", "DurSinceRequest", "DurationOpen"))

            att <- att[eval(ed), Values := as.character(lapply(att[eval(et), Values], xtimetaken))]
            att[Names == "DurationClosed", Values := "0"]
            att <- copy(att)
        }else{
            et <- substitute(Names %in% c("TimeInitiated", "TimeRequested", "TimeClosed"))
            ed <- substitute(Names %in% c("DurSinceInit", "DurSinceRequest", "DurationClosed"))

            att <- att[eval(ed), Values := as.character(lapply(att[eval(et), Values], xtimetaken))]
            att[Names == "DurationOpen", Values := "0"]
            att <- copy(att)
        }
    }
    return(att)
}

#' @describeIn ConnectionAttributes See attributes for all open connections
#' @export
#' @importFrom data.table data.table rbindlist
SeeOpenConns <- function(){

    allinPool <- ConnPool()

    ll <- lapply(allinPool, function(db){
        cnObj <- GetConn(db)

        status <- attr(cnObj, "Status")
        if(status == "Open"){
            data.table(Database            = attr(cnObj, "Database"),
                       Status              = status,
                       Initiator           = attr(cnObj, "Initiator"),
                       LastOpener          = attr(cnObj, "Opener"),
                       LastRequestor       = attr(cnObj, "Requestor"),
                       TimeInitiated       = attr(cnObj, "TimeInitiated"),
                       TimeLastRequested   = attr(cnObj, "TimeRequested"),
                       TimeLastOpened      = attr(cnObj, "TimeOpened"),
                       DurSinceInit        = xtimetaken(attr(cnObj, "TimeInitiated")),
                       DurSinceLastRequest = xtimetaken(attr(cnObj, "TimeRequested")),
                       DurationOpen        = xtimetaken(attr(cnObj, "TimeOpened")),
                       AccessCount         = attr(cnObj, "AccessCount"))
        }
    })
    rbindlist(ll)
}

#' @describeIn ConnectionAttributes See attributes for all closed connections
#' @export
#' @importFrom data.table data.table rbindlist
SeeClosedConns <- function(){

    allinPool <- ConnPool()

    ll <- lapply(allinPool, function(db){
        cnObj  <- GetConn(db)
        status <- attr(cnObj, "Status")

        if(status == "Closed"){
            data.table(Database          = attr(cnObj, "Database"),
                       Status            = status,
                       Initiator         = attr(cnObj, "Initiator"),
                       LastRequestor     = attr(cnObj, "Requestor"),
                       Closer            = attr(cnObj, "Closer"),
                       TimeInitiated     = attr(cnObj, "TimeInitiated"),
                       TimeLastRequested = attr(cnObj, "TimeRequested"),
                       TimeClosed        = attr(cnObj, "TimeClosed"),
                       DurationClosed    = xtimetaken(attr(cnObj, "TimeClosed")),
                       AccessCount       = attr(cnObj, "AccessCount"))
        }
    })
    rbindlist(ll)
}

#' @describeIn ConnectionAttributes Prints status of all connection in the pool
#' @export
#' @importFrom data.table data.table rbindlist
ConnStatus <- function(db=NULL){

    if(!is.null(db))
        return(SeeConn(db, "Status"))

    alldb <- ValidDB()
    allinPool <- ConnPool()

    notactive <- alldb[which(!alldb %in% allinPool)]
    active <- alldb[which(alldb %in% allinPool)]

    inactivell <- list(data.table(Database = notactive, Status = "Inactive"))
    activell <- lapply(active, function(db){
        cnObj <- GetConn(db)
        data.table(Database = db, Status = attr(cnObj, "Status"))
    })
    return(rbindlist(c(activell, inactivell)))
}

#' @describeIn ConnectionAttributes A function that provides a quick lookup of
#' all attribute names that an object will have
#' @export
ListConnAttr <- function(){
    c("Database",
      "Status",
      "Initiator",
      "Opener",
      "Closer",
      "Requestor",
      "TimeInitiated",
      "TimeRequested",
      "TimeOpened",
      "TimeClosed",
      "DurSinceInit",
      "DurSinceRequest",
      "DurationOpen",
      "DurationClosed",
      "AccessCount")
}






