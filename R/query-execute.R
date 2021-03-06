#' Execute a sql query on the database
#'
#' This function is a wrapper around \code{RODBC::sqlQuery} that provides some convenience as it interacts with the
#' connection handlers in this package. Additionally, it provides helpful errors, a tracing system, and various other checks
#'
#' @rdname ExecuteQuery
#' @param db A database to send a query to and retrieve data from
#' @param query A valid SQL query string
#' @param time A boolean indicating wether to display the query's time to completion
#'
#' @return A data.table with query results
#' @export
#' @importFrom RODBC sqlQuery
#' @importFrom data.table data.table
dataDish <- function(db, query, time=FALSE) {

    OpenDB(db)              # if user did not open connection, open it
    on.exit(CloseDB(db))    # then close it on exit

    # get frame number in case error occurs
    nFrame <- sys.nframe()

    # remove sci notation (for numerics in query), reset to global onexit
    globscipen <- options()$scipen
    options(scipen = 1000)
    on.exit(options(scipen = globscipen), add = TRUE)

    Timer()
    rDT <- data.table(sqlQuery(GetConn(db), query, errors = TRUE))
    dur <- Timer(START = FALSE)

    if(time){
        fubar::PrintMessage(paste0("Query completed in: ", dur, " seconds"), content = query)
    }

    # if TRUE then error
    if("V1" %in% colnames(rDT)){
        errlookup <- data.table(ErrorMessage = c("Communication link failure",
                                                 "not find stored procedure",
                                                 "Invalid object name",
                                                 "Incorrect syntax near"),
                                Description = c("Check network connection",
                                                "Check query string",
                                                "Check database table name",
                                                "Check query string"))

        errmsg <- stringr::str_extract(rDT[1, V1], "(?<=SQL Server]).*$")
        friendly <- errlookup[which(stringr::str_detect(errmsg, ErrorMessage))[1], Description]
        PrintSqlError(errmsg, friendly, nFrame)
    }

    if(!nrow(rDT))
        warning("Query successfully executed but no data")

    return(rDT)
}

# #' @describeIn ExecuteQuery An alias to xQuery
# #' @export
# dataDish <- xQuery


