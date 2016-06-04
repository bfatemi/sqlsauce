#' Internal Dev Testing and Debugging Functions
#'
#' These are functions that are not exposed to users. They are internal functions designed to assist
#' with debugging errors related to tracing and identifying connections.
#'
#' @section Internal Use Only:
#'  Internal functions not intended to be used by developers. To open and close connections to a preconfigured database,
#'  use: \code{\link{OpenDB}} or \code{\link{CloseDB}}.
#'
#' @describeIn DebugInfo A function to customize errors related to connections
#' @param msg A message to be displayed with an error
#' @param nFrame A numeric value representing an index for a frame in the function callstack
#' @param friendly A friendly description of the error message to be displayed
#' @export
DebugInfo <- function(msg=NULL, nFrame=NULL){
    bordLen <- 60
    border  <- quote(cat(paste(c(rep("-", bordLen), "\n"), collapse = "")))

    if(is.null(nFrame))
        nFrame <- sys.nframe()
    if(is.null(msg))
        msg="Generic Error Message"

    if(!exists("cn.env", mode = "environment")) {
        connEnvironment <- "None"
        openConns       <- "No Open Connections/All databases are inactive"
    }else{
        connEnvironment <- capture.output(str(cn.env))
        openConns       <- ls(cn.env)

        if(!length(ls(cn.env)))
            openConns   <- "No Open Connections"
    }

    ErrorInfo = list(Message     = msg,
                     OccurredIn  = gsub("\\(.*\\)", "", sys.calls()[nFrame]),
                     TraceBack   = .CallStack(nFrame),
                     Environment = capture.output(str(sys.frame(nFrame)))) #,
    Connection = list(OpenConns  = openConns,
                      ConnEnvir  = connEnvironment,
                      Info       = "Run ConnStatus() for details")

    # Print info with title centered on border length
    title <- "Debug Information"
    spaces <- paste(rep(" ", floor((bordLen - nchar(title))/ 2)), collapse="")
    cat(paste0(spaces, title, spaces, "\n"))
    .PrintInfo(ErrorInfo, "Debug", "Error Information")
    .PrintInfo(Connection, "Debug", "Connection Environment")
    stop("See error information above",call. = F)
}

#' @describeIn DebugInfo A helper function to display sql errors that occur during a query
#' @export
PrintSqlError <- function(msg=NULL, friendly=NULL, nFrame=NULL){
    statDT <- data.table(sapply(db, ConnAttr), keep.rownames = T)
    setnames(statDT, c("Database", db))


    ErrorInfo <- list(SQLError    = msg,
                      ErrDesc     = friendly,
                      OccurredIn  = gsub("\\(.*\\)", "", sys.calls()[nFrame]),
                      TraceBack   = .CallStack(nFrame))
    Connection <- as.list(statDT$RemoteFE)
    names(Connection) <- statDT$Database

    environments <- list(ErrorEnv = capture.output(str(sys.frame(nFrame))),
                         ConnectionEnv = capture.output(str(cn.env)))

    # Print info with title centered on border length
    str(object = ErrorInfo,
        indent.str = "$...",
        comp.str = " ",
        no.list = TRUE,
        give.length = FALSE,
        give.attr = FALSE,
        give.head = FALSE)
    stop(paste0("SQL ERROR: ", msg), call. = F)
}

#' @describeIn DebugInfo A helper function to assist timing operations
#' @param time A date/time object from which point to calculate duration since.
#' @export
timetaken <- function(time){
    return(as.numeric(difftime(time, Sys.time(), units = "sec")))
}

#' @describeIn DebugInfo A helper function to assist timing operations
#' @param START A boolean value representing whether to start the timer or end the timer. START = FALSE
#'      will display the timer information
#' @export
Timer <- function(START=TRUE, print=FALSE){
    if(START){
        tstamp <<- Sys.time()
    }else{
        dur <- round(as.numeric(difftime(Sys.time(), tstamp, units = "sec")), 2)

        if(print){
            cat(paste0("Completed duration (sec): ", round(dur, 2)))
            cat("\n")
        }
        return(dur)
    }
}

#' @describeIn DebugInfo A helper function to extract the relevant and helpful portion of the function callstack
#' @export
.CallStack <- function(nFrame){
    calls <- as.character(sys.calls()[1:nFrame])
    calls <- gsubfn::gsubfn(replacement = "", x = calls, pattern = "\\(.*\\)")

    if(length(calls)>1){
        # eliminate all calls past the first "try" call
        firstTryCall <- grep("try", calls)[1]
        if(!is.na(firstTryCall))
            calls <- calls[-(firstTryCall:length(calls))]
    }
    paste(calls, collapse = " => ")
}

#' @describeIn DebugInfo A function to parse the function call stack and identify a caller at the current postion
#'      or at the position identified by "nFrame"
#' @export
.Caller <- function(nFrame=NULL){
    if(is.null(nFrame))
        nFrame <- sys.nframe()-2
    caller <- as.character(sys.calls()[nFrame])
    caller <- gsubfn::gsubfn(replacement = "", x = caller,
                     pattern = "\\(.*\\)")
    paste(caller, collapse = " => ")
}

#' @describeIn DebugInfo A helper function and wrapper around "str" to print info to the console
#' @param ll A list with objects to print information about
#' @param label A character string used to label the information about the contents of "ll"
#' @param title A character string to place as a header title to the information printed on the console
#' @export
.PrintInfo <- function(ll, label, title){
    bordLen <- 60
    border  <- quote(cat(paste(c(rep("-", bordLen), "\n"), collapse = "")))

    eval(border)
    cat(title,"\n")
    str(object = ll,
        indent.str = label,
        comp.str = " ",
        no.list = TRUE,
        give.length = FALSE,
        give.attr = FALSE,
        give.head = FALSE)
    eval(border)
}

#' @describeIn DebugInfo A function to elegantly print a notification banner on the console
#' @param msg A character string representing the message to display within the banner
#' @param sym A character symbol used to create the banner
#' @param content An optional object to print below the banner
#' @export
PrintMessage <- function(msg, sym="#", content=NULL){

    # Will be messing with console width so grab global setting to reset later
    globscipen <- options()$width
    on.exit(options(width = globscipen))

    # Get parameters for placement
    numchars  <- ceiling(stringr::str_count(msg)/2)*2
    borderlen <- numchars*2
    tablen    <- numchars/2 - 1

    # Get objects to print on console
    border <- paste0(c("\n", rep(sym, borderlen), "\n"), collapse="")
    indent <- paste0(rep(" ", tablen), collapse="")
    msg    <- paste0(indent, msg, indent)

    # adjust the notification so the asteriks align
    adj <- paste0(rep(" ", borderlen - stringr::str_count(msg) - 2), collapse="")
    msg <- paste0(border, paste0(sym, msg, adj, sym), border, collapse="")


    # temporarily set the console width then print
    options(width=stringr::str_count(border))
    cat(msg)

    # set console with back to global (we have on.exit in case of error)
    options(width = globscipen)
    if(!is.null(content))
        cat(paste0("\n", content,"\n"))
}


