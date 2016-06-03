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
        openConns       <- "No Open Connections"
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
    #.PrintInfo(ll.debug, "Debug", "")
    .PrintInfo(ErrorInfo, "Debug", "Error Information")
    .PrintInfo(Connection, "Debug", "Connection Environment")
    stop("See error information above",call. = F)
}

#' @describeIn DebugInfo A helper function to assist timing operations
#' @param time A date/time object from which point to calculate duration since.
#' @export
timetaken <- function(time){
    return(as.numeric(Sys.time() - time))
}


#' @describeIn DebugInfo A helper function to assist timing operations
#' @param START A boolean value representing whether to start the timer or end the timer. START = FALSE
#'      will display the timer information
#' @export
Timer <- function(START=TRUE){
    if(START){
        tstamp <<- Sys.time()
    }else{
        dur <- difftime(Sys.time(), tstamp, units = "sec")
        print(paste0("Completed duration (sec): ", round(dur, 2)))
        cat("\n")
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
    caller <- gsubfn(replacement = "", x = caller,
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
