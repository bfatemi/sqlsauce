#' Internal Dev Testing and Debugging Functions
#'
#' These are functions that are not exposed to users. They are internal functions designed to assist
#' with debugging errors related to tracing and identifying connections.
#'
#' @section Internal Use Only:
#' Internal functions not intended to be used by developers.
#'
#' @name DebugHelp
#' @param msg A character string representing the message to display within the banner
#' @param nFrame A numeric value representing an index for a frame in the function callstack
#' @param friendly A friendly description of the error message to be displayed
time.env <- new.env(parent = emptyenv())

#' @describeIn DebugHelp A helper function to display sql errors that occur during a query
#' @importFrom data.table data.table setnames
PrintSqlError <- function(msg=NULL, friendly=NULL, nFrame=NULL){
    # statDT <- data.table(sapply(db, ConnAttr), keep.rownames = TRUE)
    # setnames(statDT, c("Database", db))

    ErrorInfo <- list(SQLError    = msg,
                      ErrDesc     = friendly,
                      OccurredIn  = gsub("\\(.*\\)", "", sys.calls()[nFrame]),
                      TraceBack   = .CallStack(nFrame))
    # Connection <- as.list(statDT$RemoteFE)
    # names(Connection) <- statDT$Database

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
    stop(paste0("SQL ERROR: ", msg), call. = FALSE)
}

#' @describeIn DebugHelp A helper function to assist timing operations
#' @param time A date/time object from which point to calculate duration since.
#' @export
xtimetaken <- function(time){
    return(round(as.numeric(difftime(Sys.time(), time, units = "sec")), 1))
}


#' @describeIn DebugHelp A helper function to assist timing operations
#' @param START A boolean value representing whether to start the timer or end the timer. START = FALSE
#'      will display the timer information
#' @param print A boolean indicating whether to print output on the console in addition to returning a numeric
#'      value representing the duration in seconds
Timer <- function(START=TRUE, print=FALSE){
    if(START){
        tmp <- Sys.time()
        assign("tstamp",tmp, time.env)
    }else{
        dur <- round(as.numeric(difftime(Sys.time(), get("tstamp", time.env), units = "sec")), 2)

        if(print){
            cat(paste0("Completed duration (sec): ", round(dur, 2)))
            cat("\n")
        }
        return(dur)
    }
}

#' @describeIn DebugHelp A helper function to extract the relevant and helpful portion of the function callstack
.CallStack <- function(nFrame){
    callers <- sys.calls()

    i <- callers

    ss_callers <- unlist(lapply(callers, function(i){
        currcall <- as.character(i[[1]])
        nsFuns <- ls("package:sqlsauce")

        currcall <- stringr::str_replace(currcall, "sqlsauce::", "")

        currcall[which(currcall %in% nsFuns)]
    }))
    paste(ss_callers, collapse = " >> ")
}

#' @describeIn DebugHelp A function to parse the function call stack and identify a caller at the current postion
#'      or at the position identified by "nFrame"
.Caller <- function(nFrame=NULL){
    if(is.null(nFrame))
        nFrame <- sys.nframe()-2
    caller <- as.character(sys.calls()[nFrame])
    caller <- gsubfn::gsubfn(replacement = "", x = caller,
                     pattern = "\\(.*\\)")
    paste(caller, collapse = " => ")
}


#' @describeIn DebugHelp A function to elegantly print a notification banner on the console
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
        if(class(content) %in% c("matrix", "data.frame"))
            print(content, print.gap = TRUE, quote = FALSE)
        else
            cat(paste0("\n", content,"\n"))

}


#' @describeIn DebugHelp A generic and intuitive error handling function (tryCatch wrapper)
#' @param code The code to run
#' @param emsg A character string representing the error message to display
#' @param wmsg A character string representing the warning message to display
#' @param mmsg A character string representing the generic message to display
#' @param pattern An optional regex pattern used to parse the thrown error/warning/message
#' @export
RunCatch <- function(code, emsg = NULL, wmsg = NULL, mmsg = NULL, pattern=NULL){
    Parse <- function(msg){
        if(is.null(pattern)) return(msg)
        gsub(pattern, "", msg, perl = TRUE)
    }

    efun <- function(c){
        if(is.null(emsg))
            emsg <- "GENERIC ERROR"
        PrintMessage(emsg, sym = "=")
        stop(Parse(c$message), call. = FALSE)
    }

    wfun <- function(c){
        if(is.null(wmsg))
            wmsg <- "GENERIC WARNING"
        PrintMessage(wmsg, sym = "=")
        stop(Parse(c$message), call. = FALSE)
    }

    mfun <- function(c){
        if(is.null(mmsg))
            mmsg <- "GENERIC MESSAGE"
        PrintMessage(mmsg, sym = "=")
        stop(Parse(c$message), call. = FALSE)
    }

    tryCatch(code,
             error = function(c) efun(c),
             warning = function(c) wfun(c),
             message = function(c) mfun(c)
    )
}


#' @describeIn DebugHelp A function to assist in timing code execution
#' @param successMsg A message to display after successful execution
#' @param Catch A boolean indicating whether to run the code using RunCatch
#' @param ... Arguments to pass to RunCatch if boolean \code{Catch} is provided as TRUE
#' @export
RunTimer <- function(code, successMsg=NULL, Catch=FALSE, ...){

    if(is.null(successMsg))
        successMsg <- "Execution Complete"

    Timer()
    RunCatch(eval(code), ...)
    dur <- Timer(START = FALSE)

    PrintMessage(msg = successMsg, "+")
    cat(paste0("Duration of call (seconds): ", dur))
}


# #' @describeIn DebugHelp A function to retrieve a character list of all
# #'      function names within a given package
# #' @param package expr that is the name of the package
# #' @param all.names A bool
# #' @param pattern A pattern to subset for particular functions
# lsp <- function(package, all.names = FALSE, pattern)
# {
#     package <- deparse(substitute(package))
#     ls(
#         pos = paste("package", package, sep = ":"),
#         all.names = all.names,
#         pattern = pattern
#     )
# }



