#' The Sauce for Dishing Data
#'
#' The function \code{QuerySauce} and it's helper \code{WhereSauce} provide the
#' the ability to effortlessly build a valid sql query string
#'
#' FILL IN DETAIL HERE. See example workflows
#'
#' @param tbl A character string that represents the sql tablename to query
#' @param top A numeric value indicating whether to return the top N rows from the
#'      query execution
#' @param cols An optional character vector specifying columns to return. If not
#'      provided, all columns will be returned
#' @param wh The output from a call to WhereSauce. A character string representing
#'      a valid where clause
#' @param where Deprecated. Use \code{wh} instead.
#' @param ... Arguments to create the WHERE statement or as in the case of 
#'      the \code{not} function: One or more logical operations to produce the negated syntax for sql
#' @param verbose A boolean indicating whether to print the constructed query
#' @param x A logical operation in the form of an expression
#' @param string A string to concat to the supplied where statement
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # STEP 1: Name the configured database
#' # STEP 2: Name the table to query
#' # STEP 3: Get the WHERE sauce (optional ingredient for the query sauce)
#' # STEP 4: Get the Query sauce (main ingredient for the data dish)
#' #----------------------------------------------------------------------------
#'
#' db <- "RemoteFE"
#' tbl <- "SFDC.dbo.PROCEDURES"
#'
#' #----------------------------------------------------------------------------
#' # Get procedures of specified subjects that occurred since 2015
#' #----------------------------------------------------------------------------
#' psub <- c("HPB", "dVP", "dVC", "dVL") # for WHERE sauce
#'
#' wh    <- WhereSauce(ProcedureSubject == psub & Year(CreateDate) > 2015)
#' query <- QuerySauce(tbl, top = 1000, where = wh)
#'
#' DT    <- xQuery(db, query)
#'
#' #----------------------------------------------------------------------------
#' # Alternate workflow:
#' #   Open/Closing is handled by xQuery unless db is open. In which case,
#' #   the user can control when to open and close connections
#' #----------------------------------------------------------------------------
#'
#' OpenDB(db)  # Open connection
#'
#' # .... Do work
#'
#' CloseDB(db) # Close connection
#' }
#'
#' @describeIn QuerySauce A function to build the query
#' @importFrom data.table data.table
QuerySauce <- function(tbl=NULL, top=NULL, cols=NULL, wh=NULL, where=NULL, verbose=FALSE){
    if(is.null(wh) & !is.null(where)){
        warning("Parameter 'where' is deprecated. Please use parameter 'wh' instead")
        wh <- where
    }

    cols[is.null(cols)] <- "*"
    top[!is.null(top)] <- paste("TOP", top)[!is.null(top)]
    cols <- paste(cols, collapse=",\n\t")

    sauce <- paste("SELECT", top, cols, "\nFROM", tbl, wh, "\n")

    if(TRUE){
        cat("\n
+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+
|G|e|n|e|r|a|t|e|d| |Q|u|e|r|y|
+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+
            \n")
        cat(sauce)
        cat("\n")
    }
    return(sauce)
}

#' @describeIn QuerySauce A function to the WHERE clause that becomes an input into
#'      \code{QuerySauce}
#' @export
#' @importFrom data.table data.table
WhereSauce <- function(...){
    # wheresauce needs to evaluate the right side of this

    ll <- substitute(...)
    #opsDT <- OpsDT()

    sauce <- paste0("\nWHERE ", QueryTree(ll))
    return(sauce)
}

#' @describeIn QuerySauce Function to translate R operators to sql ones
#' @importFrom data.table data.table
OpsDT <- function(){
    ops <- data.table(validops = c("|", "||", "&", "&&", "!=", ">", "<", "<=", ">=", "=="),
               querytext = c("OR", "OR", "AND", "AND",  "!=", ">", "<", "<=", ">=", "="))
    return(ops)
}

#' @describeIn QuerySauce Function to build query
#' @param ll A list object that wraps the R expressions in order to translate to sql where conditions
#' @export
#' @importFrom data.table data.table
QueryTree <- function(ll){

    if(length(ll)==0)
        stop("condition length 0", call. = F)

    opsDT <- OpsDT()


    ind <- which(opsDT$validops %in% as.character(ll$expr[[1]]))
    qlogical <- opsDT[ind, querytext]

    # elements to the left and right of the operator we just pulled out
    #rElement <- ll[-1][[length(ll[-1])]]
    # lElement <- ll[-1][[1]]

    rElement <- ll$expr[-1][[length(ll$expr[-1])]]
    lElement <- ll$expr[-1][[1]]

    if(length(lElement) == 1 | grepl("^Year(.+)$", deparse(lElement), perl = T)[1]){
        lElement <- deparse(lElement)
        if(length(eval(rElement, ll$env)) == 1)
            ret <- paste0(lElement, " ", qlogical, " '", eval(rElement), "'")
        else
            ret <- paste0(lElement, " IN ('",
                          paste(eval(rElement, ll$env), collapse = "','"), "')\n")
        return(ret)
    }
    return(paste(QueryTree(lElement), qlogical, QueryTree(rElement)))
}
# QueryTree <- function(ll){
#     if(length(ll)==0)
#         stop("condition length 0", call. = F)
#
#     opsDT <- OpsDT()
#
#     ind <- which(opsDT$validops %in% as.character(ll[[1]]))
#     qlogical <- opsDT[ind, querytext]
#
#     # elements to the left and right of the operator we just pulled out
#     rElement <- ll[-1][[length(ll[-1])]]
#     lElement <- ll[-1][[1]]
#
#
#     if(length(lElement) == 1 | grepl("^Year(.+)$", deparse(lElement), perl = T)[1]){
#         lElement <- deparse(lElement)
#         if(length(eval(rElement)) == 1)
#             ret <- paste0(lElement, " ", qlogical, " '", eval(rElement), "'")
#         else
#             ret <- paste0(lElement, " IN ('",
#                           paste(eval(rElement), collapse = "','"), "')\n")
#         return(ret)
#     }
#     return(paste(QueryTree(lElement), qlogical, QueryTree(rElement)))
# }


#' @describeIn QuerySauce A function to retrieve the column names associated with the
#'      expressions given in the WhereSauce args
#' @export
ColSauce <- function(...){
    slice <- eval(substitute(alist(...)))

    f <- function(ll){
        if(length(ll)==0) stop("condition length 0", call. = F)

        lElement <- ll[-1][[1]]
        if(length(lElement) == 1)
            return(lElement)

        return(f(lElement))
    }
    return(lapply(slice, f))
}

#' @describeIn QuerySauce A helper to append arbitrary AND conditions to the return object of \code{Wsauce}. See examples
#' @export
AND <- function(wh, string){
    paste0(wh, " AND ", string)
}

#' @describeIn QuerySauce A helper to append arbitrary OR conditions to the return object of \code{Wsauce}. See examples
#' @export
OR <- function(wh, string){
    paste0(wh, " OR ", string)
}


#' @describeIn QuerySauce A function to handle not expressions
#' @import data.table
#' @export
not <- function(...){
    d <- pryr::dots(...)[[1]]

    name <- ch_op(d)

    if(name == "is.null")
        return(paste0(lop(d), " IS NOT NULL"))

    if(name == "is.na")
        return(paste0(lop(d), " != NA"))

    col <- lop(d)
    val <- rop(d)

    if(is.na(val) | val %in% c("NA", "na", "Na", "n/a", "N/A"))
        return(paste0(col, " != NA"))

    notOps <- data.table(A = c("!=", ">", "<", "<=", ">=", "=="),
                         B = c("==", "<", ">", ">=", "<=", "!="))

    qlogical <- notOps[which(notOps$A == name), B]

    if(length(eval(val)) == 1)
        ret <- paste0(col, " ", qlogical, " '", eval(val), "'")
    else
        ret <- paste0(col, "NOT IN ('", paste(eval(val), collapse = "','"), "')\n")
    return(ret)
}


#' @describeIn QuerySauce An alias to QuerySauce
#' @export
Qsauce <- QuerySauce

#' @describeIn QuerySauce An alias to WhereSauce
#' @export
#Wsauce <- WhereSauce
Wsauce <- function(...){
    # wheresauce needs to evaluate the right side of this

    ll <- lazyeval::lazy(...)
    sauce <- paste0("\nWHERE ", QueryTree(ll))
    return(sauce)
}

#' @describeIn QuerySauce Get operator
op <- function(x) x[[1]]

#' @describeIn QuerySauce Get character op
ch_op <- function(x) as.character(x[[1]])

#' @describeIn QuerySauce Get left side op
lop <- function(x) x[[2]]

#' @describeIn QuerySauce Get character left side op
clop <- function(x) as.character(x[[2]])

#' @describeIn QuerySauce Get right side op
rop <- function(x) x[[3]]

#' @describeIn QuerySauce Get character right side op
crop <- function(x) as.character(x[[3]])

#' @describeIn QuerySauce Reverse operations
revop <- function(x) pryr::make_call(x[[1]], x[[3]], x[[2]])
