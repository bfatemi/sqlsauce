#' The Sauce for Dishing Data
#'
#' The function \code{QuerySauce} and it's helper \code{WhereSauce} provide the
#' the ability to effortlessly build a valid sql query string
#'
#' FILL IN DETAIL HERE. See example workflows
#'
#' @describeIn QuerySauce A function to build the query
#' @param tbl A character string that represents the sql tablename to query
#' @param top A numeric value indicating whether to return the top N rows from the
#'      query execution
#' @param cols An optional character vector specifying columns to return. If not
#'      provided, all columns will be returned
#' @param where The output from a call to WhereSauce. A character string representing
#'      a valid where clause
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
#' @importFrom data.table data.table
QuerySauce <- function(tbl=NULL, top=NULL, cols=NULL, where=NULL){
    cols[is.null(cols)] <- "*"
    top[!is.null(top)] <- paste("TOP", top)[!is.null(top)]
    cols <- paste(cols, collapse=",\n\t")

    sauce <- paste("SELECT", top, cols, "\nFROM", tbl, where, "\n")

    PrintMessage("Generated Query", content = sauce)
    return(sauce)
}

#' @describeIn QuerySauce A function to the WHERE clause that becomes an input into
#'      \code{QuerySauce}
#' @export
#' @importFrom data.table data.table
WhereSauce <- function(...){
    ll <- substitute(...)

    # lookup table for valid R -> query operators
    opsDT <- data.table(validops = c("|", "||", "&", "&&", "<>", "><",">", "<", "<=", ">=", "=="),
                        querytext = c("OR", "OR", "AND", "AND", "NOT", "NOT",">", "<", "<=", ">=", "="))

    QueryTree <- function(ll){
        if(length(ll)==0)
            stop("condition length 0", call. = F)

        ind <- which(opsDT$validops %in% as.character(ll[[1]]))
        qlogical <- opsDT[ind, querytext]

        # elements to the left and right of the operator we just pulled out
        rElement <- ll[-1][[length(ll[-1])]]
        lElement <- ll[-1][[1]]


        if(length(lElement) == 1 | grepl("^Year(.+)$", deparse(lElement), perl = T)[1]){
            lElement <- deparse(lElement)
            if(length(eval(rElement)) == 1)
                ret <- paste0(lElement, " ", qlogical, " '", eval(rElement), "'")
            else
                ret <- paste0(lElement, " IN ('",
                              paste(eval(rElement), collapse = "','"), "')\n")
            return(ret)
        }
        return(paste(QueryTree(lElement), qlogical, QueryTree(rElement)))
    }

    sauce <- paste0("\nWHERE ", QueryTree(ll))
    return(sauce)
}
