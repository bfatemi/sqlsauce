sqlSauce <- function(table=NULL, top=NULL, cols=NULL, where=NULL){
    # conditional assignments
    cols[is.null(cols)] <- "*"
    top[!is.null(top)] <- paste("TOP", top)[!is.null(top)]
    cols <- paste(cols, collapse=",")

    query <- paste("SELECT", top, cols, "FROM", table, where)
    return(query)
}

wsauce <- function(...){
    ll <- substitute(...)

    # lookup table for valid R -> query operators
    opsDT <- data.table(validops = c("|", "||", "&", ">", "<", "<=", ">=", "=="),
                        querytext = c("or", "or", "and", ">", "<", "<=", ">=", "="))

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
                ret <- paste0(lElement, " ", qlogical, " '", rElement, "'")
            else
                ret <- paste0(lElement, " in ('",
                              paste(eval(rElement), collapse = "','"), "')")
            return(ret)
        }
        return(paste(QueryTree(lElement), qlogical, QueryTree(rElement)))
    }
    return(paste0("WHERE ", QueryTree(ll)))
}
