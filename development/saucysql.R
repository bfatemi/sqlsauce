cSauce <- function(...){
    slice <- pryr::named_dots(...)

    cnames <- NULL
    if(hasArg("col")){
        cnames <- eval(slice[names(slice) == "col"][[1]]) # extract out the character vector of names
        slice <- slice[names(slice) != "col"]
        if(!length(slice)) return(cnames)
    }

    sCalls <- slice[which(sapply(slice, is.call))]

    c(cnames, as.character(unlist(lapply(sCalls, lop))))
}


QueryTree <- function(ll){
    if(length(ll)==0)
        stop("condition length 0", call. = F)

    opsDT <- OpsDT()

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
library(data.table)

extract_op <- function(...){
    DF <- fexpr::dots_unpack(...)
    exp <- DF[DF[, "name"] == "", "expr"]
    return(exp)
}


#fexpr::arg_expr(">", )


extract_op <- function(...){
    DF <- fexpr::dots_unpack(...)
    xpr <- DF[DF[, "name"] == "", "expr"]
    return(xpr[unlist(lapply(xpr, e3))])
}


# number unique
nunique <- pryr::compose(length, unique)

# length equals 3
e3 <- pryr::compose(pryr::partial(`==`, 3), length)

lenx <- function(...){
    d <- dots(...)[[1]]
    f <- pryr::compose(pryr::partial(eval(op(d)), lop(d)), length)
    f(eval(rop(d)))
}

lenx(x > 6)

extract_op(wfjwieowf >= 123, col = "Bobby", col>1, blah = werw, w>1, cola, colb, test=c("cola", "colb"))


op <- function(x) x[[1]]

cop <- function(x) as.character(x[[1]])

lop <- function(x) x[[2]]

clop <- function(x) as.character(x[[2]])

rop <- function(x) x[[3]]

crop <- function(x) as.character(x[[3]])

revop <- function(x) pryr::make_call(x[[1]], x[[3]], x[[2]])


cSauce(a >= 123, col = c("Bobby", "jeff"), Arm>1)

# f <- function(...) return(pryr::named_dots(...))
# sCalls <- f(a > b)
#
# pryr::subs(a + b, list("+" = sCalls[[1]]))
#
# pryr::substitute_q(sCalls[[1]], env = slice)
# pryr::partial(sCalls[[1]])
# pryr::modify_lang(sCalls, pryr::subs)
#
# fe <- function(...){
#     return(fexpr::dots_unpack(...))
#     return(dots_expressions(...))
# }
# fe(wfjwieowf >= 123, col = "Bobby", col>1, blah = werw, w>1, cola, colb, test=c("cola", "colb"))
# fexpr::dots_names(as.dots(slice), )
#  blah = werw, w>1, cola, colb, test=c("cola", "colb"))
