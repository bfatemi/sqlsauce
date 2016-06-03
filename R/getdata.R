# GetData <- function(db=NULL,
#                     table=NULL,
#                     ll=NULL,
#                     keep=NULL,
#                     drop=NULL,
#                     query=NULL,
#                     time=TRUE,
#                     top=NULL){
#
#     if(is.null(db))
#         stop("No db given. Required to open & close connections")
#
#     # Open connection then close on exit
#     OpenDB(db)
#     on.exit(CloseDB(db))
#
#     if(is.null(query)){
#         vals   <- ll[[1]]
#         cnames <- names(ll)
#
#         # Build SELECT/FROM
#         if(!is.null(top))
#             select <- paste0(paste0("SELECT TOP ", top)," * FROM ", table)
#         else
#             select <- paste0("SELECT * FROM ", table)
#
#         if(!is.null(ll)){
#             cols   <- paste0(" WHERE ", cnames) # Build WHERE
#
#             if(length(vals)==1)
#                 invals <- paste0(" = '", vals, "'")
#             else
#                 invals <- paste0(" in ('", paste(vals,collapse="','"),"')")
#
#             query <- paste0(select, cols, invals) # Final Query
#         }else{
#             query <- select
#         }
#     }
#     dt <- .sqlQuery(db, query, time)
#
#
#     # if keep provided, return selection,
#     # else return cols not in drop, else return all
#     if(!is.null(keep))
#         dt <- dt[, keep, with=F]
#     else if(!is.null(drop))
#         dt <- dt[, colnames(dt)[!colnames(dt) %in% drop], with=FALSE]
#     return(dt)
# }
