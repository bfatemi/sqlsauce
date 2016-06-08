#' @title Open and Close Connections
#'
#' @description A number of functions to handle connections. These functions are intended for developers
#' that need access to the connection environment as they develop data products.
#'
#' @details Any database where access to managing connections is required must be configured prior.
#'
#' A connection object can have multiple states:
#' \itemize{
#'  \item Open/closed
#'  \item Not Open/Not Closed
#' }
#'
#' The latter two ("Not Open" and "Not Closed") refer to states where a connection object may exist but has timeout,
#' or when the connection object does not exist at all. For example, the first time a connection is initialized, it
#' becomes "Open". Prior to that, it is not open or closed. After a request is received to close the connection, the
#' status is changed to "closed". From there on, the connection object exists in the pool until it is garbage collected
#' or explicitly removed.
#'
#' @section Internal Functions:
#' These internal functions are wrappers for package:RODBC functions. The wrappers are there primarily for custom
#' error handling and are not intended to be exposed to developers or users.
#'
#' @param db A character value naming the database to use
#' @param verbose A boolean indicating whether to print a status report on the console








########################################################################################
# RODBC open/close connection wrappers
########################################################################################

# #' @describeIn OpenDB Internal RODBC wrapper
# #' @importFrom RODBC odbcDriverConnect
# .odbcOpen <- function(db){
#
#     emsg <- "CONNECTION FAILED"
#     pat <- ".*(?<=])"
#
#     RunCatch(
#         odbcDriverConnect(ConnString(db), readOnlyOptimize = T),
#         emsg,
#         emsg,
#         pattern = pat
#     )
# }
#
# #' @describeIn OpenDB Internal RODBC wrapper
# #' @importFrom RODBC odbcClose
# .odbcClose <- function(db) {
#     cnObj <- GetConn(db)
#
#     if(is.null(cnObj)){
#         msg <- paste0("Could not close. Connection to ", db, " does not exist")
#         emsg <- "CLOSE CONNECTION FAILED"
#         RunCatch(stop(msg), emsg)
#     }else{
#         if(IsClosed(db)){
#             return(0)
#         }else{
#             emsg <- "UNK:CLOSE CONNECTION ERROR"
#             RunCatch(odbcClose(cnObj), emsg)
#             return(1)
#         }
#     }
# }
#
# #' @describeIn OpenDB Internal RODBC wrapper
# #' @importFrom RODBC odbcReConnect
# .odbcReOpen <- function(db) {
#     cnObj <- GetConn(db)
#     nFrame <- sys.nframe()
#     tryCatch({
#         return(odbcReConnect(cnObj))
#     }, error = function(e){
#         Clean(db)
#         DebugInfo("Check network or access", nFrame)
#     })
# }
