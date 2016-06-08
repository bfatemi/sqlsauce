




# #' @describeIn AboutConnection Convenience function to provide quick status for a closed db
# #' @inheritParams OpenDB
# SeeConnOpen <- function(db=NULL){
#     list(ConnStatus = "Closed/Inactive",
#          InitiatedBy = OpenedBy(db),
#          TimeInitiated = TimeOpened(db),
#          TimeClosed = TimeClosed(db),
#          DurationClosed = CloseDuration(db))
# }

# #' @describeIn AboutConnection Convenience function to provide quick status for any db
# #' @inheritParams OpenDB
# #' @export
# ConnAttr <- function(db=NULL){
#     list(Open = IsOpen(db),
#          Closed = IsClosed(db),
#          Active = IsActive(db),
#          InitiatedBy = OpenedBy(db),
#          LastOpenedBy = RequestedBy(db),
#          ClosedBy = ClosedBy(db),
#          TimeInitiated = TimeOpened(db),
#          TimeLastRequest = TimeLastRequest(db),
#          TimeClosed = TimeClosed(db),
#          DurationOpen = OpenDuration(db),
#          DurationSinceRequest = DurSinceLastRequest(db),
#          DurationClosed = CloseDuration(db),
#          AccessCount = AccessCount(db))
# }




# #' @describeIn AboutConnection A function can be "inactive" which means it has never been initiated and neither open nor closed
# #' @inheritParams OpenDB
# #' @export
# IsActive <- function(db=NULL){
#     if(IsOpen(db) | IsClosed(db))
#         return(TRUE)
#     return(FALSE)
# }
#
# #' @describeIn AboutConnection Returns timestamp of when connection was last closed
# #' @inheritParams OpenDB
# #' @export
# TimeClosed <- function(db=NULL){
#     if(!IsClosed(db)){
#         return(NA)
#     }
#     attr(GetConn(db), "TimeClosed")
# }
#
#
# #' @describeIn AboutConnection Returns timestamp of when connection was first opened
# #' @inheritParams OpenDB
# #' @export
# TimeOpened <- function(db=NULL){
#     attr(GetConn(db), "TimeOpened")
# }
#
# #' @describeIn AboutConnection Returns timestamp of when connection was last requested (last opened)
# #' @inheritParams OpenDB
# #' @export
# TimeLastRequest <- function(db=NULL){
#     attr(GetConn(db), "TimeRequested")
# }
#
#
# #' @describeIn AboutConnection Returns BOOL indicating whether status is "Open"
# #' @inheritParams OpenDB
# #' @export
# IsOpen <- function(db=NULL){
#     ifelse(!!sum(attr(GetConn(db), "Status") == "Open"),T,F)
# }
#
#
# #' @describeIn AboutConnection Returns BOOL indicating whether status is "Closed"
# #' @inheritParams OpenDB
# #' @export
# IsClosed <- function(db=NULL){
#     ifelse(!!sum(attr(GetConn(db), "Status") == "Closed"),T,F)
# }
#
# #' @describeIn AboutConnection Returns a string indicating the custom query function or user that make the FIRST opened connection to
# #' specified database
# #' @inheritParams OpenDB
# #' @export
# OpenedBy <- function(db=NULL){
#     if(!IsOpen(db))
#         return(NA)
#     opener <- attr(GetConn(db), "OpenedBy")
#     if(grepl("OpenDB",opener)){
#         return("Global Connection (user script)")
#     }else{
#         return(opener)
#     }
# }
#
# #' @describeIn AboutConnection Returns a string indicating the custom query function or user that make the LAST opened connection to
# #' specified database
# #' @inheritParams OpenDB
# #' @export
# ReOpenedBy <- function(db=NULL){
#     if(!IsOpen(db))
#         return(NA)
#     opener <- attr(GetConn(db), "ReOpenedBy")
#     if(grepl("OpenDB",opener)){
#         return("Global Connection (user script)")
#     }else{
#         return(opener)
#     }
# }
#
# #' @describeIn AboutConnection Returns a string indicating the custom query function or user most recently closed a connection
# #' to the specified database
# #' @inheritParams OpenDB
# #' @export
# ClosedBy <- function(db=NULL){
#     if(!IsClosed(db))
#         return(NA)
#     attr(GetConn(db), "ClosedBy")
# }
#
# #' @describeIn AboutConnection Returns a string indicating the custom query function or user that sent a request to make a connection
# #' to the specified database. A "request" simply means that an "open" request was made to a database that was already open.
# #' @inheritParams OpenDB
# #' @export
# RequestedBy <- function(db=NULL){
#     if(!IsOpen(db))
#         return(NA)
#     attr(GetConn(db), "RequestedBy")
# }
#
# #' @describeIn AboutConnection Returns the duration in seconds that an open connection has existed for a specified db
# #' @inheritParams OpenDB
# #' @export
# OpenDuration <- function(db=NULL){
#     if(!IsOpen(db))
#         return(NA)
#     xtimetaken(attr(GetConn(db), "TimeOpened"))
# }
#
# #' @describeIn AboutConnection Returns the duration in seconds since the last request was made to a specifed database
# #' @inheritParams OpenDB
# #' @export
# DurSinceLastRequest <- function(db=NULL){
#     if(!IsOpen(db))
#         return(NA)
#     xtimetaken(attr(GetConn(db), "TimeRequested"))
# }
#
# #' @describeIn AboutConnection Returns the duration in seconds since the status of the specifed database was changed to "close"
# #' @inheritParams OpenDB
# #' @export
# CloseDuration <- function(db=NULL){
#     if(!IsClosed(db))
#         return(NA)
#     xtimetaken(attr(GetConn(db), "TimeClosed"))
# }
#
# #' @describeIn AboutConnection Returns a numeric that represents the count of requests that were made to a specified db
# #' @inheritParams OpenDB
# #' @export
# AccessCount <- function(db=NULL){
#     if(!IsOpen(db))
#         return(NA)
#     attr(GetConn(db), "AccessCount")
# }
