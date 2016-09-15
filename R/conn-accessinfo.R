#' Access Information
#'
#' These functions are intended for developers that need access to the connection
#' environment as they develop data products.
#'
#' @section Note:
#' Any database where access to managing connections is required must be configured prior.
#' Future functionality will include the ability to auto-configurate a database.
#'
#' @name AccessInfo
#' @param db A character value representing the name of a database
NULL

#' @describeIn AccessInfo Returns the connection string associated with
#'      a configured database
#' @export
ConnString <- function(db=NULL){
    db <- CheckDB(db)

    if(db == "HNP" | db == "HNI"){
        cnstr <- substitute(paste0("Driver=",    driver,
                                   ";servernode=", servernode,
                                   ";database=", database, usr, pw))
    }else{
        cnstr <- substitute(paste0("Driver=",    driver,
                                   ";Server=",   server,
                                   ";database=", database, usr, pw))
    }

    eval(cnstr, get(paste0("ll_", db), istools::Databases()))
}


#' @describeIn AccessInfo See the access params for configured databases
#' @export
AccessInfo <- function(db=NULL){
    db      <- CheckDB(db)
    string  <- ConnString(db)

    ll.info <- sapply(strsplit(strsplit(string,";")[[1]],"="),
                      function(i){
                          obj <- list(i[2])
                          names(obj) <- i[1]
                          return(obj)})
    DF <- data.frame(ParamValue = as.matrix(ll.info))
    PrintMessage(paste0("Access Parameters: ", db), "+", DF)
}

#' @describeIn AccessInfo See the current set of configured databases
#' @export
ValidDB <- function(){
    sapply(istools::Databases(),"[[","database")
}

#' @describeIn AccessInfo A function to check the argument against a list of
#'      valid DBs. Returns error on a none match. Performs approximate match.
#' @export
CheckDB <- function(db=NULL){
    valid_DBs <- ValidDB()

    if(is.null(db))
        stop("No db name provided", call. = FALSE)
    if(length(db) != 1)
        stop(paste0("CheckDB- length(db) > 1: ", paste0(db, collapse = TRUE)), call. = FALSE)


    tryCatch(
        as.character(match.arg(db, valid_DBs)),
        error = function(e)
            stop("Not valid DB! Use ValidDB() for configured DBs", call. = FALSE)
    )
}

# havingIP <- function() {
#     if (.Platform$OS.type == "windows") {
#         ipmessage <- system("ipconfig", intern = TRUE)
#     } else {
#         ipmessage <- system("ifconfig", intern = TRUE)
#     }
#     validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
#     any(grep(validIP, ipmessage))
# }
#
# is.character(RCurl::getURL("http://infoweb/"))
#
# has_internet <- function(){
#     !is.null(curl::nslookup("r-project.org", error = FALSE))
# }

