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
    cnstr <- substitute(paste0("Driver=",    driver,
                               ";Server=",   server,
                               ";database=", database, usr, pw))
    eval(cnstr, get(paste0("ll_", db), Databases()))
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
    as.character(sapply(Databases(),"[[","database"))
}

#' @describeIn AccessInfo A function to check the argument against a list of
#'      valid DBs. Returns error on a none match. Performs approximate match.
#' @export
CheckDB <- function(db=NULL){
    valid_DBs <- ValidDB()

    if(is.null(db))
        stop("No db name provided", call. = FALSE)

    tryCatch(
        as.character(match.arg(db, valid_DBs)),
        error = function(e)
            stop("Not valid DB! Use ValidDB() for configured DBs", call. = FALSE)
    )
}

#' @describeIn AccessInfo internal function
Databases <- function(){
    ll_all <- list(
        ll_Morpheus =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "Morpheus",
                 usr      = ";trusted_connection=true",
                 pw       = NULL),
        # ll_MorpheusDev =
        #     list(driver   = "SQL Server",
        #          server   = "sccdsql01.dv.local,1515",
        #          database = "Morpheus",
        #          usr      = ";uid=MorpheusAdmin",
        #          pw       = ";pwd=MorpheusAdmin2015"),
        ll_RemoteFE =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "RemoteFE",
                 usr      = ";trusted_connection=true",
                 pw       = NULL),
        ll_ProcedureCategorization =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "ProcedureCategorization",
                 usr      = ";trusted_connection=true",
                 pw       = NULL),
        ll_USMOperationsDB =
            list(driver   = "SQL Server",
                 server   = "snganalytics\\db1",
                 database = "USMOperationsDB",
                 usr      = ";uid=USMUser",
                 pw       = ";pwd=USMUser2016#!"))
    return(ll_all)
}
