#' Tools to explore the tables or columns in a database
#'
#' Functions assist with exploring db objects at the abstraction layer, and general
#' exploration of the Morpheus data
#'
#' @param db A character value naming the database (e.g. "Morpheus")
#' @param closeConn A boolean indicating whether this function to close the connection to "db". Default is TRUE.
#' @param print A boolean indicating whether to print information on the R console
#' @param tbl_Pattern A regex pattern to filter on desired tables
#'
#' @examples
#' \dontrun{
#' # Let's explore what data is available in Morpheus
#'
#' # Available tables?
#' TableInfo(db = "Morpheus")
#' TableInfo(db = "RemoteFE", print = T)
#'
#' # What are the keys for each table?
#' PrimaryKey("Morpheus")
#'
#' # Get key for a specific table
#' PrimaryKey("Morpheus", "MainLog")
#'
#' # Let's explore MainLog columns
#' ColumnInfo(tables = "MainLog")
#'
#' # Shortcuts/quick view
#' Columns()
#' Columns(table = "ProcedureSummaries")
#' }
#' @describeIn TableInfo Given a database, get all databases on same server
#' @export
#' @importFrom data.table data.table set setnames setkeyv rbindlist
TableInfo <- function(db=NULL, print=FALSE) {
    dropCols     <- c("TABLE_SCHEM", "TABLE_TYPE", "REMARKS")
    keepCols     <- c("TABLE_CAT", "TABLE_NAME")
    friendlyName <- c("Database",  "Table")

    if(is.null(db))
        db <- sapply(Databases(),"[[","database")

    fn_fetch <- function(i){
        print(paste0("Fetching db: ", i))
        OpenDB(i)                          # OpenDB will check valid db name

        dt <- getTables(i)

        if(nrow(dt)==0)
            return(NULL)

        set(dt, j = dropCols, value = NULL)
        setnames(dt, friendlyName)
        setkeyv(dt, c("Database", "Table"))

        return(PrimaryKey(i, dt$Table))
    }

    res <- rbindlist(lapply(db, fn_fetch), fill=TRUE)

    if(print){
        print(res[,.(NumberOfTables = .N), by=Database], row.names = FALSE)
        cat("\n\n")
        print(res[1:min(15,nrow(res)),], row.names = FALSE)
    }

    return(res)
}


#' @describeIn TableInfo Will present a relation between tables in a given db
#' @export
#' @importFrom stringr str_detect
Relate <- function(db=NULL, tbls=NULL, tbl_Pattern=NULL){
    resdt <- TableInfo(db)

    if(is.null(tbls) & is.null(tbl_Pattern))
        return(resdt)

    dat <- resdt[Table %in% tbls | stringr::str_detect(Table, tbl_Pattern)]
    wresdt <- dcast(dat,
                    Database + Table ~ PrimaryKey,
                    value.var = "PK_Position")
    setnames(wresdt, "NA", "NO_KEY")
    return(wresdt)
}



# Relate("RemoteFE")

#' @describeIn TableInfo A function to retrieve information about one or more table's primary key column
#' @param tbls A character vector of table names belonging to the database specified by "db"
#' @export
#' @importFrom data.table data.table set setnames setkeyv rbindlist
# tbls <- dt$Table
PrimaryKey <- function(db=NULL, tbls=NULL){
    dropCols     <- c("TABLE_SCHEM", "PK_NAME")
    keepCols     <- c("TABLE_CAT", "TABLE_NAME", "COLUMN_NAME","KEY_SEQ")
    friendlyName <- c("Database", "Table", "PrimaryKey", "PK_Position")

    OpenDB(db)
    on.exit(CloseDB(db))
    if(is.null(tbls)) stop("No tables supplied to PrimaryKey", call. = FALSE) #tbls <- TableInfo(db)[, unique(Table)]

    # Helper fn to run through each table iteratively
    f <- function(i){
        tmp <- getPrimaryKey(db, i)

        if(nrow(tmp)){
            # tmp2 <- tmp[, !dropCols, with=FALSE]
            set(tmp, j = dropCols, value = NULL)    # Drop cols by ref
            setnames(tmp, keepCols, friendlyName)   # Change to friendly names
        }else{
            tmp <- data.table(Database=db, Table=i)
        }
        return(tmp)
    }

    emsg <- "Error in PrimaryKey()"
    dt <- RunCatch(rbindlist(lapply(tbls, f), fill=TRUE), emsg, emsg)

    emsg <- "Error setting keys in PrimaryKey"
    RunCatch(setkeyv(dt, c("Database", "Table")), emsg, emsg)

    # release easydata on cran then uncomment this
    # return(easydata::CleanCols(dt))
    return(dt)
}

#' @describeIn TableInfo A function to detailed information about a given table's columns.
#'      If argument "tbls" not provided, retrive information about all tbls in the specified database
#' @export
#' @importFrom data.table data.table set setnames setkeyv rbindlist
ColumnInfo <- function(db=NULL, tbls=NULL, closeConn=FALSE, print=FALSE) {
    dropCols     <- c("TABLE_SCHEM", "DATA_TYPE", "BUFFER_LENGTH",
                      "NUM_PREC_RADIX", "NULLABLE", "REMARKS",
                      "COLUMN_DEF", "SQL_DATA_TYPE","SQL_DATETIME_SUB",
                      "CHAR_OCTET_LENGTH", "SS_DATA_TYPE")
    keepCols     <- c("TABLE_CAT", "TABLE_NAME",  "COLUMN_NAME",
                      "TYPE_NAME", "COLUMN_SIZE", "DECIMAL_DIGITS",
                      "ORDINAL_POSITION", "IS_NULLABLE")
    friendlyName <- c("Database",  "Table", "Column", "Type", "ColumnSize",
                      "DecimalDigits", "Position","IsNullable")

    # If not provided, run for all dbo tbls
    if(is.null(tbls)) tbls <- "%"
    if(is.null(db)) stop("Please provide db name")

    OpenDB(db)
    dt <- getColumns(db, tbls)                     # run query for col names

    set(dt, j = dropCols, value = NULL)
    setnames(dt, keepCols, friendlyName)
    setkeyv(dt, c("Database", "Table"))

    # Add Column for IsKey
    set(dt, j="IsKey", value = "NO")

    pk <- PrimaryKey(db, unique(dt$Table))
    dt[Column %in% pk[, PrimaryKey], IsKey:="YES"]

    if(print){
        cat("\n")
        print(dt[,.(NumberOfCols = .N), by="Database,Table"], row.names = FALSE)
        cat("\n\n")
        n <- min(10,nrow(dt))
        print(dt[1:n, .(Table, Column, Type, Position, IsKey)], row.names = FALSE)
        if(n < nrow(dt))
            cat("--------------------- TRUNCATED ---------------------\n")
    }
    return(dt)
}

#' @describeIn TableInfo A function that serves as a quick lookup of columns in a table. Defaults to table
#'      "MainLog" in database "Morpheus".
#' @param return A boolean indicating whether to return objects in memory. Defaults to FALSE which means
#'      information is simply printed on the R console and not captured. Ideal default for quick lookups.
#' @export
#' @importFrom data.table data.table set setnames setkeyv rbindlist
Columns <- function(db=NULL, tbls=NULL, return=FALSE, closeConn=TRUE){
    cat("\nDatabase:", db)
    cat("\nDB Table:", tbls)
    cat("\n\n")

    dt <- ColumnInfo(db, tbls, closeConn, print = FALSE)

    set(dt, j=c("Database",
                "Table",
                "ColumnSize",
                "DecimalDigits",
                "IsNullable"), value=NULL)
    if(return)
        return(dt)
    print(dt, row.names = TRUE)
}

#' @describeIn TableInfo An internal wrapper around sqlTables
#' @importFrom RODBC sqlTables
#' @importFrom data.table data.table set setnames setkeyv rbindlist
getTables <- function(db=NULL) {
    OpenDB(db)
    cnObj <- GetConn(db)

    dt <- RunCatch(sqlTables(channel   = cnObj, tableType = "TABLE"),
                   "Error in getTables")
    dt <- data.table(dt)
    return(dt)
}

#' @describeIn TableInfo An internal wrapper around sqlColumns
#' @importFrom RODBC sqlColumns
#' @importFrom data.table data.table set setnames setkeyv rbindlist
getColumns <- function(db=NULL, tbls) {
    cnObj  <- GetConn(db)

    dt <- RunCatch(sqlColumns(channel = cnObj,
                              sqtable = tbls), "Error in getColumns")
    dt <- data.table(dt)
    return(dt)
}

#' @describeIn TableInfo An internal wrapper around sqlPrimaryKeys
#' @importFrom RODBC sqlPrimaryKeys
#' @importFrom data.table data.table set setnames setkeyv rbindlist
getPrimaryKey <- function(db=NULL, tbls) {
    # OpenDB(db)
    cnObj  <- GetConn(db)
    # tbls = "SAPTBL.ZCOPA_XRATE_DATA_04092016"

    dt <- tryCatch({
        sqlPrimaryKeys(channel = cnObj, sqtable = tbls)
    }, error = function(c){
        if(stringr::str_detect(c$message, "table not found")){
            a <- as.data.table(list(Database=NULL, Table=NULL))
            return(a)
        }else{
            stop(c$message, call. = FALSE)
        }
    })

    dt <- as.data.table(dt)
    return(dt)
}
