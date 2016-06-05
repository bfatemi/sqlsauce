#' Tools to explore the tables or columns in a database
#'
#' Functions assist with exploring db objects at the abstraction layer, and general
#' exploration of the Morpheus data
#'
#' @param db A character value naming the database (e.g. "Morpheus")
#' @param closeConn A boolean indicating whether this function to close the connection to "db". Default is TRUE.
#' @param print A boolean indicating whether to print information on the R console
#'
#'
#' @example /example/ex_dbexplorer.R
#' @describeIn TableInfo Given a database, get all databases on same server
#' @export
#' @import data.table
TableInfo <- function(db=NULL, closeConn=F, print=F) {
    dropCols     <- c("TABLE_SCHEM", "TABLE_TYPE", "REMARKS")
    keepCols     <- c("TABLE_CAT", "TABLE_NAME")
    friendlyName <- c("Database",  "Table")

    if(is.null(db))
        db <- sapply(Databases(),"[[","database")

    fn_fetch <- function(i){
        OpenDB(i)                          # OpenDB will check valid db name
        dt <- getTables(i)

        data.table::set(dt, j = dropCols, value = NULL)
        data.table::setnames(dt, friendlyName)
        data.table::setkeyv(dt, c("Database", "Table"))

        return(PrimaryKey(i,dt$Table))
    }

    res <- rbindlist(lapply(db, fn_fetch))

    if(print){
        print(res[,.(NumberOfTables = .N), by=Database], row.names = F)
        cat("\n\n")
        print(res[1:min(15,nrow(res)),], row.names = F)
    }

    if(closeConn) CloseDB(db)
    return(res)
}

#' @describeIn TableInfo A function to retrieve information about one or more table's primary key column
#' @param tables A character vector of table names belonging to the database specified by "db"
#' @export
#' @import data.table
PrimaryKey <- function(db=NULL, tables=NULL, closeConn=F){
    dropCols     <- c("TABLE_SCHEM", "PK_NAME")
    keepCols     <- c("TABLE_CAT", "TABLE_NAME", "COLUMN_NAME","KEY_SEQ")
    friendlyName <- c("Database", "Table", "PrimaryKey", "PK_Position")

    OpenDB(db)
    if(is.null(tables)) tables <- TableInfo(db)[, unique(Table)]

    # Helper fn to run through each table iteratively
    f <- function(i){
        tmp <- getPrimaryKey(db, i)
        data.table::set(tmp, j = dropCols, value = NULL)    # Drop cols by ref
        data.table::setnames(tmp, keepCols, friendlyName)   # Change to friendly names
        return(tmp)
    }

    dt <- data.table::rbindlist(lapply(tables, f))
    data.table::setkey(dt, Database, Table)                          # key for merge inside other explore fns

    if(closeConn) CloseDB(db)
    return(dt)
}

#' @describeIn TableInfo A function to detailed information about a given table's columns.
#'      If argument "tables" not provided, retrive information about all tables in the specified database
#' @export
#' @import data.table
ColumnInfo <- function(db="Morpheus", tables=NULL, closeConn=F, print=T) {
    dropCols     <- c("TABLE_SCHEM", "DATA_TYPE", "BUFFER_LENGTH",
                      "NUM_PREC_RADIX", "NULLABLE", "REMARKS",
                      "COLUMN_DEF", "SQL_DATA_TYPE","SQL_DATETIME_SUB",
                      "CHAR_OCTET_LENGTH", "SS_DATA_TYPE")
    keepCols     <- c("TABLE_CAT", "TABLE_NAME",  "COLUMN_NAME",
                      "TYPE_NAME", "COLUMN_SIZE", "DECIMAL_DIGITS",
                      "ORDINAL_POSITION", "IS_NULLABLE")
    friendlyName <- c("Database",  "Table", "Column", "Type", "ColumnSize",
                      "DecimalDigits", "Position","IsNullable")

    # If not provided, run for all dbo tables
    if(is.null(tables)) tables <- "%"
    if(is.null(db)) stop("Please provide db name")

    OpenDB(db)
    dt <- getColumns(db, tables)                     # run query for col names

    data.table::set(dt, j = dropCols, value = NULL)
    data.table::setnames(dt, keepCols, friendlyName)
    data.table::setkey(dt, Database, Table)

    # Add Column for IsKey
    data.table::set(dt, j="IsKey", value = "NO")

    pk <- PrimaryKey(db, unique(dt$Table))
    dt[Column %in% pk[, PrimaryKey], IsKey:="YES"]

    if(print){
        cat("\n")
        print(dt[,.(NumberOfCols = .N), by="Database,Table"], row.names = F)
        cat("\n\n")
        n <- min(10,nrow(dt))
        print(dt[1:n, .(Table, Column, Type, Position, IsKey)], row.names = F)
        if(n < nrow(dt))
            cat("--------------------- TRUNCATED ---------------------\n")
    }

    if(closeConn) CloseDB(db)
    return(dt)
}

#' @describeIn TableInfo A function that serves as a quick lookup of columns in a table. Defaults to table
#'      "MainLog" in database "Morpheus".
#' @param return A boolean indicating whether to return objects in memory. Defaults to FALSE which means
#'      information is simply printed on the R console and not captured. Ideal default for quick lookups.
#' @export
#' @import data.table
Columns <- function(db="Morpheus", tables="MainLog", return=F, closeConn=T){
    cat("\nDatabase:", db)
    cat("\nDB Table:", tables)
    cat("\n\n")

    dt <- ColumnInfo(db, tables, closeConn, print = F)

    data.table::set(dt, j=c("Database",
                "Table",
                "ColumnSize",
                "DecimalDigits",
                "IsNullable"), value=NULL)
    if(return)
        return(dt)
    print(dt, row.names = T)
}

#' @describeIn TableInfo An internal wrapper around sqlTables
#' @importFrom RODBC sqlTables
#' @import data.table
getTables <- function(db) {
    cnObj <- GetConn(db)

    dt <- RunCatch(sqlTables(channel   = cnObj,
                                    schema    = "dbo",
                                    tableType = "TABLE"), "Error in getTables")
    dt <- data.table(dt)
    return(dt)
}

#' @describeIn TableInfo An internal wrapper around sqlColumns
#' @importFrom RODBC sqlColumns
#' @import data.table
getColumns <- function(db, tables) {
    cnObj  <- GetConn(db)

    dt <- RunCatch(sqlColumns(channel = cnObj,
                                     sqtable = tables,
                                     schema  = "dbo"), "Error in getColumns")
    dt <- data.table(dt)
    return(dt)
}

#' @describeIn TableInfo An internal wrapper around sqlPrimaryKeys
#' @importFrom RODBC sqlPrimaryKeys
#' @import data.table
getPrimaryKey <- function(db, tables) {
    cnObj  <- GetConn(db)

    dt <- RunCatch(sqlPrimaryKeys(channel = cnObj,
                                         sqtable = tables,
                                         schema  = "dbo"), "Error in getPrimaryKey")
    dt <- data.table(dt)
    return(dt)
}
