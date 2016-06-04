getTables <- function(db) {
    cnObj <- GetConn(db)

    tryCatch({
        dt <- data.table(RODBC::sqlTables(channel   = cnObj,
                                          schema    = "dbo",
                                          tableType = "TABLE"))
    }, error = function(e){
        if(IsClosed(db)){
            OpenDB(db)
            getTables(db, table)
        }else{
            DebugInfo("Error in getTables", nFrame)
            CloseDB(db)
        }
    })
    return(dt)
}


getColumns <- function(db, table) {
    cnObj  <- GetConn(db)
    nFrame <- sys.nframe()

    tryCatch({
        dt <- data.table(RODBC::sqlColumns(channel = cnObj,
                                           sqtable = table,
                                           schema  = "dbo"))
    }, error = function(e){
        if(IsClosed(db)){
            OpenDB(db)
            getColumns(db, table)
        }else{
            DebugInfo("Check query or connection", nFrame)
            CloseDB(db)
        }
    })
    return(dt)
}

getPrimaryKey <- function(db, table) {
    cnObj  <- GetConn(db)
    nFrame <- sys.nframe()

    tryCatch({
        dt <- data.table(RODBC::sqlPrimaryKeys(channel = cnObj,
                                               sqtable = table,
                                               schema  = "dbo"))
    }, error = function(e){
        if(IsClosed(db)){
            OpenDB(db)
            getColumns(db, table)
        }else{
            DebugInfo("Check query or connection", nFrame)
            CloseDB(db)
        }
    })
    return(dt)
}
