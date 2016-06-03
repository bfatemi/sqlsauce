runQuery <- function(db, query, time=TRUE) {
    cnObj  <- GetConn(db)
    nFrame <- sys.nframe()

    # remove sci notation (for numerics in query), reset to global onexit
    globscipen <- options()$scipen
    options(scipen = 1000)
    on.exit(options(scipen = globscipen))

    tryCatch({

        t <- Sys.time()
        dt <- data.table(RODBC::sqlQuery(cnObj, query, errors = T)) # start timer
        if(time) print(paste0("Query completed in: ", Sys.time() - t)) # print timer if time=true

        if(sum(grep("Communication link failure", dt[1])))
            stop("No Access: Check Network")
        if(sum(grep("not find stored procedure", dt[1])))
            stop("Check query string")
        if(sum(grep("Invalid object name", dt[1])))
            stop("Check table name")

        # if no error and just no data existed
        if(!nrow(dt))
            stop("Query successfully exected but no data")

        return(dt)
    }, error = function(e){
        if(IsClosed(db)){
            OpenDB(db)
            runQuery(db, query, time)
        }else{
            DebugInfo(e, nFrame)
            CloseDB(db)
        }
    })
}

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
