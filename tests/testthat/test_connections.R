
# Common developer helper functions #
db <- "Morpheus"
CheckDB(db)     # trys to find a matching database name, returns error if not
AccessInfo(db)  # access info for specified db
ValidDB()       # What DBs are configured in the package?
ConnString(db)  # Connection string for db?

# Opening/Closing Connections #
OpenDB(db)      # Returns 1 on success
CloseDB(db)     # Returns 1 on success
CloseDB(db)     # Warning if closing db that is already closed
CloseDB("blah") # Error if db doesn't exist

# Manage connections #
ConnExists(db)  # Returns TRUE for any open/closed connections
ConnPool()      # Returns all the connection (names) in the pool
ConnStatus()    # Returns a table showing status for all configured dbs

# Use should be rare to not at all #
GetConn(db)     # Retrieve connection object
Clean(db)       # Removes connection from pool (usually because of error)
Clean()         # Removes ALL connections from pool (usually because of error)



SeeConn(db, "Initiator") # Get specified attributes about a connection object
SeeConn(db)              # Get ALL attributes about a connection object

SeeOpenConns()           # See attributes for every open connection
SeeClosedConns()         # See attributes for every closed connection



skip_on_cran()


ColumnInfo()
Columns(db)






PrimaryKey(db)
QuerySauce()

TableInfo(db)
WhereSauce()
xQuery()

# Expect error to say "SQL Server does not exist or access denied"
db <- "Morpheus"
cnstr <- ConnString(db)
emsg <- "CONNECTION FAILED"
smsg <- "CONNECTION ESTABLISHED"
pat <- ".*(?<=])"

sqlsauce::RunTimer(
    RODBC::odbcDriverConnect(cnstr, readOnlyOptimize = T),
    successMsg = smsg,
    Catch = TRUE,
    emsg = "Error happened stupid",
    wmsg = emsg,
    patter = pat
)


# Expect error to say "SQL Server does not exist or access denied"
db <- "blah"
cnstr <- ConnString(db)
emsg <- "CONNECTION FAILED"
pat <- ".*(?<=])"

sqlsauce::RunTimer(
    RODBC::odbcDriverConnect(cnstr, readOnlyOptimize = T),
    successMsg = smsg,
    Catch = TRUE,
    emsg = "Error happened stupid",
    wmsg = emsg,
    patter = pat
)


# Expect error to say "Data source name not found and no default driver specified "
cnstr <- "blah"
emsg <- "Error happened stupid"
pat <- ".*(?<=])"

sqlsauce::RunTimer(
    RODBC::odbcDriverConnect(cnstr, readOnlyOptimize = T),
    successMsg = smsg,
    Catch = TRUE,
    emsg = "Error happened stupid",
    wmsg = emsg,
    patter = pat
)
