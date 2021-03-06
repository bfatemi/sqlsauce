
### Terminology

### Example workflow

### Operators for the where sauce


#----------------------------------------------------------------------------
# Work-flow to serve the data dish:
#
#               Where sauce --> Query sauce --> Data dish!
#
# STEP 1: Name the configured database
# STEP 2: Name the table to query
# STEP 3: Get the WHERE sauce (optional ingredient for the query sauce)
# STEP 4: Get the Query sauce (main ingredient for the data dish)
#----------------------------------------------------------------------------

db <- "RemoteFE"
tbl <- "SFDC.dbo.PROCEDURES"

#----------------------------------------------------------------------------
# Get procedures of specified subjects that occurred since 2015
#----------------------------------------------------------------------------
psub <- c("HPB", "dVP", "dVC", "dVL") # for WHERE sauce

wh    <- Wsauce(ProcedureSubject == psub & Year(CreateDate) > 2015)
query <- Qsauce(tbl, top = 1000, where = wh)

DT    <- xQuery(db, query)

#----------------------------------------------------------------------------
# Alternate workflow:
#   Open/Closing is handled by xQuery unless db is open. In which case,
#   the user can control when to open and close connections
#----------------------------------------------------------------------------

OpenDB(db)  # Open connection

# .... Do work

CloseDB(db) # Close connection

