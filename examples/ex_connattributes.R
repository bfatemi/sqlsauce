#------------------------------------------------------------------------------
## Each attribute can be retrieved with the function SeeConn(db, what) where
## the database of interest and an attribute name is provided as the
## argument:

# SeeConn("Morpheus", "DurationOpen")

#------------------------------------------------------------------------------
## To see all attributes about a connection, don't supply the argument "what":

# SeeConn("Morpheus") # returns error if connection doesnt exist

#------------------------------------------------------------------------------
## As a general helper, don't supply any arguments to see a list of all the
## attributes that exist for any given connection object:

# SeeConn()

#------------------------------------------------------------------------------
## To see this information for all open connections, or all closed connections,
## use the following functions

# SeeOpenConns()
# SeeClosedConns()

#------------------------------------------------------------------------------
## To print the status of all connections, run:
# ConnStatus

#------------------------------------------------------------------------------
## To remind yourself of all possible connection object attributes, use the
## following lookup function:

# ListConnAttr()
