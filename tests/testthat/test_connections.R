# Declare testing functions -----------------------------------------------


# skip when doing continuous and remote building. Also, look into encryption for Travis
check_access <- function() {
    skip_on_travis()
}


# Function to open/close/clean connection to db, as well as checking attributes
OpenCloseClean <- function(db){

    # Open connection
    opentest <- function(db){
        expect_identical(OpenDB(db), 1)
        expect_identical(SeeConn(db)[Names=="Status", Values], "Open")
        expect_identical(ConnStatus()[Database == db, Status], "Open")
        expect_identical(ConnStatus(db), "Open")
        expect_identical(class(SeeConn(db, "TimeInitiated")), "character")
        expect_identical(class(SeeConn(db, "TimeOpened")), "character")
    }
    opentest(db)


    # Close connection
    closetest <- function(db){
        expect_identical(CloseDB(db), 1)
        expect_identical(SeeConn(db)[Names=="Status", Values], "Closed")
        expect_identical(ConnStatus()[Database == db, Status], "Closed")
        expect_identical(ConnStatus(db), "Closed")
        expect_identical(class(SeeConn(db, "TimeInitiated")), "character")
        expect_identical(class(SeeConn(db, "TimeOpened")), "character")
        expect_identical(class(SeeConn(db, "TimeClosed")), "character")
    }
    closetest(db)

    # Clean connection env
    expect_equal(Clean(), 1)
}

# Function to perform extensive checking of connection managment and attributes
Chk_ConnManagement <- function(db){
    time <- Sys.time()
    expect_equal(OpenDB(db), 1)
    expect_equal(ConnPool(), db)                        # Returns all the connection (names) in the pool
    expect_true(ConnExists(db))
    expect_error(ConnExists("blah"), "Not valid DB!")
    expect_type(ListConnAttr(), "character")

    expect_identical(SeeConn(db, "Database"),   db)
    expect_identical(SeeConn(db, "Status"),     "Open")
    expect_identical(SeeConn(db, "Status"),     ConnStatus(db))
    expect_identical(SeeConn(db, "Initiator"),  "OpenDB")
    expect_identical(SeeConn(db, "Opener"),     "OpenDB")
    expect_identical(SeeConn(db, "Closer"),     NA)
    expect_identical(SeeConn(db, "Requestor"),  NA)

    # expecting time opened to be close to time declared above
    expect_lt(abs(as.numeric(difftime(SeeConn(db, "TimeOpened"), time, units = "sec"))), 1.5)

    # expecting time since initiated to be diff until now
    tdiff   <- abs(as.numeric(difftime(SeeConn(db, "TimeInitiated"),
                                       Sys.time(), units="sec")))
    attrdur <- SeeConn(db, "DurSinceInit")
    tdiff2 <- SeeConn(db)[Names=="DurSinceInit", Values]
    expect_lt(abs(abs(tdiff) - abs(as.numeric(tdiff2))), 1.5)
    expect_lt(abs(abs(tdiff) - as.numeric(attrdur)), 1.5)

    # expecting time requested to be close to be diff until new request now
    reqtime <- Sys.time()
    expect_equal(OpenDB(db), 1)
    expect_identical(SeeConn(db, "Status"), "Open")
    expect_lt(abs(as.numeric(difftime(SeeConn(db, "TimeRequested"), reqtime, units = "sec"))),1.5)
    expect_identical(SeeConn(db, "AccessCount"), 2)

    # Close and Reopen to check attributes that should have updated
    Sys.sleep(3) # sleep for 5 seconds so time closed is an observable amount
    # of seconds later than time initiated

    tclose <- Sys.time()
    expect_identical(CloseDB(db), 1)

    # duration between TimeRequested and TimeClosed should be about 5 seconds
    expect_lt(abs(as.numeric(difftime(SeeConn(db, "TimeClosed"),
                                      SeeConn(db, "TimeRequested"),
                                      units = "sec"))), 4)


    # expect 0 and warning from trying to close a closed connection
    expect_warning(expect_identical(CloseDB(db), 0))

    # check 'close' related attributes that are now updated
    expect_identical(SeeConn(db, "Status"),     "Closed")
    expect_identical(SeeConn(db, "Status"),     ConnStatus(db))

    expect_identical(SeeConn(db, "Closer"),     "CloseDB")
    expect_identical(SeeConn(db, "Requestor"),  "OpenDB")
    expect_identical(SeeConn(db, "DurationOpen"), 0)

    expect_lt(abs(as.numeric(difftime(SeeConn(db, "TimeClosed"), tclose, units="sec"))),1.5)


    tdiff   <- abs(as.numeric(difftime(SeeConn(db, "TimeRequested"),
                                       Sys.time(), units="sec")))
    attrdur <- SeeConn(db, "DurSinceRequest")
    tdiff2 <- SeeConn(db)[Names=="DurSinceRequest", Values]
    expect_lt(abs(abs(tdiff) - abs(as.numeric(tdiff2))), 1.5)
    expect_lt(abs(abs(tdiff) - as.numeric(attrdur)), 1.5)

    tdiff   <- abs(as.numeric(difftime(SeeConn(db, "TimeClosed"),
                                       Sys.time(), units="sec")))
    attrdur <- SeeConn(db, "DurationClosed")
    tdiff2 <- SeeConn(db)[Names=="DurationClosed", Values]
    expect_lt(abs(abs(tdiff) - abs(as.numeric(tdiff2))), 1.5)
    expect_lt(abs(abs(tdiff) - as.numeric(attrdur)), 1.5)


    # Ensure attributes for old conn object are same as new one
    oldcnObj <- GetConn(db)
    expect_identical(class(oldcnObj), "RODBC")

    # get old attr
    oldvals <- sapply(ListConnAttr(), attr, x=oldcnObj)
    oldnames <- names(oldvals)

    # new obj
    opentest(db)
    newcnObj <- GetConn(db)
    newvals <- sapply(ListConnAttr(), attr, x=newcnObj)
    newnames <- names(newvals)
    closetest(db)
    Clean()
}

# UNIT TESTS BELOW --------------------------------------------------------

# check various and simple lookup functions
test_that("Lookup functions", {
    check_access()

    db <- "Morpheus"
    expect_equal(CheckDB(db),   "Morpheus")  # trys to find a matching database name, returns error if not
    expect_type(AccessInfo(db), "list")      # access info for specified db
    expect_type(ValidDB(),      "character") # What DBs are configured in the package?
    expect_type(ConnString(db), "character") # Connection string for db?
})


# Test open/close/clean/attributes for some DBs
test_that("RemoteFE: open/close/clean", {
    check_access()

    OpenCloseClean("RemoteFE")
    OpenCloseClean("HNI")
    OpenCloseClean("SAP")

    Chk_ConnManagement("RemoteFE")
    Chk_ConnManagement("HNI")
    Chk_ConnManagement("SAP")
})





