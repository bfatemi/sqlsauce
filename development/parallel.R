
library(foreach)
library(iterators)
library(doParallel)
library(RODBC)

cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
    library(RODBC)
    library(sqlsauce)
    dbConn <- RODBC::odbcDriverConnect(connection=sqlsauce::ConnString("Morpheus"))
    NULL
})

nameToID <- function(x) {
    library(sqlsauce)
    where <- sqlsauce::Wsauce(ProcedureID == x)
    query <- sqlsauce::Qsauce("MainLog", wh = where)
    sqlQuery(dbConn, query)
}


# read data we just pulled
procdata <- readRDS("../surgicalsight/data/SFDCProcs2015_DT")
indll <- easydata::SplitIndex(nrow(procdata), 10000)
procll <- lapply(indll, function(ind) procdata[ind, ProcedureID])



# clusterExport(cl, c("pID"))
pID <- procll[1:5]
ll <- clusterApply(cl, pID, nameToID)
ll <- parSapply(cl, pID, nameToID)
