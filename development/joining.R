library(data.table)
library(sqlsauce)

pID <- 4550426
DT <- TraceFollow(pID)

wh <- WhereSauce(ProcedureID == 4550426 & Value > 10 | Duraton < 100)

QuerySauce("MainLog",cols = c("A", "B", "C"), where = wh, verbose = TRUE)

library(stringr)
subQuery = QuerySauce("MainLog", where=wh, verbose=TRUE)
QuerySauce(str_c("(",subQuery, ")"),cols = c("A", "B", "C"), verbose = TRUE)

Vrows <- 4950:5000
View(DT[rows])
