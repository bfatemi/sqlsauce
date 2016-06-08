# ColumnInfo()
# Columns(db)
#
#
#
#
#
#
# PrimaryKey(db)
# QuerySauce()
#
# TableInfo(db)
# WhereSauce()
# xQuery()
#
# # Expect error to say "SQL Server does not exist or access denied"
# db <- "Morpheus"
# cnstr <- ConnString(db)
# emsg <- "CONNECTION FAILED"
# smsg <- "CONNECTION ESTABLISHED"
# pat <- ".*(?<=])"
#
# sqlsauce::RunTimer(
#     RODBC::odbcDriverConnect(cnstr, readOnlyOptimize = T),
#     successMsg = smsg,
#     Catch = TRUE,
#     emsg = "Error happened stupid",
#     wmsg = emsg,
#     patter = pat
# )
#
#
# # Expect error to say "SQL Server does not exist or access denied"
# db <- "blah"
# cnstr <- ConnString(db)
# emsg <- "CONNECTION FAILED"
# pat <- ".*(?<=])"
#
# sqlsauce::RunTimer(
#     RODBC::odbcDriverConnect(cnstr, readOnlyOptimize = T),
#     successMsg = smsg,
#     Catch = TRUE,
#     emsg = "Error happened stupid",
#     wmsg = emsg,
#     patter = pat
# )
#
#
# # Expect error to say "Data source name not found and no default driver specified "
# cnstr <- "blah"
# emsg <- "Error happened stupid"
# pat <- ".*(?<=])"
#
# sqlsauce::RunTimer(
#     RODBC::odbcDriverConnect(cnstr, readOnlyOptimize = T),
#     successMsg = smsg,
#     Catch = TRUE,
#     emsg = "Error happened stupid",
#     wmsg = emsg,
#     patter = pat
# )
