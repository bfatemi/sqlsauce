# Let's explore what data is available in Morpheus

# Available tables?
TableInfo(db = "Morpheus")
TableInfo(db = "RemoteFE", print = T)

# What are the keys for each table?
PrimaryKey("Morpheus")

# Get key for a specific table
PrimaryKey("Morpheus", "MainLog")

# Let's explore MainLog columns
ColumnInfo(tables = "MainLog")

# Shortcuts/quick view
Columns()
Columns(table = "ProcedureSummaries")


