# sqlsauce

## Source File Prefix

The prefixes (and potentially an additional subsequent identifier) for the filenames describe their purpose and reason for grouping:

1. __Query Functions (e.g. query-*.R)__

   All functions that have a prefix "query" are functions either (1) import a function from the RODBC package; or (2) are designed to be an intuitive interface between R and SQL

2. __Query Help Functions (e.g. query-help_*.R)__

   All functions with the prefix "query" plus the indentifier "help" are functions that rely on external RODBC functions that retrieve data exploration resources (i.e. data dictionaries, etc.).  

3. __Connection Functions (e.g. conn-*.R)__

   All functions that have prefix "conn" are low-level functions that call functions from the package RODBC to open/close/get/set/access connection objects as well as interact with the connection pool.
   
4. __Utility Functions (e.g. utils-*.R)__  

   Functions within files that begin with "utils" are mostly internal utilities. Some of these functions will be extracted and bundled in their own package

[![Travis-CI Build Status](https://travis-ci.org/bfatemi/sqlsauce.svg?branch=master)](https://travis-ci.org/bfatemi/sqlsauce)
