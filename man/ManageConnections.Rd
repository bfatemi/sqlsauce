% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conn-manage.R
\name{ManageConnections}
\alias{Clean}
\alias{CloseDB}
\alias{ConnExists}
\alias{ConnPool}
\alias{GetConn}
\alias{GetConnRef}
\alias{ManageConnections}
\alias{OpenDB}
\title{Managing Connections}
\usage{
OpenDB(db = NULL)

CloseDB(db = NULL)

ConnPool()

ConnExists(db = NULL)

Clean(db = NULL)

GetConn(db)

GetConnRef(db)
}
\arguments{
\item{db}{A character value representing the name of a database}
}
\description{
Functions to open/close and otherwise manage the connections to a configured
database, stored in a connection pool. These functions are intended for
developers that need access to the connection environment as they develop data
products.
}
\section{Functions}{
\itemize{
\item \code{OpenDB}: Open connection to a given database. Returns 1 on success

\item \code{CloseDB}: Close a connection to given db. Returns 1 on success

\item \code{ConnPool}: See all active (open or closed) connections in the pool

\item \code{ConnExists}: A function to check whether a connection has been
initiated. Returns TRUE if connection object exists, and FALSE otherwise. Error
if argument is not the name of a configured db

\item \code{Clean}: A function to clean the connection pool

\item \code{GetConn}: A function to get a connection object from the connection pool

\item \code{GetConnRef}: A function to get a connection object from the connection pool
}}
\section{Note}{

Any database where access to managing connections is required must be configured prior.
Future functionality will include the ability to auto-configurate a database.
}

