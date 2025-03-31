#---------------------------------------------------
#' @title create_conn
#' @name create_conn
#' @description creates a connection with the database
#' @param db_name database name
#' @param host_name name of the host
#' @param user user name
#' @param pwd password
#' @return a connection with the database
create_conn <- function(db_name, host_name, user, pwd) {
  conn <- dbConnect(RPostgres::Postgres(),
                          host = host_name,
                          port = '5432',
                          dbname = db_name,
                          user = user,
                          password = pwd)
  return(conn)
}

#' @title get_query_output
#' @name get_query_output
#' @description query data from database
#' @param conn a connection with database
#' @param query SQL query
get_query_output <- function(conn, query) {
  # Run the query
  res <- dbSendQuery(conn, query)
  query_output <- dbFetch(res)

  return(query_output)
  
}