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
#' @description import .sql queries from sql folder and store the output as variables
#' of the file's basename
#' @param conn a connection with a database
#' @param query_folder_path path to the SQL folder that stores all queries
get_query_output <- function(conn, QUERY_FOLDER_PATH) {
  # List all of the .sql files in the directory
  query_file_names <- list.files(path = QUERY_FOLDER_PATH, pattern = "\\.sql$", full.names = TRUE)
  
  # Loop through each SQL file and query the data and assign them as variables after the file name
  for (file in query_file_names) {
    # Extract the base name of the file without .sql extension
    variable_name <- tools::file_path_sans_ext(basename(file))
    
    # Read the SQL file
    query <- read_file(file)
    query <- paste(query, collapse = "\n")
    
    # Run the query
    res <- dbSendQuery(conn, query)
    query_output <- dbFetch(res)
    
    # Assign the output into a variable that's the file's basename
    assign(variable_name, query_output, envir = .GlobalEnv)
    
  }
}
