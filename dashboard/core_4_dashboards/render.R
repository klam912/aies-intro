# Install dependencies
library(rmarkdown)
library(DBI)
library(RPostgres)
library(readxl)

# Import all functions
source('./dashboards/core_4_dashboards/utils/get_query.R')
source('./dashboards/core_4_dashboards/utils/render_report.R')

# Retrieve DB credentials from a .txt file
credentials <- readLines("credentials.txt")
user <- sub("user: ", "", credentials[1])
pwd <- sub("pwd: ", "", credentials[2])

db <- 'database_name'
connection <- 'made_up_connection.com'

# Create a connection with the DB
conn <- create_conn(db, connection, user, pwd)

#----------------------------------------------------
#Old code: querying the database

query_one <- '
select stuff
from table
where filters'


#----------------------------------------------------
# Query all data
submit_date_df <- get_query_output(conn, submit_date_query)
estab_response_df <- get_query_output(conn, estab_response_query)
kau_response_df <- get_query_output(conn, KAU_response_query)
#----------------------------------------------------


#New code:
#read in a particular sheet to an excel file since we arent connecting to DBs
estab_response_df <- readxl::read_excel('./dashboards/synthetic_dashboard_data.xlsx',
                   sheet = 'estab_response')

kau_response_df <- readxl::read_excel('./dashboards/synthetic_dashboard_data.xlsx',
                                        sheet = 'kau_response')

submit_date_df <- readxl::read_excel('./dashboards/synthetic_dashboard_data.xlsx',
                                        sheet = 'submit_date')

# Variable names of the core 4 variables
CORE_4_VARS <- c(
  "measurement1",
  "measurement2",
  "measurement3",
  "measurement4"
)

# Formal names of the core 4 variables
CORE_4_VARS_NAMES <- c(
  "revenue",
  "annual payroll",
  "employment",
  "Q1 payroll"
)

# Store the dataframes into a list
dfs <- list(general_df = submit_date_df, estab_general = estab_response_df, KAU_general = kau_response_df)

# Create a params list that stores a variable with its corresponding 
# variable name and dataframes
params_list <- lapply(seq_along(CORE_4_VARS), function(i) {
  list(variable = CORE_4_VARS[i], variable_name = CORE_4_VARS_NAMES[i], dfs = dfs,
       title = paste0("Visualizing AIES response rates: ", CORE_4_VARS_NAMES[i]))
})

# Render all of the response summary dashboards
render_reports(params_list)
