# Install dependencies
library(rmarkdown)
library(DBI)
library(RPostgres)

# Import all functions
source('./dashboard/check_in_response_dashboard/utils/get_query.R')
source('./dashboard/check_in_response_dashboard/utils/render_report.R')

# Retrieve DB credentials from a .txt file
credentials <- readLines("./dashboards//credentials.txt")
user <- sub("user: ", "", credentials[1])
pwd <- sub("pwd: ", "", credentials[2])

db <- 'database_name'
connection <- 'made_up_connection.com'

# Create a connection with the DB
conn <- create_conn(db, connection, user, pwd)

#----------------------------------------------------
#OLD CODE: query from db. doesn't work now since we are using synthetic data
# Declare path where all of the .sql files are stored
QUERY_PATH <- "./dashboard/check_in_response_dashboard/queries"

# Run the query and store them as variables
get_query_output(conn, QUERY_PATH) 

auth_burn <- readRDS("auth_burn_file.rds")

#----------------------------------------------------
#NEW approach:
#read in from excel sheet that has synthetic data
submit_date <- readxl::read_excel('./dashboards/synthetic_dashboard_data.xlsx',
                                     sheet = 'submit_date')

check_in <- readxl::read_excel('./dashboards/synthetic_dashboard_data.xlsx',
                                     sheet = 'check_in')

auth_burn <- readxl::read_excel('./dashboards/synthetic_dashboard_data.xlsx',
                               sheet = 'auth_burn')


# Store all query outputs into dfs 
dfs <- list(general_df = submit_date, check_in_df = check_in, 
            auth_burn = auth_burn)

#----------------------------------------------------

# Render dashboard
render_report(dfs = dfs)