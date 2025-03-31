# Declare paths to both versions of the dashboard 
REVENUE_DASHBOARD_PATH <- "./dashboards/core_4_dashboards/dashboards/revenue_dashboard.Rmd"
NON_REVENUE_DASHBOARD_PATH <- "./dashboards/core_4_dashboards/dashboards/non_revenue_dashboard.Rmd"

#----------------------------------------------

#' @title render_reports 
#' @name render_reports
#' @description render reports for each of the core 4 variables and store them
#' in a folder that's named after the render date
#' @param params_list a list of parameters like dataframes and variable names
#' @return HTML reports all stored in a folder
render_reports <- function(params_list) {
  lapply(params_list, function(params) {
    # Create a folder for each render date
    output_dir <- paste0("./output_dir", Sys.Date(), "/")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Render each file
    output_file <- paste0(output_dir, "AIES High Level Dashboard -", params$variable_name, Sys.Date(), ".html")
    rmarkdown::render(
      if(params$variable_name == "revenue") {
        input = REVENUE_DASHBOARD_PATH
      } else {
        input = NON_REVENUE_DASHBOARD_PATH
      },
      output_file = output_file,
      params = params,
      envir = new.env()
    )
  })
}
