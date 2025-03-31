CHECK_IN_DASHBOARD_PATH <- "./dashboards/check_in_response_dashboard/checkin_response_dashboard.Rmd"

#-------------------------------------------------

render_report <- function(check_in_dashboard_path = CHECK_IN_DASHBOARD_PATH, dfs) {
  # Check if directory exists
  output_dir <- paste0("./output_file", Sys.Date(), "/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Render the dashboard
  output_file <- paste0(output_dir, "AIES High Level Dashboard (Checked in rates)", Sys.Date(), ".html")
  rmarkdown::render(
    input = check_in_dashboard_path,
    output_file = output_file,
    params = list(dfs = dfs),
    envir = new.env()
  )
}