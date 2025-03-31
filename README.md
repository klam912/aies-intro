# aies-intro

Welcome to the GitLab repository for the data visualization of AIES survey using RMarkdown dashboards. This repository includes the code for:

-   Two main dashboards

-   Render scripts

## Where is the data queried?

We queried data mainly from two database to get additional variables for further breakdown and analysis. All of the data is queried directly from the cloud using the `DBI` and `RPostgres` packages in R to establish a connection with the database.

## Repo overview
```
.
├── check_in_response_dashboard
│   ├── checkin_response_dashboard.Rmd
│   ├── queries
│   ├── render.R
│   └── utils
├── core_4_dashboards
│   ├── dashboards
│   ├── queries
│   ├── render.R
│   └── utils
└── credentials.txt
```
-   check_in_response_dashboard/: Contains the RMarkdown dashboard, a `utils` folder that contains R files with helper functions to create the dashboard, an R script that automates the dashboard rendering process, and a `queries` folder with SQL queries.

    -   `check_in_response_dashboard.Rmd`: The template for creating a dashboard that looks at companies' check in rates.

    -   utils/: Contains modules with helper functions.

    -   `render.R`: Renders the `check_in_response_dashboard.Rmd` file into a static HTML file.

<!-- -->

-   core_4_dashboards/: Contains a RMarkdown dashboard template, a `utils` folder that contains R files with helper functions to create the dashboard, an R script that automates the creation of 4 reports for each of the core 4 variables using the template, and a `queries` folder with SQL queries.
    -   dashboards/: Contains 2 dashboard versions for revenue and the 3 other variables.

        -   `revenue_dashboard.Rmd`: RMarkdown dashboard displaying high-level summaries for revenue at both  establishment and KAU levels with multiple breakdowns.

        -   `non_revenue_dashboard.Rmd`: Similar to the `revenue_dashboard.Rmd` but only summarize data at an establishment level with similar breakdowns.

    -   utils/: Contains modules with helper functions.

    -   `render.R`: Renders 4 reports using the the RMarkdown templates in `dashboards/`.


## How to render the dashboards?

### Check in response dashboard

1.  Run `render.R` .
2.  The script will query data from the database to get the data necessary for the dashboard.
3.  The script will knit `check_in_response_dashboard.Rmd` with parameters, which contains the query outputs.
4.  Once render is done, you can view it as an HTML file.

### Response summary dashboards

1.  Run `render.R`
2.  The script will query data from the databases to get the necessary data for the dashboards.
3.  The script will then knit `revenue_dashboard.Rmd`.
4.  It will also knit `non_revenue_dashboard.Rmd`.
5.  Once rendered, you can view all four of the dashboards in a folder that is named after the render date as HTML files.

## Where to store the dashboards?

Once you have rendered the dashboards as HTML files, you can store them in a secure folder, where certain people can have access to it. To view the dashboards, simply click on the HTML link and you should be able to view it on your browser.

## Contact information

For more information please contact:

-   John Lombardi

-   Ken Lam

Any opinions and conclusions expressed herein are those of the author(s) and do not reflect the views of the U.S. Census Bureau. The Census Bureau has reviewed this data product to ensure appropriate access, use, and disclosure avoidance protection of the confidential source data (Project No. P-7529180, Disclosure Review Board (DRB) approval number:  CBDRB-FY24-EWD001-008).
