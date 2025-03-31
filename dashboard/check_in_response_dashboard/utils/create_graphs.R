# GLOBAL VARIABLE DECLARED HERE 
# Aesthetic options for check in groups
CHECK_IN_COLORS <- data.frame(
  check_in_group = c("Checked in", "Not checked in"),
  color = c("#205493", "#FF7043")
  )

# Color for MU and SU
MU_AND_SU_COLORS <- data.frame(
  ent_typ_txt = c("MU", "SU"),
  color = c("#FFBEA9", "#97BCE9")
)

FONT_OPTIONS <- list(
  font_size_treemap = 24,
  font_size_barchart = 15
)

#-------------------------------------------
#' @title create_time_series_graph
#' @name create_time_series_graph
#' @description plot a time series graph
#' @param timeseries_data time series dataframe
#' @return a time series graph
create_time_series_graph <- function(timeseries_data) {
  check_in_color <- CHECK_IN_COLORS$color[CHECK_IN_COLORS$check_in_group == "Checked in"]
  not_check_in_color <- CHECK_IN_COLORS$color[CHECK_IN_COLORS$check_in_group == "Not checked in"]
  fig <- plot_ly(
    timeseries_data,
    mode = 'lines',
    type = 'scatter'
  ) %>%
    add_lines(x = ~week, y = ~check_in_count, name = 'Checked in',
              hovertext = ~paste(week, ":", prettyNum(check_in_count, big.mark = ","), "login_ids"), line = list(color = check_in_color),
              hoverinfo = "text") %>%
    add_lines(x = ~week, y = ~not_checkin_count, name = 'Not checked in',
              hovertext = ~paste(week, ":", prettyNum(not_checkin_count, big.mark = ","), "login_ids"), line = list(color = not_check_in_color),
              hoverinfo = "text") %>%
    layout(
      title = list(text = 'Checked in counts over time', font = list(size = 18)),
      xaxis = list(title = list(text = 'Week')),
      yaxis = list(title = 'login_id counts'),
      legend = list(x = 1, y = 1, font = list(size = 24))
    )
  return(fig)
}

MU_SU_treemap_graph <- function(plot_data) {
  data_merged_with_colors <- merge(plot_data, MU_AND_SU_COLORS, by = "ent_typ_txt")
  
  fig <- plot_ly(
    data_merged_with_colors,
    type = "treemap",
    labels = ~ent_typ_txt,
    values = ~perc,
    parents = c("", ""),
    hovertext = ~paste(perc, "%", "<br>", prettyNum(count, big.mark = ","), "login_ids"),
    hoverinfo = "text",
    textfont = list(size = 24),
    marker = list(colors = ~color)
  ) %>% layout(title = list(
    text = "Distribution of login_id per enterprise type (MU vs. SU)", 
    font = list(size = 15)),
    font = list(size = 24)
  )
  return(fig)
}

#' @title ent_typ_check_in_breakdown_graph
#' @name ent_typ_check_in_breakdown_graph
#' @description breakdowns the check in group for an ent_typ
#' @param plot_data dataframe that has 'ent_typ' col
#' @param ent_typ "MU" or "SU"
#' @return a treemap breakdown of check in group for an ent_typ
ent_typ_check_in_breakdown_graph <- function(plot_data, ent_typ) {
  # Merge to get colors for each ent_typ
  merged_with_colors <- merge(plot_data, CHECK_IN_COLORS, by = "check_in_group")
  
  fig <- plot_ly(
    merged_with_colors,
    type = "treemap",
    labels = ~check_in_group,
    values = ~perc,
    parents = c("", ""),
    hoverinfo = "text",
    hovertext = ~paste(check_in_group, ":", perc, "%", "<br>", "Count:", prettyNum(count, big.mark = ",")),
    marker = list(colors = ~color)
  ) %>% layout(title = list(text = paste("Distribution of checkin group (", ent_typ, ")"), font = list(size = 15)),
               font = list(size = 24))
  
  return(fig)
}

#' @title ent_size_breakdown_graph
#' @name ent_size_breakdown_graph
#' @description create a graph that breaks the data down by ent_typ and compare
#' check in rates
#' @param plot_data data ready to be plot
#' @return a stacked bar graph
ent_size_breakdown_graph <- function(plot_data) {
  fig <- plot_ly(
    plot_data,
    type = "bar",
    x = ~ent_size_code_txt.x,
    y = ~checked_in_group,
    name = "Checked in",
    hovertext = ~paste(case_when(
      ent_size_code_txt.x == "L" ~ "L (5k+ employees)",
      ent_size_code_txt.x == "M" ~ "M (1k-4.9k employees)",
      ent_size_code_txt.x == "N" ~ "N (10+ estabs OR 50+ employees and is not an L or M)",
      ent_size_code_txt.x == "S" ~ "S (All other ENTs that are not identified as a L, M, or N)"
      
    ),
    ":", checked_in_group, "%",
    "<br>", "Count: ", prettyNum(all_checked_in_count, big.mark = ","), "login_ids"
    ),
    hoverinfo = "text",
    marker = list(color = CHECK_IN_COLORS$color[CHECK_IN_COLORS$check_in_group == "Checked in"])
  ) %>%
    add_trace(
      y = ~not_checked_in_group,
      name = "Not checked in",
      hovertext = ~paste(case_when(
        ent_size_code_txt.x == "L" ~ "L (5k+ employees)",
        ent_size_code_txt.x == "M" ~ "M (1k-4.9k employees)",
        ent_size_code_txt.x == "N" ~ "N (10+ estabs OR 50+ employees and is not an L or M)",
        ent_size_code_txt.x == "S" ~ "S (All other ENTs that are not identified as a L, M, or N)"),
        ":", not_checked_in_group, "%",
        "<br>", "Count: ", prettyNum(all_delinquent_count, big.mark = ","), "login_ids"),
      hoverinfo = "text",
      marker = list(color = CHECK_IN_COLORS$color[CHECK_IN_COLORS$check_in_group == "Not checked in"])
    ) %>%
    layout(
      barmode = "stack",
      xaxis = list(title = 'Enterprise Size', font = list(size = FONT_OPTIONS$font_size_barchart)),
      yaxis = list(title = "% of all enterprises", font = list(size = FONT_OPTIONS$font_size_barchart)),
      legend = list(title = list(text = 'Category', font = list(size = FONT_OPTIONS$font_size_barchart))),
      title = list(text = "Breakdown of MU by enterprise size", font = list(size = 15)),
      font = list(size = FONT_OPTIONS$font_size_barchart)
    )
  return(fig)
}

#' @title comparison_graph
#' @name comparison_graph
#' @description compare check in rates
#' @param data_1 data from survey 1
#' @param data_2 data from survey 2
#' @return side by side bar graph comparing the check in rates between surveys
comparison_graph <- function(data_1, data_2) {
  colors <- c("survey_1" = "#4B636E", "survey_2" = "#A7C0CD")
  
  fig <- plot_ly(
    data_1,
    type = "bar",
    x = ~check_in_group,
    y = ~perc,
    name = "survey_1",
    marker = list(color = colors["survey_1"]),
    hoverinfo = "text",
    hovertext = ~paste(
      data_1$perc, "%"
    )
  ) %>% add_trace(
    y = ~data_2$perc,
    name = "survey_2",
    marker = list(color = colors["survey_2"]),
    hoverinfo = "text",
    hovertext = ~paste(
      data_2$perc, "%"
    )
  ) %>%
    layout(
      xaxis = list(title = "Check in status", font = list(size = FONT_OPTIONS$font_size_barchart)),
      yaxis = list(title = "% of MU", font = list(size = FONT_OPTIONS$font_size_barchart)),
      barmode = "group",
      legend = list(title = list(text = 'Category', font = list(size = FONT_OPTIONS$font_size_barchart))),
      title = "Multi Unit Comparison"
    )
  return(fig)
}

#' @title auth_status_breakdown_graph
#' @name auth_status_breakdown_graph
#' @description creates a graph breakdown of login ids by their auth_status
#' and check in group
#' @param plot_data data with 'auth_status' col
#' @return a stacked bar chart
auth_status_breakdown_graph <- function(plot_data) {
  colors = c("issued" = "#2E78D2", "used" = "#006C7A", "disabled" = "#FFBEA9")
  
  fig <- plot_ly(
    plot_data,
    x = ~check_in_group,
    y = ~perc,
    type = "bar",
    color = ~auth_status,
    colors = colors,
    hovertext = ~paste(auth_status, ":", perc, "%",
                       "<br>", "Count:", prettyNum(N, big.mark = ","), "login_ids"),
    hoverinfo = 'text'
  ) %>% layout(
    barmode = "stack",
    xaxis = list(title = "Check in status"),
    yaxis = list(title = "% of login_ids"),
    title = list(text = "Breakdown of login_ids by check in status and auth_status")
  )
  return(fig)
}
