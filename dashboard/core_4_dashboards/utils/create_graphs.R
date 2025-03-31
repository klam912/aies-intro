# GLOBAL VARIABLES DECLARED HERE
# Font size for bar chart
FONT_SIZE_BAR_CHART = 15

# Color for each response group
RESP_GROUP_COLORS <- data.frame(
  group = c("Responded", "Partial", "All responded as zero", "All responded as NA"),
  color = c("#26C6DA", "#2E78D2", "#006C7A", "#FFBEA9")
)

# Color for MU and SU
MU_AND_SU_COLORS <- data.frame(
  ent_typ_txt = c("MU", "SU"),
  color = c("#FFBEA9", "#97BCE9")
)

# --------------------------------------------------------
#' @title create_NAICS_breakdown_graph
#' @name create_NAICS_breakdown_graph
#' @description create a Plotly stacked bar chart that breaks the data down by
#' NAICS sector groups
#' @param plot_data a dataframe that contains NAICS sector groups and percentage
#' of login ids within each sector group
#' @return a stacked bar chart graph
create_NAICS_breakdown_graph <- function(plot_data) {
  # Merge df with the colors dataframe to get each resp group an associated color
  plot_data_with_colors <- merge(plot_data, RESP_GROUP_COLORS, by = "group")
  
  # Plot the graph
  fig <- plot_ly(
    plot_data_with_colors,
    x = ~sector_naics_group,
    y = ~perc,
    color = ~group,
    colors = ~color,
    type = "bar",
    hovertext = ~paste(
      "Sector", sector_naics_group, "<br>",
      group, ":", perc, "%",
      case_when(
        group == "All responded as NA" ~ "establishment responses all NA",
        group == "All responded as zero" ~ "establishment responses all 0",
        group == "Responded" ~ "complete establishment responses",
        group == "Partial" ~ "partial establishment responses"
      ),
      "<br>",
      "Count:", prettyNum(login_id_count, big.mark = ","), "login_ids"),
    hoverinfo = "text"
  ) %>% layout(
    barmode = "stack",
    xaxis = list(title = "NAICS sector", tickangle = -45, font = list(size = FONT_SIZE_BAR_CHART)),
    yaxis = list(title = "% of login_id responses", font = list(size = FONT_SIZE_BAR_CHART)),
    title = list(text = "Breakdown of response rate by NAICS sector",
                 font = list(size = FONT_SIZE_BAR_CHART)
    ), font = list(size = FONT_SIZE_BAR_CHART)
  )
  return(fig)
}


#' @title create_MU_SU_comparison_graph_KAU
#' @name create_MU_SU_comparison_graph_KAU
#' @description creates a side by side bar chart to compare distribution of 
#' MU and SU in each response group
#' @param plot_data a dataframe that contains information about a login id's response 
#' group, percentage of login ids per response group, a login id's enterprise 
#' type
#' @return a side by side bar chart graph
create_MU_SU_comparison_graph_KAU <- function(plot_data) {
  # Merge with MU_AND_SU_COLORS to get colors
  plot_data_with_colors <- merge(plot_data, MU_AND_SU_COLORS, by = "ent_typ_txt")
  
  fig <- plot_ly(
    plot_data_with_colors,
    type = "bar",
    x = ~group,
    y = ~perc,
    color = ~ent_typ_txt,
    colors = ~color,
    hovertext = ~paste(group, ":", perc, "%",
                       "<br>",
                       "Count:", prettyNum(login_id_count, big.mark = ","), "login_ids"),
    hoverinfo = "text"
  ) %>% layout(
    barmode = "group",
    xaxis = list(title = "Response group", tickangle = -45, font = list(size = FONT_SIZE_BAR_CHART)),
    yaxis = list(title = "% of login_id responses", font = list(size = FONT_SIZE_BAR_CHART)),
    title = list(text = "Breakdown of response rate by enterprise type",
                 font = list(size = FONT_SIZE_BAR_CHART)
    ), font = list(size = FONT_SIZE_BAR_CHART)
  )
  return(fig)
}
