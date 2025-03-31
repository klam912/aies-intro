# Function: create time series data
#' @title create_time_series_data
#' @name create_time_series_data
#' @description create a time series data to plot
#' @param query_output queried data
#' @return a time series data
create_time_series_data <- function(query_output) {
  time_series <- query_output
  
  # Format submit_date col into Date format
  time_series$submit_date <- as.Date(time_series$submit_date, format = '%Y-%m-%d')
  
  # Construct a window (separate weekly)
  weekly_data <- data.frame(date = seq(min(time_series$submit_date, na.rm = TRUE),
                                       max(time_series$submit_date, na.rm = TRUE),
                                       by = 'week'))
  
  #For each week cutoff assign counts that have a submit date <= that week_cutoff
  #rinse and repeat for each week cutoff
  #count the rest as the 'not checked in' for a login id
  summary_list <- list()
  i <- 1
  
  for (week_cutoff in weekly_data$date) {
    
    # All the login_ids that are below the week cutoff
    submitted <- time_series %>% filter(submit_date < week_cutoff)
    
    # Find the login_ids that are not in the submitted (count up)
    not_submitted <- time_series %>% filter(!(login_id %in% submitted$login_id))
    
    # Keep changing the group based on the moving window
    time_series <- time_series %>% mutate(
      check_in_group = case_when(
        is.na(submit_date) ~ "Not checked in",
        login_id %in% not_submitted$login_id ~ "Not checked in",
        TRUE ~ "Checked in"
      )
    )
    
    checkin_count <- return_check_in_count(time_series, "Checked in")
    not_checkin_count <- return_check_in_count(time_series, "Not checked in")
    
    check_in_data <- data.frame('week' = weekly_data$date[i],
                                'check_in_count' = checkin_count,
                                'not_checkin_count' = not_checkin_count)
    
    summary_list[[i]] <- check_in_data
    
    i <- i + 1
  }
  
  # Once have all of the weeks, combine them into a dataframe
  time_series <- dplyr::bind_rows(summary_list)
  return(time_series)
}

#' @title aggregate_by_ent_typ
#' @name aggregate_by_ent_typ
#' @param data_with_ent_typ data that contains 'ent_typ_txt' col
#' @return data aggregated by ent_typ
aggregate_by_ent_typ <- function(data_with_ent_typ) {
  aggregated_data <- data_with_ent_typ %>%
    group_by(ent_typ_txt) %>%
    summarise(
      count = sum(ent_typ_txt == ent_typ_txt),
      tot = nrow(data_with_ent_typ),
      perc = round((count / tot) * 100, 2)
    )
  return(aggregated_data)
}

#' @title aggregate_by_checkin_and_ent_typ
#' @name aggregate_by_checkin_and_ent_typ
#' @description breaks the data down by check in group for certain ent_typ
#' @param intial_data dataframe that has 'ent_typ_txt' and 'check_in_group' cols
#' @param ent_typ "MU" or "SU"
#' @return data aggregated by check in group
aggregate_by_checkin_and_ent_typ <- function(intial_data, ent_typ) {
  filtered_df <- intial_data %>%
    filter(ent_typ_txt == ent_typ) %>%
    group_by(check_in_group) %>%
    summarise(
      count = n(),
      tot = sum(intial_data$ent_typ_txt == ent_typ),
      perc = round(count / tot * 100, 2)
    )
  
  # Reorder the check in group order
  filtered_df$check_in_group <- factor(filtered_df$check_in_group, levels = c("Checked in", "Not checked in"))
  return(filtered_df)
}

#' @title aggregate_by_ent_size
#' @name aggregate_by_ent_size
#' @description breaks MUs down into ent_size and calculate check in rates
#' @param ent_size_data data that has 'ent_size_code_txt' col
#' @return data aggregated by check in and ent_size
aggregate_by_ent_size <- function(ent_size_data) {
  ent_size_code_MU_login_id_count <- ent_size_data %>%
    filter(ent_typ_txt == "MU") %>%
    group_by(ent_id, ent_size_code_txt) %>%
    summarise(
      login_id_count = n()
    )
  
  ent_size_code_MU_df <- inner_join(ent_size_data, ent_size_code_MU_login_id_count, by = "ent_id") %>%
    select(ent_id, ent_size_code_txt.x, check_in_group) %>%
    group_by(ent_size_code_txt.x) %>%
    summarise(
      all_checked_in_count = sum(check_in_group == "Checked in"),
      all_delinquent_count = sum(check_in_group == "Not checked in"),
      tot_count = all_checked_in_count + all_delinquent_count,
      checked_in_group = round((all_checked_in_count / tot_count) * 100, 2),
      not_checked_in_group = round((all_delinquent_count / tot_count) * 100, 2)
    )
  
  return(ent_size_code_MU_df)
}

#' @title label_checkin_group_for_data
#' @name label_checkin_group_for_data
#' @description labeled data with check in group
#' @param data data queried from survey
#' @return labeled dataframe with check in group for suvey
label_checkin_group_for_data <- function(data) {
  labeled_data <- data %>%
    mutate(check_in_group = case_when(
      delinquent == 0 ~ "Checked in",
      responded == 0 ~ "Not checked in",
      TRUE ~ "Partial checked in"
    )
    )
  return(labeled_data)
}

#' @title calc_perc_check_in
#' @name calc_perc_check_in
#' @description calculates the percentage of check in group in survey
#' @param datadata
#' @return data breakdown by check in group for survey
calc_perc_check_in <- function(data) {
    summary <- data %>%
    group_by(check_in_group) %>%
    summarise(
      count = n(),
      tot = nrow(data),
      perc = round((count / tot) * 100, 2)
    )
    return(summary)
}

#' @title calc_perc_check_in_aies
#' @name calc_perc_check_in_aies
#' @description calculates the percentage of check in group in AIES
#' @param data AIES data
#' @return data breakdown by check in group for AIES
calc_perc_check_in_aies <- function(aies_data) {
  summary <- aies_data %>%
    filter(ent_typ_txt == "MU") %>%
    select(login_id, check_in_group) %>%
    group_by(check_in_group) %>%
    summarise(
      count = n(),
      tot = sum(aies_data$ent_typ_txt == "MU"),
      perc = round((count / tot) * 100, 2)
    )
  # Add in an empty row to AIES df because AIES doesn't have partial check in group
  new_row <- data.frame(
    check_in_group = "Partial checked in",
    count = 0,
    tot = nrow(aies_data),
    perc = 0
  )
  
  summary <- rbind(summary, new_row)
  return(summary)
}

#' @title auth_status_breakdown
#' @name auth_status_breakdown
#' @description a breakdown of login_ids by check in status and auth_status
#' @param data data queried from collection database
#' @param submit_date_df dataframe that contains submit_date cols
#' @return data breakdown by auth_status and check in group
auth_status_breakdown <- function(data, submit_date_df) {
  # Merge data to get submit_date
  submit_date <- dplyr::left_join(submit_date_df, data, by = 'login_id')
  
  # Convert into data.table
  submit_date <- as.data.table(submit_date)
  check_in_and_auth_status_summ <- submit_date[, .N, by = .(check_in_group, auth_status)]
  
  # Get the total of each check in group
  check_in_tot <- submit_date %>%
    group_by(check_in_group) %>%
    summarise(
      tot = n()
    )
  
  # Merge to get all cols from both dfs
  check_in_and_auth_status_summ <- inner_join(check_in_and_auth_status_summ, check_in_tot, by = 'check_in_group')
  
  # Calculate the percentage
  check_in_and_auth_status_summ <- check_in_and_auth_status_summ %>%
    mutate(perc = round(N / tot * 100, 2))
  
  return(check_in_and_auth_status_summ)
}
