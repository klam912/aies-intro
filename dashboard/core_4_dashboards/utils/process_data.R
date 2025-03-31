#' @title grab_sector_code
#' @name grab_sector_code
#' @description substring the first two digits of a NAICS code
#' @param initial_data raw queried data
#' @return a dataframe with 'sector' col that stores NAICS 
#' sectors for login ids
grab_sector_code <- function(initial_data) {
  initial_data$sector <- case_when(
    is.na(initial_data$naics_code) ~ "Adds",
    TRUE ~ substr(initial_data$naics_code, 0, 2)
  )
  return(initial_data)
}

#' @title group_sectors
#' @name group_sectors
#' @description Group NAICS sector together (i.e. 44 and 45 into 44-45)
#' @param initial_data raw queried data
#' @return a dataframe with 'sector_naics_group' col that stores NAICS sector
#' groups for login ids
group_sectors <- function(initial_data) {
  data_with_naics_sector_group <- initial_data %>%
    filter(sector != "92") %>% # sector 92 is out of scope
    mutate(sector_naics_group = case_when(
      sector %in% c("31", "32", "33") ~  "31-33",
      sector %in% c("44", "45") ~ "44-45",
      sector %in% c("48", "49") ~ "48-49",
      is.na(sector) ~ "Add", # for new logins with blank NAICS
      TRUE ~ sector
    ) 
    )
  return(data_with_naics_sector_group)
}

#' @title convert_negative_into_NA
#' @name convert_negative_into_NA
#' @description labels negative values for a specified variable as NA
#' @param data_with_negative_values dataframe that initially contain negative
#' values for a variable
#' @param params list of parameters that contain the variable of interest
#' @return dataframe without negative values
convert_negative_into_NA <- function(data_with_negative_values, params) {
  # Set df as data.table type
  data_without_negative_values <- setDT(data_with_negative_values)
  
  # Convert negative values into NA
  data_without_negative_values[get(params$variable) < 0, (params$variable) := NA]
  return(data_without_negative_values)
}


#' @title summarise_perc
#' @name summarise_perc
#' @description calculates the percentage of values that are 0s and NAs per
#' login id
#' @param initial_data dataframe that contains values for a specific variable
#' @param group_by_variables a list containing variables to group the data by
#' @param params list of parameters that contain the variable of interest
#' @return a summary that calculates the percentage of zero and NA responses
#' per login id
#' @example 
#' # Initialize an example df
#' raw_data <- data.frame(
#'  login_id = c(1,2,3,4),
#'  reporting_id = c(1,2,3,4),
#'  rcpt_tot_val = c(NA, 0, 5000, 31)
#' )
#' 
#' # Initialize variables used in the group by
#' group_by_variables <- c("login_id")
#' 
#' # When we run this function, it will calculate the percentage of reporting_ids
#' whose responses are zeros or left as blank.
#' result <- summarise_perc(raw_data, group_by_variables, params)
#' @export
summarise_perc <- function(initial_data, group_by_variables, params) {
  # Convert into data.table type
  initial_data <- as.data.table(initial_data)
  
  grouped_data <- initial_data[,.(
    count = .N,
    total = sum(get(params$variable), na.rm = TRUE),
    zero_perc = (sum(get(params$variable) == 0, na.rm = TRUE) / .N) * 100,
    na_perc = (sum(is.na(get(params$variable))) / .N) * 100
  ), by = group_by_variables]
  return(grouped_data)
}

#' @title label_response_groups
#' @name label_response_groups
#' @description categorizes a login id's response group based on its percentage
#' of zero and NA responses
#' @param data_with_perc_0_and_NA_responses dataframe that contains calculated 
#' percentage of 0 and NA responses
#' @return dataframe with 'group' col 
label_response_groups <- function(data_with_perc_0_and_NA_responses) {
  data_with_resp_group <- data_with_perc_0_and_NA_responses %>% mutate(
    group = case_when(
      (zero_perc == 0) & (na_perc == 0) ~ "Responded",
      zero_perc == 100 ~ "All responded as zero",
      na_perc == 100 ~ "All responded as NA",
      TRUE ~ "Partial"
    )
  )
  return(data_with_resp_group)
}

#' @title create_valuebox_df
#' @name create_valuebox_df
#' @description cleans the data into a dataframe ready to be used to make 
#' valueBoxes
#' @param query_output_data originally queried data
#' @param survey_level level of the survey ("estab" or "KAU")
#' @param params list of parameters that contain the variable of interest
#' @param submit_date_df dataframe that contains submit_date and check in group cols
#' @return dataframe ready to be used to make valueBoxes
#' @example 
#' # Initialize data
#' example_data <- data.frame(
#'  login_id = c(1,2,3,4),
#'  reporting_id = c(1,2,3,4),
#'  kau_id = c(1,2,3,4),
#'  ent_id = c(1,2,3,4),
#'  rcpt_tot_val = c(NA, 0, 5000, 31)
#' )
#' 
#' # When we run this function, it will create a dataframe summary that calculates
#' the number of login ids per response group and check in group. 
#' # For this example, we want to find the total of checked in login id counts 
#' per response group at the establishment level.
#' result <- create_valuebox_df(example_data, "estab", params, "Checked in")
#' @export
create_valuebox_df <- function(query_output_data, survey_level = "estab", params, submit_date_df) {
  # Convert negative values to NAs
  data_without_negatives <- convert_negative_into_NA(query_output_data, params)
  
  # Calculate zero_perc, NA_perc based on the level
  if (survey_level == "estab") {
    # Merge with general_df to get submit_date col
    data_with_check_in_group_col <- left_join(data_without_negatives, submit_date_df, by = "login_id")
    group_by_vars <- c("login_id", "check_in_group")
    data_with_perc_0_and_NA_responses <- summarise_perc(data_with_check_in_group_col, group_by_vars, params)
    
  } else if (survey_level == "KAU") {
    group_by_vars <- c("login_id")
    data_with_perc_0_and_NA_responses <- summarise_perc(data_without_negatives, group_by_vars, params)
    # Merge with submit_date_df to get submit_date col
    data_with_perc_0_and_NA_responses <- left_join(data_with_perc_0_and_NA_responses, submit_date_df, by = "login_id")
  }
  
  # Label response groups
  valueboxes_df <- label_response_groups(data_with_perc_0_and_NA_responses)
  
  # Count login ids per response group per check in group
  valueboxes_df <- valueboxes_df %>%
    group_by(group, check_in_group) %>%
    summarise(
      login_count = length(unique(login_id))
    )
  
  return(valueboxes_df)
}

#' @title factorize_resp_groups
#' @name factorize_resp_groups
#' @description re-order the response groups 
#' @param data_with_unordered_resp_groups
#' @return dataframe with the response groups in order
factorize_resp_groups <- function(data_with_unordered_resp_groups) {
  data_with_ordered_resp_groups <- data_with_unordered_resp_groups
  data_with_ordered_resp_groups$group <- factor(data_with_ordered_resp_groups$group, levels = c("Responded", "Partial", "All responded as zero", "All responded as NA"))
  
  return(data_with_ordered_resp_groups)
}

#' @title calc_sum_login_id_per_sector
#' @name calc_sum_login_id_per_Sector
#' @description calculates the total of login ids per sector
#' @param data_with_sector dataframe that contains 'sector' col
#' @return data grouped by NAICS sector and its respective login id counts
calc_sum_login_id_per_sector <- function(data_with_sector) {
  aggregated_data_by_NAICS_sector <- data_with_sector %>%
    # filter(check_in_group == check_in_group) %>%
    group_by(sector_naics_group, check_in_group) %>%
    summarise(
      tot_login_id_count_per_sector = sum(login_id_count)
    ) %>%
    ungroup()
  return(aggregated_data_by_NAICS_sector)
}

#' @title calc_perc_login_id_in_sector
#' @name calc_perc_login_id_in_sector
#' @description calculates percentage of login ids within each NAICS sector
#' @param data_with_tot_login_ids_per_sector data aggregated by sector with login
#' id counts
#' @param naics_sector_data data that has 'sector_naics_group' col
#' @return dataframe that is merged between the two datasets to get a 'perc' col
calc_perc_login_id_in_sector <- function(data_with_tot_login_ids_per_sector, naics_sector_data) {
  filtered_data_by_check_in_group <- naics_sector_data %>% filter(!is.na(sector_naics_group))

  # Merge the two dataframes together to get a single dataframe with all information
  merged_df <- right_join(data_with_tot_login_ids_per_sector, filtered_data_by_check_in_group, by = c("sector_naics_group", "check_in_group")) %>%
    # select(sector_naics_group, group, check_in_group.x, login_id_count, tot_login_id_count_per_sector) %>%
    mutate(
      perc = round((login_id_count / tot_login_id_count_per_sector) * 100, 2)
    )
  
  return(merged_df)
}

#' @title group_sectors_into_groups
#' @name group_sectors_into_groups
#' @description add an extra col 'sector_naics_group' that groups NAICS sectors
#' @param data_with_naics_col dataframe that has 'naics_code' col
#' @param submit_date_df dataframe that contains submit_date and check in group cols
#' @return dataframe with 'sector_naics_group' col
group_sectors_into_groups <- function(data_with_naics_col, params, submit_date_df) {
  # Merge with general_df to get submit_date col
  merged_df <- left_join(data_with_naics_col, submit_date_df, by = "login_id")
  
  # Get sector NAICS
  sector_NAICS_df <- grab_sector_code(merged_df)
  
  # Group sector together
  sector_NAICS_df <- group_sectors(sector_NAICS_df)
  
  # For the value of the variable that are negatives, turn into NAs
  sector_NAICS_df <- convert_negative_into_NA(sector_NAICS_df, params)
  
  return(sector_NAICS_df)
}

#' @title aggregate_data_by_ent_typ
#' @name aggregate_data_by_ent_typ
#' @description aggregate login ids by ent_typ to create a df that is ready for
#' plotting
#' @param query_output data queried at the KAU level
#' @param check_in_group "Checked in" or "Not checked in"
#' @param params list of parameters that contain the variable of interest
#' @param submit_date_df dataframe that contains submit_date and check in group cols
#' @return dataframe that is aggregated by ent_typ
#' @example 
#' # Initialize example data
#' example_data <- data.frame(
#'  login_id = c(1,2,3,4),
#'  reporting_id = c(1,2,3,4),
#'  kau_id = c(1,2,3,4),
#'  ent_id = c(1,2,3,4),
#'  rcpt_tot_val = c(NA, 0, 5000, 31),
#'  check_in_group = c("Checked in", "Checked in", "Not checked in", "Checked in")
#' )
#' 
#' # When we run this function, it will first categorize login ids into their appropriate
#' response groups. Then, it will merge that summary df with submit_date_df to get
#' other columns relating to company characteristics (i.e. ent_typ_txt). Once merged,
#' it will group the login ids by ent_typ_txt and sum up the login ids per ent_typ_txt.
#' result <- aggregate_data_by_ent_typ(example_data, "Checked in", params, submit_date_df)
#' 
aggregate_data_by_ent_typ <- function(query_output, check_in_group = "Checked in", params, submit_date_df) {
  # Convert dataframe into a data.table type
  converted_df <- setDT(query_output)
  
  # Merge the data with general_df to get check_in_group col
  converted_df <- merge(query_output, submit_date_df, by = "login_id")

  # Calculate percentage of 0 and NA responses
  group_by_variables <- c("login_id")
  data_with_resp_groups <- summarise_perc(converted_df, group_by_variables, params)
  # Categorize response groups
  data_with_resp_groups <- label_response_groups(data_with_resp_groups)
  # Reorder the resp group
  data_with_resp_groups <- factorize_resp_groups(data_with_resp_groups)

  # Merge with converted_df to get their ent_typ_txt and check_in_group cols
  merged_df_with_ent_typ <- merge(data_with_resp_groups, converted_df, by = "login_id") %>%
    rename(ent_typ_txt = ent_typ_txt.x)

  # Count total login ids within each ent_typ and check_in_group
  total_login_ids_per_ent_typ <- merged_df_with_ent_typ[, .(
    total_per_ent_typ = length(unique(login_id))
  ), by = c("ent_typ_txt", "check_in_group")]
  
  # Merge with merged_df_with_ent_typ to get other cols
  merged_df_with_ent_typ <- left_join(merged_df_with_ent_typ, total_login_ids_per_ent_typ, by = c("ent_typ_txt"))
  
  # Calculate the percentage of login ids per resp_group and ent_typ
  aggregated_data_by_ent_typ_and_resp_group <- merged_df_with_ent_typ %>%
    filter(check_in_group.x == check_in_group) %>%
    group_by(check_in_group.x, group, ent_typ_txt) %>%
    summarise(
      login_id_count = length(unique(login_id))
    ) %>%
    group_by(ent_typ_txt) %>%
    mutate(perc = round(login_id_count / sum(login_id_count) * 100, 2),
           tot_per_ent_typ = sum(login_id_count))

  return(aggregated_data_by_ent_typ_and_resp_group)
}


#' @title calculate_and_label_resp_groups
#' @name calculate_and_label_resp_groups
#' @description calculates the percentage of login ids with 0 and NA responses
#' and label them with appropriate response groups
#' @param data_with_sector dataframe containing 'sector_naics_group' col
#' @param survey_level "estab" or "KAU"
#' @param params list of parameters that contain the variable of interest
#' @return dataframe where each login id is labeled with a response group
calculate_and_label_resp_groups <- function(data_with_sector, survey_level = "estab", params) {
  # Initialize columns to aggregate
  if (survey_level == "estab") {
    group_by_vars <- c("sector_naics_group", "check_in_group", "login_id")
  } else if (survey_level == "KAU") {
    group_by_vars <- c("login_id")
  }
  # Calc zero_perc and NA_perc
  summ_0_NA_perc <- summarise_perc(data_with_sector, group_by_vars, params)
  
  # Label response groups
  summ_with_resp_group <- label_response_groups(summ_0_NA_perc)
  return(summ_with_resp_group)
}

#' @title aggregate_login_id_by_sector_checkin_and_resp_group
#' @name aggregate_login_id_by_sector_checkin_and_resp_group
#' @description aggregate login ids by sector, response group, and check in group
#' @param data_summ_resp_group dataframe containing labeled response groups
#' @param data_with_sector_cols dataframe containing sector groups
#' @param survey_level "estab" or "KAU"
#' @param check_in_group "Checked in" or "Not checked in"
#' @param submit_date_df dataframe that contains submit_date and check in group cols
#' @return data aggregated by sector, checkin, and response groups
#' @example 
#' example_data <- data.frame(
#'  login_id = c(1,2,3,4),
#'  reporting_id = c(1,2,3,4),
#'  kau_id = c(1,2,3,4),
#'  ent_id = c(1,2,3,4),
#'  rcpt_tot_val = c(NA, 0, 5000, 31),
#'  group = c("Responded", "Partial", "All responded as zero", "Partial"),
#' )
#' 
#' sector_data <- data.frame(
#'  login_id = c(1,2,3,4),
#'  sector_naics_group = c("44-45", "11", "55", "22", "22")
#' )
#' 
#' submit_date_df <- data.frame(
#'  login_id = c(1,2,3,4),
#'  check_in_group = c("Checked in", "Not checked in", "Checked in", "Checked in")
#' )
#' 
#' # When you run this function, it first will join other dataframes that have
#' cols, such as check_in_group and sector groups. Then it will sum up login ids
#' that are in each sector group, which is further grouped by check in and response
#' groups. Finally, it will calculate the percentage of login ids in these
#' groups.
#' result <- aggregate_login_id_by_sector_checkin_and_resp_group(example_data, sector_data, "estab", "Checked in", submit_date_df)
aggregate_login_id_by_sector_checkin_and_resp_group <- function(data_summ_resp_group, data_with_sector_cols, survey_level = "estab", check_in_group = "Checked in", submit_date_df) {
  # Merge to get sector and submit date columns
  if (survey_level == "KAU") {
    naics_summ <- left_join(data_summ_resp_group, submit_date_df, by = "login_id")
    naics_summ <- left_join(naics_summ, data_with_sector_cols, by = "login_id")
    naics_summ <- naics_summ %>% rename(check_in_group = check_in_group.x)
  } else if (survey_level == "estab") {
    naics_summ <- data_summ_resp_group
  } else {
    paste("Survey level doesn't exist")
  }
    
  # Convert to data.table type
  naics_summ <- as.data.table(naics_summ)
  # Summarise login_id_count
  naics_summ <- naics_summ[,.(
    login_id_count = .N
  ), by = .(sector_naics_group, group, check_in_group)]
  
  # Set factor levels for custom order in bar chart
  naics_summ <- factorize_resp_groups(naics_summ)
  
  # Calculate the total of login_ids per sector and response group
  naics_tot <- calc_sum_login_id_per_sector(naics_summ)
  # Calculate the percentage of login_ids per sector and response group
  naics_perc <- calc_perc_login_id_in_sector(naics_tot, naics_summ)
  
  return(naics_perc)
}

#' @title process_data_for_naics_graph
#' @name process_data_for_naics_graph
#' @description processes query output to create a dataframe ready to be plot
#' @param data_with_naics_col dataframe containing 'naics_code' col
#' @param survey_level "estab" or "KAU"
#' @param check_in_group "Checked in" or "Not checked in"
#' @param params list of parameters that contain the variable of interest
#' @param submit_date_df dataframe that contains submit_date and check in group cols
#' @return data aggregated by sector, checkin, and response groups
process_data_for_naics_graph <- function(data_with_naics_col, survey_level = "estab", check_in_group = "Checked in", params, submit_date_df) {
  # Group login ids by sector
  data_with_sector_groups <- group_sectors_into_groups(data_with_naics_col, params, submit_date_df)
  
  # Label df with response groups
  data_summ_resp_group <- calculate_and_label_resp_groups(data_with_sector_groups, survey_level, params)
  
  # Aggregate login ids by sector, check in, and response group
  data_aggregated_by_sector_checkin_and_resp_groups <- aggregate_login_id_by_sector_checkin_and_resp_group(data_summ_resp_group, data_with_sector_groups, survey_level, check_in_group, submit_date_df)

  return(data_aggregated_by_sector_checkin_and_resp_groups %>% filter(check_in_group == check_in_group))
}