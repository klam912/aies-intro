# GLOBAL VARIABLE DECLARED HERE 
# Color for check in group
CHECK_IN_GROUP_COLORS <- data.frame(
  group = c("Checked in", "Not checked in"),
  color = c("#205493", "#FF7043")
)

# --------------------------------------------------------
  
#' @title return_check_in_count_for_valuebox
#' @name return_check_in_count_for_valuebox
#' @description returns the login id count for a check in group valuebox
#' @param checked_in_data data with the appropriate check in group
#' @return a total login id count that is formatted using prettyNum to 
#' format numbers
return_check_in_count_for_valuebox <- function(checked_in_data) {
  count <- sum(checked_in_data$login_count)
  
  return(prettyNum(count, big.mark = ","))
}

#' @name create_caption_valueboxes_resp_group
#' @title create_caption_valueboxes_resp_group
#' @description create a caption for each response group's valuebox
#' @param resp_group response group
#' @param survey_level "estabs" or "KAUs" --> sample units
#' @param params a list of parameters containing different variables
#' @return a valuebox caption
create_caption_valueboxes_resp_group <- function(resp_group, survey_level, params) {
  if (resp_group == "Responded") {
    caption = paste("Responded: all", survey_level, "reported", "<br>", params$variable_name, "> 0")
  } else if (resp_group == "Partial") {
    caption = paste("Partial responded: some", survey_level, "reported", params$variable_name, "> 0")
  } else if (resp_group == "All responded as zero") {
    caption = paste("All", survey_level, "reported", params$variable_name, "as 0")
  } else if (resp_group == "All responded as NA") {
    caption = paste("All", survey_level, "left", params$variable_name, "blank")
  } else {
    return("Response group doesn't exist")
  }
  return(caption)
}

#' @title create_valueboxes_resp_group
#' @name create_valueboxes_resp_group
#' @description create a valueBox with the corresponding color and caption based
#' on response group name and check in group
#' @param count the total login id count for a given response group within a 
#' check in group
#' @param check_in_group "Checked in" or "Not checked in"
#' @param resp_group "Responded", "Partial", "All responded as zero", "All
#' responded as NA"
#' @param survey_level "establishments" or "KAUs"
#' @param params a list of parameters containing different variables
#' @return a valueBox with the correct label, color, caption, and unit
create_valueboxes_resp_group <- function(count, check_in_group, resp_group, survey_level, params) {
  count <- prettyNum(count, big.mark = ",")
  sentence <- paste(count, "login ids")
  color <- CHECK_IN_GROUP_COLORS$color[CHECK_IN_GROUP_COLORS$group == check_in_group]
  caption <- create_caption_valueboxes_resp_group(resp_group, survey_level, params)
  
  return(valueBox(sentence, color = color, caption = caption))
}

#' @title create_valueboxes_general_checkin
#' @name create_valueboxes_general_checkin
#' @description creates a valueBox for the check in groups that display the 
#' total login id count
#' @param count the sum of login id count for that check in group
#' @param check_in_group "Checked in" or "Not checked in"
#' @return a valueBox with the appropriate color and icon
create_valueboxes_general_checkin <- function(count, check_in_group) {
  count <- prettyNum(count, big.mark = ",")
  color <- CHECK_IN_GROUP_COLORS$color[CHECK_IN_GROUP_COLORS$group == check_in_group]
  sentence <- paste("Total", check_in_group, ":", count, "login ids")
  
  return(valueBox(sentence, color = color))
}