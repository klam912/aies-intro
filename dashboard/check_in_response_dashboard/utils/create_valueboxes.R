# GLOBAL VARIABLE DECLARED HERE 
# Aesthetic options for check in groups
CHECK_IN_VALUEBOX_AES <- data.frame(
  group = c("Checked in", "Not checked in"),
  color = c("#205493", "#FF7043"),
  icon = c("ion-checkmark-circled", "ion-close-circled")
)

# ------------------------------------------
#' @title return_check_in_count
#' @name return_check_in_count
#' @description return the count of login ids for a check in group
#' @param data_with_checkin_group data that contains 'check_in_group' col
#' @param check_in_status "Checked in" or "Not checked in"
#' @return total login ids count per check in group
return_check_in_count <- function(data_with_checkin_group, check_in_status) {
  count <- data_with_checkin_group %>%
    filter(check_in_group == check_in_status) %>%
    summarise(
      count = n()
    ) %>%
    pull(count)
  
  return(prettyNum(count, big.mark = ","))
}


# Creates a valuebox based on level and color (Check-in tab)
#' @title create_valueboxes_checkin
#' @name create_valueboxes_checkin
#' @description create valueBox that displays a login id count for a check_in_group
#' @param login_id_count total number of login id for a check_in_group
#' @param check_in_group "Checked in" or "Not checked in"
#' @return a valueBox with the appropriate color based on the check in group
create_valueboxes_checkin <- function(login_id_count, check_in_group) {
  sentence <- paste(login_id_count, "login_ids")
  color <- CHECK_IN_VALUEBOX_AES$color[CHECK_IN_VALUEBOX_AES$group == check_in_group]
  icon <- CHECK_IN_VALUEBOX_AES$icon[CHECK_IN_VALUEBOX_AES$group == check_in_group]
  
  return(valueBox(sentence, 
                  icon = icon,
                  color = color))
}