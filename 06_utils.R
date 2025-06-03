# 06_utils.R - Helper Functions and Utilities

# Function to check data integrity
check_data_integrity <- function(data_list) {
  required_data <- c("main_data", "word_data", "pie_data", "poverty_opers")
  missing_data <- setdiff(required_data, names(data_list))
  
  if (length(missing_data) > 0) {
    stop(paste("Missing required data:", paste(missing_data, collapse = ", ")))
  }
  
  # Check for required columns in main_data
  required_cols <- c("poverty", "adversity", "country", "poverty_oper")
  missing_cols <- setdiff(required_cols, names(data_list$main_data))
  
  if (length(missing_cols) > 0) {
    warning(paste("Missing columns in main_data:", paste(missing_cols, collapse = ", ")))
  }
  
  return(TRUE)
}

# Function to create a footer
create_footer <- function() {
  div(
    style = paste0(
      "position: fixed; bottom: 0; width: 100%; ",
      "background-color: ", THEME_CONFIG$bg_secondary, "; ",
      "padding: 10px; text-align: center; ",
      "border-top: 1px solid ", alpha(THEME_CONFIG$text_secondary, 0.2), "; ",
      "color: ", THEME_CONFIG$text_secondary, ";"
    ),
    tags$p("You can contact me at: n.ali@tilburguniversity.edu")
  )
}