re_create_parent_question <- function(df, multiple_choices, separator){
  #Loop throughout received questions
  for (m in 1:length(multiple_choices)) {
    choice_char_count <- nchar(multiple_choices[m]) + 2
    choices <- df %>% select(starts_with(paste0(multiple_choices[m], separator)))
    #Loop throughout choices of each question
    for (c in 1:length(choices)) {
      choice_name <- substring(names(choices[c]),choice_char_count)
      #Loop throughout rows of each choice
      for (r in 1:nrow(choices)) {
        if (!is.na(choices[r,c]) & as.numeric(choices[[c]][r]) == 1) {
          temp <- str_detect(df[[multiple_choices[m]]][r], choice_name)
          if(!is.na(temp) & !temp){
            #Insert the choice name to parent question
            df[r, multiple_choices[m]] <- paste(df[[multiple_choices[m]]][r], choice_name, sep = " ")
          }
        }
      }
    }
    cat("\014")
    print (paste("Creating parent ", m, "of", length(multiple_choices)))
  }
  return(df)
}

multiple_choices <- c(
  "relocate_pushpull",
  "water_barrier",
  "health_barrier",
  "move_why_m",
  "move_why_f",
  "food_source",
  "covid_info",
  "covid_safeprac",
  "covid_coping",
  "mhpss_secondary",
  "soldout_item"
  
)


parent_created <- re_create_parent_question(vulnerability_data, multiple_choices, separator = ".")
