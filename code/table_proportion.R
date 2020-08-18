library(questionr)
library(tidyverse)

analyzer_prop <- function(x, data,
                     weighting_function = NULL, main_col_name = "Overall", ...) {
  strata <- list(...)
  strata <- unlist(strata)
  strata <- strata[!is.na(strata)]
  data <- filter(data, !is.na(!!sym(x)))
  if (is.null(weighting_function)) {
    weights <- rep(1, nrow(data))
  } else {
    weights <- weighting_function(data)
  }
  
  x_data <- data[[x]]
  
    table <- wtd.table(x_data, rep(1, length(x_data)), weights = weights)
    if(is_empty(table)) {
      table <- tibble("data" = "", !!main_col_name := "")
    } else {
      table <- prop(table)
      table <- as.data.frame.matrix(table)
      table[,1] <- row.names(table)
      table <- table[-nrow(table),]
      table <- table[order(as.numeric(table[,ncol(table)]),
                           decreasing = T), c(1, ncol(table))] %>%
        mutate(Total = round(Total, 2))
      table <- rbind(c(x, main_col_name), table)
      table <- as.data.frame(table)
      names(table) <- c("data", main_col_name)
    }
  if (length(strata) > 0) {
    for (i in 1:length(strata)) {
      groups <- unique(unlist(data[strata[[i]]]))
      groups <- groups[!is.na(groups)]
      for (j in 1:length(groups)) {
        new_data <- filter(data, !!sym(strata[[i]]) == groups[j])
        new_table <- analyzer_prop(x, new_data, 
                              #analysisplan, 
                              weighting_function, groups[j], NA)
        table <- left_join_NA(table, new_table, by = "data")
      }
    }
  }
  return(table)
}

left_join_NA <- function(x, y, ...) {
  left_join(x = x, y = y, by = ...) %>% 
    mutate_each(list(~replace(., which(is.na(.)), 0)))
}

table_maker_prop <- function(data, questionnaire_object, questionnaire, choices, weighting_function = NULL,
                        labels = T, language = NULL, 
                        main_col_name = "overall", ...) {
  # collecting strata information
  strata <- list(...)
  strata <- strata[!is.na(strata)]
  strata <- unlist(strata)
  
  # getting analysis names, don't analyze strata
  if (!is.null(strata)) {
    analysis_names <- names(select(data, -one_of(strata)))
    #analysis_names <- analysisplan[["dependent.variable"]]
  } else {
    analysis_names <- names(data)
    #analysis_names <- analysisplan[["dependent.variable"]]
  }
  
  # detect which are in the questionnaire
  #in_questionnaire <- analysis_names %in% questionnaire$name
  
  # get initial table output
  
  if (!is.null(strata)) {
    table_output <- bind_rows(map(analysis_names,
                                   analyzer_prop, 
                                   data,
                                   #analysisplan,
                                   weighting_function,
                                   main_col_name,
                                   strata))
  } else {
    table_output <- bind_rows(map(analysis_names,
                                   analyzer_prop,
                                   #analysisplan,
                                   data, 
                                   weighting_function,
                                   main_col_name))
  }
  
  # Editing select multiple binary options
  # Removes extra rows so that we end up with # of rows for # of options
  
  #if(sum(unlist(purrr::map(var.names, questionnaire_object$question_is_select_multiple)))>0){}
  
  # Getting question labels if requested
  
  if (labels) {
    if (is.null(language)) {
      label_col <- "label"
    } else {
      cols <- names(questionnaire)
      label_col <- str_detect(cols, paste0("label[\\W]{2}(?i)", language))
      label_col <- cols[label_col]
    }
    
    
    choice_indices <- match(table_output$data, choices$name)
    choice_labels <- choices[[label_col]]
    question_indices <- match(table_output$data, questionnaire$name)
    question_labels <- questionnaire[[label_col]]
    
    table_output <- table_output %>%
      mutate(data = ifelse(is.na(question_indices), 
                           data, 
                           ifelse(is.na(question_labels[question_indices]) | question_labels[question_indices] == "",
                                  data,
                                  question_labels[question_indices])),
             data = ifelse(is.na(choice_indices),
                           data,
                           choice_labels[choice_indices]))
    
    # Fixing select multiple labels
    
  }
  
  # Cleaning rows with question names
  
  split_rows <- table_output[,2] == main_col_name
  table_output[split_rows, 2:ncol(table_output)] <- ""
  
  return(table_output)
}

'%!in%' = Negate('%in%')