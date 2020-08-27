select_percents <- function(x, n, df, survey_sheet, choice_sheet, return_what, language = NULL, exclude = NULL, x_name = NULL, group = NULL) {
  # if (is.null(x_name)) {
  #   x_name <- deparse(substitute(x))
  # }
  x_var <- df[[x]]
  group_var <- group_vars(df)
  
  # basic setup
  if (!is.null(group) & nrow(df) == length(x_var)) {
    x_var <- x_var[df[[group_var]] == group]
  }
  # x<- deparse(substitute(x))
  x_var <- x_var[!is.na(x_var)]
  
  if (length(x_var) == 0) {
    if (return_what == "label") {
      NA_character_
    } else {
      NA_integer_
    }
  } else {
    # Getting choices and labels
    l_name <- filter(survey_sheet, name == x)$type
    l_name <- str_remove(l_name, "(select_one |select_multiple )")
    choices <- filter(choice_sheet, list_name == l_name)$name
    
    if (!is.null(language)) {
      cols <- names(choice_sheet)
      col <- str_detect(cols, paste0("label[\\W]{2}(?i)", language))
      col <- cols[col]
    } else {
      col <- "label"
    }
    
    labels <- filter(choice_sheet, list_name == l_name)[[col]]
    # finding instances of choice options
    choice_rgx <- str_c("\\b", choices, "\\b")
    counts <- map_dbl(choice_rgx, ~sum(str_count(x_var, .x)))
    
    if (!is.null(exclude)) {
      choices <- choices[order(counts, decreasing = T)]
      while (choices[n] %in% exclude | is.na(choices[n])) {
        n <- n + 1
      }
    }
    
    if (return_what == "label") {
      labels <- labels[order(counts, decreasing = T)]
      labels[n]
    } else if (return_what == "percent") {
      counts <- counts[order(counts, decreasing = T)]
      paste0(round(100 * (counts[n] / length(x_var)), 0)," %")
    } else if (return_what == "count") {
      counts <- counts[order(counts, decreasing = T)][n]
    }
  }
}

round_pct<-function(data,var_name){
  paste0(round(mean(data[[var_name]],na.rm = T)*100,2)," %")
}

horizental_plot <- function(data,toplot,title){
  data %>% 
    count(!!sym(toplot)) %>% 
    mutate(pct=(prop.table(n)))%>%
    ggplot(aes(y = !!sym(toplot), x = pct, label = scales::percent(pct))) + 
    geom_col(fill = hypegrammaR:::reach_style_color_red(1), width = 0.7) +
    geom_text(position = position_dodge(width = .9),
              vjust = -0.5, 
              hjust = -0.2,
              size = 5)+
    labs(title = title,
         x = NULL,
         y = NULL) +
    # scale_y_continuous(expand = c(0, NA)) + 
    scale_x_continuous(labels=scales::percent, limits = c(0,1))+
    theme_minimal() + 
    theme(
      text = element_text(family = "Arial Narrow",colour = hypegrammaR:::reach_style_color_darkgrey(1)),
      plot.title = element_text(size = 20),
      axis.text = element_text(size = 15),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.ontop = TRUE
    )
}

top_3_dt<-function(data,var_name,title=NULL,col_name,survey,choices){
  result<-data.frame(var = c(select_percents(var_name,1,data,survey,choices,"label"),
                             select_percents(var_name,2,data,survey,choices,"label"),
                             select_percents(var_name,3,data,survey,choices,"label")),
                     percentage = c(select_percents(var_name,1,data,survey,choices,"percent"),
                                    select_percents(var_name,2,data,survey,choices,"percent"),
                                    select_percents(var_name,3,data,survey,choices,"percent")))
  names(result)[1]<-col_name
  return(datatable(result,rownames = F,caption = title))
}
