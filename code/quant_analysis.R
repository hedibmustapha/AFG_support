#rm(list = ls())
source("code/table_function.R")
source("code/table_proportion.R")

library(koboquest)
library(parallel)
library(crayon)

questions <- read_excel("./input/reach_afg_isets_kobo_siteprofiling_rollout_edited.xlsx",1)
choices <- read_excel("./input/reach_afg_isets_kobo_siteprofiling_rollout_edited.xlsx",2)

questionnaire<-load_questionnaire(data = aggregated_data,
                                  questions = questions,
                                  choices = choices,
                                  choices.label.column.to.use = "label")

aggregated_data <- mutate_if(aggregated_data, is.character, na_if, "")


text <- filter(questions, str_detect(type, "(\\btext\\b)|(\\bnote\\b)"))$name

data_to_analyze <- aggregated_data %>% 
  select(-one_of(text)) %>%
  select(-start, -end, -date,-end_survey,-deviceid,-enumerator_id,-ki_gender,-ki_age,
       -consent, -index, -uuid, -submission_time, -id,-gbv_incident,-validation_status) %>%
  select_if(~ !(all(is.na(.x)) | all(. == "")))

analysis_aggregated_data <- table_maker(data = data_to_analyze,
                                   questionnaire_object = questionnaire,
                                   questionnaire = questions,
                                   choices = choices,
                                   weighting_function = NULL,
                                   labels = T,
                                   language = NULL,
                                   main_col_name = "overall",
                                   var_tosum = var_tosum,
                                   "region",
                                   "province",
                                   "district",
                                   "tenure_sec_insec",
                                   "access_toservice",
                                   "shelter_condition",
                                   "over_less_5years",
                                   "mixed_sep_popn",
                                   "return_status",
                                   "location_type",
                                   "site_type")

analysis_aggregated_data %>% write_csv("./output/analysis_aggregated_data.csv")

nonaggregated_data_to_analyze<- nonaggregated_data %>% 
  select(ki_gender,ki_age,ki_gender_age,region,province,district,
               tenure_sec_insec,access_toservice ,shelter_condition,
                over_less_5years,mixed_sep_popn,
                return_status,location_type,site_type)%>%
  select_if(~ !(all(is.na(.x)) | all(. == "")))%>%
  mutate_if(is.character, na_if, "")

analysis_nonaggregated_data<- table_maker_prop(data = nonaggregated_data_to_analyze,
                 questionnaire_object = questionnaire,
                 questionnaire = questions,
                 choices = choices,
                 weighting_function = NULL,
                 labels = T,
                 language = NULL,
                 main_col_name = "overall",
                 "region",
                 "province",
                 "district",
                 "tenure_sec_insec",
                 "access_toservice",
                 "shelter_condition",
                 "over_less_5years",
                 "mixed_sep_popn",
                 "return_status",
                 "location_type",
                 "site_type")

analysis_nonaggregated_data %>% write_csv("./output/analysis_nonaggregated_data.csv")
