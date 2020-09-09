library(hypegrammaR)
library(composr)
library(dplyr)

aggregated_data<-load_data("./input/data.csv")
names(aggregated_data) <- sub("[_]*","",names(aggregated_data))
aggregated_data<-aggregated_data %>%filter(consent=="yes")

survey<-read.csv("./input/survey.csv", stringsAsFactors = F)
choices<-read.csv("./input/choices.csv", stringsAsFactors = F)
questionnaire <-load_questionnaire(data = aggregated_data,
                                   questions = survey,
                                   choices = choices,
                                   choices.label.column.to.use = "label")

dap<-load_analysisplan("./input/dap.csv")

# dap$dependent.variable.type<-sapply(dap$dependent.variable,questionnaire$question_type,
#        from.questionnaire = T,
#        from.data = T,
#        data = vulnerability_data)
# dap$dependent.variable.type[dap$dependent.variable.type %in% c("select_one","select_multiple")]<-"categorical"
# dap$dependent.variable.type[dap$dependent.variable.type=="numeric"] <- "numerical"

dap %>% readr::write_csv("./input/dap_final.csv.")

results <- from_analysisplan_map_to_output(data = dap_data,
                                           analysisplan = dap,
                                           weighting = NULL,
                                           questionnaire = questionnaire)

labeled_results <- lapply(results$results, map_to_labeled,questionnaire)
hypegrammaR::map_to_master_table(results_object = labeled_results,
                                 filename ="./output/result_hypegrammaR.csv")
