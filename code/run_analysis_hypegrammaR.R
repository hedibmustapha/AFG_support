library(hypegrammaR)
library(readxl)
library(readr)
library(parallel)

# remove.packages("hypegrammaR", c(.libPaths(),"C:/Users/REACH/Documents/R/win-library/3.4"))
# Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE)
# devtools::install_github("ellieallien/hypegrammaR", build_opts = c())

questions <- read_excel("./MSNA/input/wash/questionnaire_kobo_msna_2020_combine_final_v3.xlsx",1)
questions<-read.csv("./input/wash/survey.csv",stringsAsFactors = F)
choices <- read_excel("./MSNA/input/wash/questionnaire_kobo_msna_2020_combine_final_v3.xlsx",2)
choices<-read.csv("./input/wash/choices.csv")
# choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)

data <- read_excel("./MSNA/input/wash/REACH_CAF_DATASET_CAR2001_JULY2020_V7.xlsx", sheet = "Clean data")%>%type_convert()
data<-load_data("./input/wash/data_autre_fixed.csv")
data <- mutate_if(data, is.character, na_if, "")
data<-data%>%mutate(
  strata.names=paste0(admin_1,"_",ig_8_statut)
)

data<-data%>%mutate(
  sante_4_60plus_sympt.peau = rep(NA,nrow(data)),
  sante_4_60plus_sympt.vomissement = rep(NA,nrow(data))
)
sampling_frame_admin1 <- load_samplingframe(file = "./input/wash/wash_samplingframe_admin1.csv")
sampling_frame_admin2 <- load_samplingframe(file = "./input/wash/wash_samplingframe_admin2.csv")

questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label")
analysisplan_overall <- load_analysisplan(file = "./input/wash/dap_overall.csv")
analysisplan_overall<-analysisplan_overall%>%filter(dependent.variable %in% names(data))
analysisplan_overall<-read.csv("./MSNA/input/wash/dap_overall.csv",stringsAsFactors = F)

check_input<-kobostandards::check_input(data = data, questions = questions, choices = choices ,samplingframe = sampling_frame_admin1,
                           analysisplan = analysisplan_overall)

weights_admin1 <-map_to_weighting(sampling.frame = sampling_frame_admin1,
                           data.stratum.column = "strata.names",
                           sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "strata.names",
                           data = data)
weights_admin2 <-map_to_weighting(sampling.frame = sampling_frame_admin2,
                                  data.stratum.column = "admin_2",
                                  sampling.frame.population.column = "population",
                                  sampling.frame.stratum.column = "admin_2",
                                  data = data)
weights_overall <-combine_weighting_functions(weight_function_1 = weights_admin1, weight_function_2 = weights_admin2)

results <- from_analysisplan_map_to_output(data = data,
                                           analysisplan = analysisplan_overall,
                                           weighting = weights_overall,
                                           questionnaire = questionnaire
)

labeled_results <- lapply(results$results, map_to_labeled,questionnaire)
#labeled_results_mantika <- lapply(results_mantika$results, map_to_labeled,questionnaire)

map_to_master_table(results_object = results$results, filename = "./output/wash/overall_result.csv",questionnaire = questionnaire)

result %>% map_to_labeled(questionnaire) -> result_labeled
map_to_file(result_labeled$summary.statistic,"../final_analysis/analysis_requests/_median_total_expenditure_hh_size.csv")

case <- map_to_case(hypothesis.type = "direct_reporting",
                    dependent.var.type = "categorical")
map_to_result(data = data,
              dependent.var = "sante_4_60plus_sympt",
              case = case,
              weighting = weights_overall,
              questionnaire = questionnaire
              )
dm<-dmerge(results = results$results)
