
vulnerability_data<-aggregated_data %>% mutate(
  susceptability_index1 = case_when(
    vul_chronic >= 0.05 ~ 2,
    vul_chronic < 0.05 ~ 0
  ),
  susceptability_index2 = case_when(
    vul_elderly >= 0.05 ~ 2,
    vul_elderly < 0.05 ~ 0
  ),
  adaptative_capacity_index1 = case_when(
    aid_govt == "yes" ~ 0,
    aid_govt == "no" ~ 3,
    aid_govt %in% c("no_consensus","dont_know") ~ NA_real_
  ),
  coping_capacity_index1 = case_when(
    soap_avail == "yes" ~ 0,
    soap_avail == "no" ~ 3,
    soap_avail %in% c("no_consensus","dont_know") ~ NA_real_
  ),
  coping_capacity_index2 = case_when(
    handwash_crowded == "yes" ~ 3,
    handwash_crowded == "no" ~ 0,
    handwash_crowded == "dont_know" ~ NA_real_
  ),
  coping_capacity_index3 = case_when(
    health_distance == "no_none" ~ 3,
    health_distance %in% c("yes","no_further") ~ 0
  ),
  coping_capacity_index4 = case_when(
    health_barrier.none == 0 ~ 3,
    health_barrier.none == 1 ~ 0
  ),
  adaptative_capacity_index2 = case_when(
    mhpss_secondary.none == 0 ~ 3,
    mhpss_secondary.none == 1 ~ 0
  ),
  susceptability_index3 = case_when(
    shelter_type %in% c("tent",
                        "makeshift",
                        "transitional",
                        "collective",
                        "open_space",
                        "unfinished",
                        "damaged") ~ 2,
    shelter_type %in% c("mud","brick") ~ 0,
    shelter_type %in% c("no_consensus","other") ~ NA_real_
  ),
  susceptability_index4 = case_when(
    vul_sleeping_space > 4 ~ 2,
    vul_sleeping_space <= 4 ~ 0
  ),
  susceptability_index5 = case_when(
    livelihood %in% c("business",
                      "unskilled",
                      "skilled",
                      "borrow") ~ 2,
    livelihood %in% c("formal",
                      "livestock",
                      "farm_crop") ~ 0,
    livelihood %in% c("no_consensus","other") ~ NA_real_
  ),
  susceptability_index6 = case_when(
    work_change %in% c("full_stop",
                       "part_stop") ~ 2,
    work_change == "no_stop" ~ 0
  ),
  adaptative_capacity_index3 = case_when(
    covid_safeprac.none == 1 ~ 3,
    covid_safeprac.none == 0 ~ 0
  )
) %>% mutate(
  vulnerability_score = rowSums(select(.,susceptability_index1:adaptative_capacity_index3),na.rm = T) / 36,
  vulnerability_classification = case_when(
    vulnerability_score <= 0.2 ~ "lower_risk",
    vulnerability_score <= 0.4 ~ "moderate_risk",
    vulnerability_score <= 0.6 ~ "moderate_high_risk",
    TRUE ~ "higher_risk"
  )
)

vulnerability_data_to_analyze <- vulnerability_data %>%
  select(region,province,district,tenure_sec_insec,access_toservice,shelter_condition,
         over_less_5years,mixed_sep_popn,return_status,location_type,
         site_type,vulnerability_classification)

analysis_vulnerability_data<- table_maker_prop(data = vulnerability_data_to_analyze,
                                               questionnaire_object = questionnaire,
                                               questionnaire = questions,
                                               choices = choices,
                                               weighting_function = NULL,
                                               labels = F,
                                               language = NULL,
                                               main_col_name = "overall",
                                               "region",
                                               "province",
                                               "district","tenure_sec_insec",
                                               "access_toservice",
                                               "shelter_condition",
                                               "over_less_5years",
                                               "mixed_sep_popn",
                                               "return_status",
                                               "location_type",
                                               "site_type")
analysis_vulnerability_data %>% write_csv("./output/vulnerability_calculation_regional_analysis.csv")
