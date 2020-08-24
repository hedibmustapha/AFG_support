#Load required packages
library(readxl)
library(readr)
library(dplyr)
library(composr)
# library(hypegrammaR)


#read dap
dap<-read_excel("./input/AFG_ISETs_siteprofiling_DAP_fordataunit_15.08.2020.xlsx",sheet = 3)
var_names<-dap$`Question Code`[which(!is.na(dap$`Question Code`))]

#read aggregated data
aggregated_data<-read_excel("./input/iset_aggregated_aug20-all-v7.xlsx")%>%type_convert()
# aggregated_data<-load_data("./input/data.csv")
names(aggregated_data) <- sub("[_]*","",names(aggregated_data))
aggregated_data<-aggregated_data %>%filter(consent=="yes")
aggregated_data$province[aggregated_data$province=="balkh"]<-"balkh_province"
aggregated_data$province[aggregated_data$province=="bamyan"]<-"bamyan_province"
aggregated_data$province[aggregated_data$province=="ghazni"]<-"ghazni_province"
aggregated_data$province[aggregated_data$province=="kabul"]<-"kabul_province"
aggregated_data$province[aggregated_data$province=="kandahar"]<-"kandahar_province"
aggregated_data$province[aggregated_data$province=="kunduz"]<-"kunduz_province"
aggregated_data$province[aggregated_data$province=="sar_e_pul"]<-"sar_e_pul_province"

dap_data<-select(aggregated_data,contains(var_names))

#read non-aggregated data
nonaggregated_data<-read_excel("./input/iset_clean_data_june2020.xlsx", sheet = "all")%>%type_convert()
names(nonaggregated_data) <- sub("[_]*","",names(nonaggregated_data))
nonaggregated_data<-nonaggregated_data%>% filter(consent=="yes")
nonaggregated_data<-nonaggregated_data %>%filter(consent=="yes")
nonaggregated_data$province[nonaggregated_data$province=="balkh"]<-"balkh_province"
nonaggregated_data$province[nonaggregated_data$province=="bamyan"]<-"bamyan_province"
nonaggregated_data$province[nonaggregated_data$province=="ghazni"]<-"ghazni_province"
nonaggregated_data$province[nonaggregated_data$province=="kabul"]<-"kabul_province"
nonaggregated_data$province[nonaggregated_data$province=="kandahar"]<-"kandahar_province"
nonaggregated_data$province[nonaggregated_data$province=="kunduz"]<-"kunduz_province"
nonaggregated_data$province[nonaggregated_data$province=="sar_e_pul"]<-"sar_e_pul_province"

#adding new indicators for the non-aggregated dataset
nonaggregated_data<-nonaggregated_data%>% mutate(
  ki_gender_age=paste0(ki_gender,"_",ki_age),
  location_fs=case_when(
    location == "rural" ~ "rural",
    location == "suburb" ~ "suburb",
    location %in% c("prov_centre","dist_centre","other_city") ~ "urban"
  ),
  region = case_when(
    province %in% c("bamyan",
                    "daykundi",
                    "kabul",
                    "kapisa",
                    "logar",
                    "maidan_wardak",
                    "panjsher",
                    "parwan") ~ "capital" ,
    province %in% c("kunar",
                    "laghman",
                    "nangarhar",
                    "nuristan") ~ "east",
    province %in% c("balkh",
                    "faryab",
                    "jawzjan",
                    "samangan",
                    "sar_e_pul") ~ "north",
    province %in% c("badakhshan",
                    "baghlan",
                    "kunduz",
                    "takhar") ~ "north_east",
    province %in% c("helmand",
                    "kandahar",
                    "nimroz",
                    "uruzgan",
                    "zabul") ~ "south",
    province %in% c("ghazni",
                    "khost",
                    "paktika",
                    "paktya") ~ "south_east",
    TRUE ~ "west"
    
  ),
  total_hhs = rowSums(select(.,idp_hhs,prolonged_hhs,protracted_hhs,refugee_hhs,returnee_hhs,economic_hhs),na.rm = T),
  total_hhs=ifelse(total_hhs==0,NA,total_hhs),
  tenure_sec_insec = case_when(
    tenure=="written" ~ "secure_tenure",
    tenure %in% c("verbal","none","prefer_not") ~ "insecure_tenure"
  ),
  services_proxy = ifelse(water_distance %in% c("no_further","no_none") | health_distance=="no_none","yes","no"),
  access_toservice = case_when(
    services_proxy == "yes" ~ "service_access",
    services_proxy == "no" ~ "service_inaccess"
  ),
  shelter_condition = case_when(
    shelter_type %in% c("tent","makeshift","collective","open_space","unfinished","damaged") ~ "inadequate_shelter",
    shelter_type %in% c("transitional","mud","brick") ~ "adequate_shelter"
  ),
  over_less_5years = case_when(
    five_years == "yes" ~ "over_5years",
    five_years %in% c("no","dont_know") ~ "less_5years"
  ),
  mixed_sep_popn = case_when(
    iset_population == "mixed" ~ "mixed_popn",
    iset_population == "discrete" ~ "seperate_popn"
  ),
  return_status = case_when(
    returnee_3mo_hhs >0 ~ "recent_return",
    returnee_3mo_hhs == 0 ~ "no_recent_return"
  ),
  location_type = case_when(
    location_fs == "urban" ~ "urban_location",
    location_fs == "suburb" ~ "suburb_location",
    location_fs == "rural" ~ "rural_location"
  ),
  site_type = case_when(
    total_hhs <= 750 ~ "small_site",
    total_hhs > 750 ~ "large_site"
  )
)

#adding new indicators for aggregated dataset
aggregated_data<-aggregated_data %>% mutate(
  location_fs=case_when(
    location == "rural" ~ "rural",
    location == "suburb" ~ "suburb",
    location %in% c("prov_centre","dist_centre","other_city") ~ "urban"
  ),
  idp_3mo_hhs_prop=case_when(
    idp_3mo_hhs>0 ~ 1,
    is.na(idp_3mo_hhs) | idp_3mo_hhs==0 ~ 0
  ),
  idp_mixed=case_when(
    iset_population =="mixed" & migrant_population.idp==1 ~ 1,
    migrant_population.idp==0 ~ 0
  ),
  idp_discrete=case_when(
    iset_population =="discrete" & migrant_population.idp==1 ~ 1,
    migrant_population.idp==0 ~ 0
  ),
  prolong_mixed=case_when(
    iset_population =="mixed" & migrant_population.prolong==1 ~ 1,
    migrant_population.prolong==0 ~ 0
  ),
  prolong_discrete=case_when(
    iset_population =="discrete" & migrant_population.prolong==1 ~ 1,
    migrant_population.prolong==0 ~ 0
  ),
  protract_mixed=case_when(
    iset_population =="mixed" & migrant_population.protract==1 ~ 1,
    migrant_population.protract==0 ~ 0
  ),
  protract_discrete=case_when(
    iset_population =="discrete" & migrant_population.protract==1 ~ 1,
    migrant_population.protract==0 ~ 0
  ),
  refugee_mixed=case_when(
    iset_population =="mixed" & migrant_population.refugee==1 ~ 1,
    migrant_population.refugee==0 ~ 0
  ),
  refugee_discrete=case_when(
    iset_population =="discrete" & migrant_population.refugee==1 ~ 1,
    migrant_population.refugee==0 ~ 0
  ),
  returnee_mixed=case_when(
    iset_population =="mixed" & migrant_population.returnee==1 ~ 1,
    migrant_population.returnee==0  ~ 0
  ),
  returnee_discrete=case_when(
    iset_population =="discrete" & migrant_population.returnee==1 ~ 1,
    migrant_population.returnee==0 ~ 0
  ),
  economic_mixed=case_when(
    iset_population =="mixed" & migrant_population.economic==1 ~ 1,
    migrant_population.economic==0  ~ 0
  ),
  economic_discrete=case_when(
    iset_population =="discrete" & migrant_population.economic==1 ~ 1,
    migrant_population.economic==0 ~ 0
  ),
  nomad_mixed=case_when(
    iset_population =="mixed" & migrant_population.nomad==1 ~ 1,
    migrant_population.nomad==0  ~ 0
  ),
  nomad_discrete=case_when(
    iset_population =="discrete" & migrant_population.nomad==1 ~ 1,
    migrant_population.nomad==0 ~ 0
  ),
  caseload_hhs = rowSums(select(.,idp_hhs,prolonged_hhs,protracted_hhs,refugee_hhs,returnee_hhs),na.rm = T),
  caseload_hhs=ifelse(caseload_hhs==0,NA,caseload_hhs),
  caseload_indvl = rowSums(select(.,idp_indvl,prolonged_indvl,protracted_indvl,refugee_indvl,returnee_indvl),na.rm = T),
  caseload_indvl=ifelse(caseload_indvl==0,NA,caseload_indvl),
  returnee_3mo_hhs_prop = case_when(
    returnee_3mo_hhs>0 ~ 1,
    is.na(returnee_3mo_hhs) |  returnee_3mo_hhs==0 ~ 0
  ),
  total_hhs = rowSums(select(.,idp_hhs,prolonged_hhs,protracted_hhs,refugee_hhs,returnee_hhs,economic_hhs),na.rm = T),
  total_hhs=ifelse(total_hhs==0,NA,total_hhs),
  total_indvl = rowSums(select(.,idp_indvl,prolonged_indvl,protracted_indvl,refugee_indvl,returnee_indvl,economic_indvl),na.rm = T),
  total_indvl=ifelse(total_indvl==0,NA,total_indvl),
  hh_size = total_indvl / total_hhs,
  caseload_prop = caseload_hhs/total_hhs,
  prop_disability = disability / total_hhs,
  vul_chronic = chronic_ill / total_hhs,
  vul_chronic_fs = ifelse(vul_chronic>0.05,1,0),
  vul_elderly = elderly / total_indvl,
  vul_chronic_fs = ifelse(vul_elderly>0.05,1,0),
  prop_female_hoh = female_hoh / total_hhs,
  using_public_facility = ifelse(sm_selected(handwash_location,any = c("public","public_facility")),1,0),
  water_distance_fs= case_when(
    water_distance %in% c("no_further","no_none") ~ "yes",
    water_distance =="yes" ~ "no",
    water_distance =="no_consensus" ~ "NC"
  ),
  water_source_fs = ifelse(water_source %in% c("well_unprotect","surface"),1,0),
  health_distance_fs=case_when(
    health_distance %in% c("no_further","no_none") ~ "yes",
    health_distance =="yes" ~ "no",
    health_distance =="no_consensus" ~ "NC"
  ),
  services_proxy = ifelse(water_distance %in% c("no_further","no_none") | health_distance=="no_none","yes","no"),
  health_satisfy_constraint=ifelse(health_use_recent!="yes",NA,health_use_satisfy),
  health_unsatisfy_constraint=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy),
  health_unsatisfy_constraint.hours=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.hours),
  health_unsatisfy_constraint.expensive=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.expensive),
  health_unsatisfy_constraint.drug_unavail=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.drug_unavail),
  health_unsatisfy_constraint.staff_behave=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.staff_behave),
  health_unsatisfy_constraint.crowded=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.crowded),
  health_unsatisfy_constraint.other=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.other),
  health_unsatisfy_constraint.dont_know=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.dont_know),
  health_unsatisfy_constraint.prefer_not=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.prefer_not),
  tenure_insecure = case_when(
    tenure %in% c("verbal","none","dont_know","prefer_not") ~ "yes",
    tenure == "written" ~ "no",
    tenure == "no_consensus" ~ "NC"
  ),
  access_school= case_when(
    school_distance %in% c("no_further","yes") ~ "yes",
    school_distance == "no_one" ~ "no",
    school_distance == "no_consensus" ~ "NC"
  ),
  hh_rooms_space_isolate= hh_rooms-1,
  hh_rooms_space_isolate=ifelse(hh_rooms_space_isolate==0,NA,hh_rooms_space_isolate),
  vul_sleeping_space = hh_size / (hh_rooms_space_isolate),
  vul_sleeping_prop = ifelse(vul_sleeping_space>=4,1,0),
  region = case_when(
    province %in% c("bamyan",
                    "daykundi",
                    "kabul",
                    "kapisa",
                    "logar",
                    "maidan_wardak",
                    "panjsher",
                    "parwan") ~ "capital" ,
    province %in% c("kunar",
                    "laghman",
                    "nangarhar",
                    "nuristan") ~ "east",
    province %in% c("balkh",
                    "faryab",
                    "jawzjan",
                    "samangan",
                    "sar_e_pul") ~ "north",
    province %in% c("badakhshan",
                    "baghlan",
                    "kunduz",
                    "takhar") ~ "north_east",
    province %in% c("helmand",
                    "kandahar",
                    "nimroz",
                    "uruzgan",
                    "zabul") ~ "south",
    province %in% c("ghazni",
                    "khost",
                    "paktika",
                    "paktya") ~ "south_east",
    TRUE ~ "west"
    
  ),
  tenure_sec_insec = case_when(
    tenure=="written" ~ "secure_tenure",
    tenure %in% c("verbal","none","prefer_not") ~ "insecure_tenure"
  ),
  access_toservice = case_when(
    services_proxy == "yes" ~ "service_access",
    services_proxy == "no" ~ "service_inaccess"
  ),
  shelter_condition = case_when(
    shelter_type %in% c("tent","makeshift","collective","open_space","unfinished","damaged") ~ "inadequate_shelter",
    shelter_type %in% c("transitional","mud","brick") ~ "adequate_shelter"
  ),
  over_less_5years = case_when(
    five_years == "yes" ~ "over_5years",
    five_years %in% c("no","dont_know") ~ "less_5years"
  ),
  mixed_sep_popn = case_when(
    iset_population == "mixed" ~ "mixed_popn",
    iset_population == "discrete" ~ "seperate_popn"
  ),
  return_status = case_when(
    returnee_3mo_hhs >0 ~ "recent_return",
    returnee_3mo_hhs == 0 ~ "no_recent_return"
  ),
  location_type = case_when(
    location_fs == "urban" ~ "urban_location",
    location_fs == "suburb" ~ "suburb_location",
    location_fs == "rural" ~ "rural_location"
  ),
  site_type = case_when(
    total_hhs <= 750 ~ "small_site",
    total_hhs > 750 ~ "large_site"
  )
                                  
)

#adding variables to the dap_data
dap_data<-dap_data %>% mutate(
  location_fs=case_when(
    location == "rural" ~ "rural",
    location == "suburb" ~ "suburb",
    location %in% c("prov_centre","dist_centre","other_city") ~ "urban"
  ),
  idp_3mo_hhs_prop=case_when(
    idp_3mo_hhs>0 ~ 1,
    is.na(idp_3mo_hhs) | idp_3mo_hhs==0 ~ 0
  ),
  idp_mixed=case_when(
    iset_population =="mixed" & migrant_population.idp==1 ~ 1,
    migrant_population.idp==0 ~ 0
  ),
  idp_discrete=case_when(
    iset_population =="discrete" & migrant_population.idp==1 ~ 1,
    migrant_population.idp==0 ~ 0
  ),
  prolong_mixed=case_when(
    iset_population =="mixed" & migrant_population.prolong==1 ~ 1,
    migrant_population.prolong==0 ~ 0
  ),
  prolong_discrete=case_when(
    iset_population =="discrete" & migrant_population.prolong==1 ~ 1,
    migrant_population.prolong==0 ~ 0
  ),
  protract_mixed=case_when(
    iset_population =="mixed" & migrant_population.protract==1 ~ 1,
    migrant_population.protract==0 ~ 0
  ),
  protract_discrete=case_when(
    iset_population =="discrete" & migrant_population.protract==1 ~ 1,
    migrant_population.protract==0 ~ 0
  ),
  refugee_mixed=case_when(
    iset_population =="mixed" & migrant_population.refugee==1 ~ 1,
    migrant_population.refugee==0 ~ 0
  ),
  refugee_discrete=case_when(
    iset_population =="discrete" & migrant_population.refugee==1 ~ 1,
    migrant_population.refugee==0 ~ 0
  ),
  returnee_mixed=case_when(
    iset_population =="mixed" & migrant_population.returnee==1 ~ 1,
    migrant_population.returnee==0  ~ 0
  ),
  returnee_discrete=case_when(
    iset_population =="discrete" & migrant_population.returnee==1 ~ 1,
    migrant_population.returnee==0 ~ 0
  ),
  economic_mixed=case_when(
    iset_population =="mixed" & migrant_population.economic==1 ~ 1,
    migrant_population.economic==0  ~ 0
  ),
  economic_discrete=case_when(
    iset_population =="discrete" & migrant_population.economic==1 ~ 1,
    migrant_population.economic==0 ~ 0
  ),
  nomad_mixed=case_when(
    iset_population =="mixed" & migrant_population.nomad==1 ~ 1,
    migrant_population.nomad==0  ~ 0
  ),
  nomad_discrete=case_when(
    iset_population =="discrete" & migrant_population.nomad==1 ~ 1,
    migrant_population.nomad==0 ~ 0
  ),
  caseload_hhs = rowSums(select(.,idp_hhs,prolonged_hhs,protracted_hhs,refugee_hhs,returnee_hhs),na.rm = T),
  caseload_hhs=ifelse(caseload_hhs==0,NA,caseload_hhs),
  caseload_indvl = rowSums(select(.,idp_indvl,prolonged_indvl,protracted_indvl,refugee_indvl,returnee_indvl),na.rm = T),
  caseload_indvl=ifelse(caseload_indvl==0,NA,caseload_indvl),
  returnee_3mo_hhs_prop = case_when(
    returnee_3mo_hhs>0 ~ 1,
    is.na(returnee_3mo_hhs) |  returnee_3mo_hhs==0 ~ 0
  ),
  total_hhs = rowSums(select(.,idp_hhs,prolonged_hhs,protracted_hhs,refugee_hhs,returnee_hhs,economic_hhs),na.rm = T),
  total_hhs=ifelse(total_hhs==0,NA,total_hhs),
  total_indvl = rowSums(select(.,idp_indvl,prolonged_indvl,protracted_indvl,refugee_indvl,returnee_indvl,economic_indvl),na.rm = T),
  total_indvl=ifelse(total_indvl==0,NA,total_indvl),
  hh_size = total_indvl / total_hhs,
  caseload_prop = caseload_hhs/total_hhs,
  prop_disability = disability / total_hhs,
  vul_chronic = chronic_ill / total_hhs,
  vul_chronic_fs = ifelse(vul_chronic>0.05,1,0),
  vul_elderly = elderly / total_indvl,
  vul_chronic_fs = ifelse(vul_elderly>0.05,1,0),
  prop_female_hoh = female_hoh / total_hhs,
  using_public_facility = ifelse(sm_selected(handwash_location,any = c("public","public_facility")),1,0),
  water_distance_fs= case_when(
    water_distance %in% c("no_further","no_none") ~ "yes",
    water_distance =="yes" ~ "no",
    water_distance =="no_consensus" ~ "NC"
  ),
  water_source_fs = ifelse(water_source %in% c("well_unprotect","surface"),1,0),
  health_distance_fs=case_when(
    health_distance %in% c("no_further","no_none") ~ "yes",
    health_distance =="yes" ~ "no",
    health_distance =="no_consensus" ~ "NC"
  ),
  services_proxy = ifelse(water_distance %in% c("no_further","no_none") | health_distance=="no_none","yes","no"),
  health_satisfy_constraint=ifelse(health_use_recent!="yes",NA,health_use_satisfy),
  health_unsatisfy_constraint=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy),
  health_unsatisfy_constraint.hours=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.hours),
  health_unsatisfy_constraint.expensive=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.expensive),
  health_unsatisfy_constraint.drug_unavail=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.drug_unavail),
  health_unsatisfy_constraint.staff_behave=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.staff_behave),
  health_unsatisfy_constraint.crowded=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.crowded),
  health_unsatisfy_constraint.other=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.other),
  health_unsatisfy_constraint.dont_know=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.dont_know),
  health_unsatisfy_constraint.prefer_not=ifelse(health_use_recent!="yes",NA,health_use_unsatisfy.prefer_not),
  tenure_insecure = case_when(
    tenure %in% c("verbal","none","dont_know","prefer_not") ~ "yes",
    tenure == "written" ~ "no",
    tenure == "no_consensus" ~ "NC"
  ),
  access_school= case_when(
    school_distance %in% c("no_further","yes") ~ "yes",
    school_distance == "no_one" ~ "no",
    school_distance == "no_consensus" ~ "NC"
  ),
  hh_rooms_space_isolate= hh_rooms-1,
  hh_rooms_space_isolate=ifelse(hh_rooms_space_isolate==0,NA,hh_rooms_space_isolate),
  vul_sleeping_space = hh_size / (hh_rooms_space_isolate),
  vul_sleeping_prop = ifelse(vul_sleeping_space>=4,1,0),
  region = case_when(
    province %in% c("bamyan",
                    "daykundi",
                    "kabul",
                    "kapisa",
                    "logar",
                    "maidan_wardak",
                    "panjsher",
                    "parwan") ~ "capital" ,
    province %in% c("kunar",
                    "laghman",
                    "nangarhar",
                    "nuristan") ~ "east",
    province %in% c("balkh",
                    "faryab",
                    "jawzjan",
                    "samangan",
                    "sar_e_pul") ~ "north",
    province %in% c("badakhshan",
                    "baghlan",
                    "kunduz",
                    "takhar") ~ "north_east",
    province %in% c("helmand",
                    "kandahar",
                    "nimroz",
                    "uruzgan",
                    "zabul") ~ "south",
    province %in% c("ghazni",
                    "khost",
                    "paktika",
                    "paktya") ~ "south_east",
    TRUE ~ "west"
    
  ),
  tenure_sec_insec = case_when(
    tenure=="written" ~ "secure_tenure",
    tenure %in% c("verbal","none","prefer_not") ~ "insecure_tenure"
  ),
  access_toservice = case_when(
    services_proxy == "yes" ~ "service_access",
    services_proxy == "no" ~ "service_inaccess"
  ),
  shelter_condition = case_when(
    shelter_type %in% c("tent","makeshift","collective","open_space","unfinished","damaged") ~ "inadequate_shelter",
    shelter_type %in% c("transitional","mud","brick") ~ "adequate_shelter"
  ),
  over_less_5years = case_when(
    five_years == "yes" ~ "over_5years",
    five_years %in% c("no","dont_know") ~ "less_5years"
  ),
  mixed_sep_popn = case_when(
    iset_population == "mixed" ~ "mixed_popn",
    iset_population == "discrete" ~ "seperate_popn"
  ),
  return_status = case_when(
    returnee_3mo_hhs >0 ~ "recent_return",
    returnee_3mo_hhs == 0 ~ "no_recent_return"
  ),
  location_type = case_when(
    location_fs == "urban" ~ "urban_location",
    location_fs == "suburb" ~ "suburb_location",
    location_fs == "rural" ~ "rural_location"
  ),
  site_type = case_when(
    total_hhs <= 750 ~ "small_site",
    total_hhs > 750 ~ "large_site"
  )
  
)

#names of the variable to which we will apply sum rather than proportion/average
var_tosum<-c("idp_hhs",
             "idp_indvl",
             "prolonged_hhs",
             "prolonged_indvl",
             "protracted_hhs",
             "protracted_indvl",
             "idp_3mo_hhs",
             "refugee_hhs",
             "refugee_indvl",
             "returnee_hhs",
             "returnee_indvl",
             "returnee_3mo_hhs",
             "economic_hhs",
             "economic_indvl",
             "nomad_hhs",
             "nomad_indvl",
             "host_hhs",
             "host_indvl",
             "disability",
             "chronic_ill",
             "elderly",
             "female_hoh",
             "water_point_no",
             "caseload_hhs",
             "caseload_indvl",
             "total_hhs",
             "total_indvl")
