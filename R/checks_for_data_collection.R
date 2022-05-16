# load packages

library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")
# read data

df_tool_data_an <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx") %>% 
 mutate(z.uuid = `_uuid`, 
        z.start_date = as_date(start),
        z.enumerator_id = enumerator_id,
        z.point_number = point_number,
        start = as_datetime(start),
        end = as_datetime(end)) %>% 
  filter(consent == "yes", z.start_date > as_date("21-08-29"))

df_survey_an <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "survey")
df_choices_an <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "choices")

df_sample_data_an <- sf::st_read("inputs/dfa_settlement_host_samples.gpkg", quiet = TRUE)        


# output holder ------------------------------------------------------------

logic_output <- list()   


# data not meeting minimum requirements -----------------------------------

# no_consent_not_hoh
df_no_consent_not_hoh <- df_tool_data_an %>% 
  filter(hoh == "no") %>% 
  mutate(z.type = "remove_survey",
         z.name = "hoh",
         z.current_value = as.character(hoh),
         z.value = "",
         z.issue_id = "logic_m_requirement_no_consent_not_hoh",
         z.issue = "no_consent_not_hoh",
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "",
         z.reviewed = "1",
         z.adjust_log = "",
         z.uuid_cl = "",
         z.so_sm_choices = "") %>% 
  dplyr::select(starts_with("z.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_consent_not_hoh")

# below age
df_respondents_not_of_age <- df_tool_data_an %>% 
  filter(respondent_age < 18) %>% 
  mutate(z.type = "remove_survey",
         z.name = "respondent_age",
         z.current_value = as.character(respondent_age),
         z.value = "",
         z.issue_id = "logic_m_requirement_below_age",
         z.issue = "below_age",
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "", 
         z.reviewed = "1",
         z.adjust_log = "",
         z.uuid_cl = "",
         z.so_sm_choices = "") %>% 
  dplyr::select(starts_with("z.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_respondents_not_of_age")


# Time checks -------------------------------------------------------------

# Time interval for the survey

min_time_of_survey <- 25
max_time_of_survey <- 120

df_c_survey_time_an <- df_tool_data_an %>% 
  mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
         int.survey_time_interval = ceiling(int.survey_time_interval),
         z.type = "remove_survey",
         z.name = "point_number",
         z.current_value = "",
         z.value = "",
         z.issue_id = case_when(
           int.survey_time_interval < min_time_of_survey ~ "less_survey_time",
           int.survey_time_interval > max_time_of_survey ~ "more_survey_time",
           TRUE ~ "normal_survey_time"),
         z.issue = glue("{int.survey_time_interval} min taken to do the survey"),
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "",
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = paste0(z.uuid, "_", z.type, "_", z.name),
         z.so_sm_choices = "") %>% 
  filter(z.issue_id %in% c("less_survey_time", "more_survey_time"))

if(exists("df_c_survey_time_an")){
  if(nrow(df_c_survey_time_an) > 0){
    logic_output$df_c_survey_time_an <- df_c_survey_time_an
  }
}

# check the time between surveys

min_time_btn_surveys <- 5

df_c_survey_time_an <- df_tool_data_an %>% 
  group_by(z.start_date, z.enumerator_id) %>% 
  filter(n()>1) %>% 
  arrange(start, .by_group = TRUE) %>% 
  mutate(int.time_between_survey = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
         int.time_between_survey = ceiling(int.time_between_survey)) %>% 
  filter(int.time_between_survey != 0 & int.time_between_survey < min_time_btn_surveys) %>% 
  mutate(z.type = "remove_survey",
         z.name = "point_number",
         z.current_value = "",
         z.value = "",
         z.issue_id = "less_time_btn_surveys",
         z.issue = glue("{int.time_between_survey} min taken between surveys"),
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "",
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = paste0(z.uuid, "_", z.type, "_", z.name),
         z.so_sm_choices = "") %>% 
  dplyr::select(starts_with("z.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))
              
if(exists("df_c_time_btn_surveys_an")){
  if(nrow(df_c_survey_time_an) > 0){
    logic_output$df_c_time_btn_survey_an <- df_c_time_btn_survey_an
  }
}
  
         

# Logical checks ----------------------------------------------------------

# Anyone who selected "ugandan" and previously answered community_type = refugee, should be checked
df_c_nationality_an <- df_tool_data_an %>% 
  filter(status == "refugee", nationality == "ugandan") %>% 
  mutate(z.type = "change_response",
         z.name = "nationality",
         z.current_value = nationality,
         z.value = "",
         z.issue_id = "logic_c_nationality",
         z.issue = "nationality: ugandan but community_type: refugee",
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "",
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = paste0(z.uuid, "_", z.type, "_", z.name),
         z.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i")) %>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

if(exists("df_c_nationality_an")){
  if(nrow(df_c_nationality_an) > 0){
    logic_output$df_c_nationality_an <- df_c_nationality_an
    
    
  }
  
}        
        
# Anyone who selected host for "type of community" and answers "refugee ID" or "beneficiary ID" should be checked.
df_c_id_type_an <- df_tool_data_an %>% 
  filter(status == "host_community", str_detect(string = id_type, pattern = "unhcr_refugee_id|ug_refugee_id|benef_id_not_unhcr")) %>% 
  mutate(z.type = "change_response",
         z.name = "id_type",
         z.current_value = id_type,
         z.value = "",
         z.issue_id = "logic_c_status",
         z.issue = glue("status: host_community but refugee id_type: {id_type}"),
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "", 
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = paste0(z.uuid, "_", z.type, "_", z.name),
         z.so_sm_choices = "") %>% 
  dplyr::select(starts_with("z."))%>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

if(exists("df_c_id_type_an")){
  if(nrow(df_c_id_type_an) > 0){
    logic_output$df_c_id_type_an <- df_c_id_type_an
  }
}

# If respondents have selected a language but have NOT selected the same language that they previously selected for their main language, we need to check the survye.
df_c_language_an <- df_tool_data_an %>% 
  mutate(z.check.type = "change_response",
         z.check.name = "main_language",
         z.check.current_value = main_language,
         z.check.value = "",
         z.check.issue_id = ifelse(str_detect(string = language_understand, pattern = main_language, negate = TRUE) , 
                                   "logic_c_main_language", "main_language_also_understood"),
         z.check.issue = glue("main_language: {main_language} not in understood languages: {language_understand}"),
         z.check.other_text = "",
         z.check.checked_by = "",
         z.check.checked_date = as_date(today()),
         z.check.comment = "", 
         z.check.reviewed = "",
         z.check.adjust_log = "",
         z.check.uuid_cl = "",
         z.check.so_sm_choices = "") %>% 
  filter(z.check.issue_id == "logic_c_main_language") %>% 
  dplyr::select(starts_with("z."))%>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

if(exists("df_c_language_an")){
  if(nrow(df_c_language_an) > 0){
    logic_output$df_c_language_an <- df_c_language_an
  }
}

# If respondent has selected "none" in addition to another option, the survey needs to be checked.
# type_phone_owned

df_c_type_phone_owned_an <- df_tool_data_an %>% 
  rowwise() %>% 
  mutate(int.type_phone_owned_count = sum(c_across(starts_with("type_phone_owned/")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(z.type = "remove_option",
         z.name = "type_phone_owned",
         z.current_value = "none",
         z.value = "none",
         z.issue_id = ifelse(int.type_phone_owned_count > 1 & `type_phone_owned/none` == 1, "logic_c_type_phone_owned", "expected_response"),
         z.issue = glue("none option selected with other options: {type_phone_owned}"),
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "", 
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = "",
         z.so_sm_choices = "") %>% 
  filter(z.issue_id == "logic_c_type_phone_owned") %>% 
  dplyr::select(starts_with("z."))%>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

if(exists("df_c_type_phone_owned_an")){
  if(nrow(df_c_type_phone_owned_an) > 0){
    logic_output$df_c_type_phone_owned_an <- df_c_type_phone_owned_an
  }
}
# If they previously selected "yes" to having mobile internet coverage (Q56) and now replied "no", the survey needs to be checked.
# mobile_internet == "yes" and internet_awareness == "no"

df_c_internet_awareness_an <- df_tool_data_an %>% 
  filter(mobile_internet == "yes", internet_awareness == "no") %>% 
  mutate(z.type = "change_response",
         z.name = "internet_awareness",
         z.current_value = internet_awareness,
         z.value = NA,
         z.issue_id = "logic_c_internet_awareness",
         z.issue = "mobile_internet: yes but internet_awareness: no",
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "", 
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = "",
         z.so_sm_choices = "") %>% 
  dplyr::select(starts_with("z."))%>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

if(exists("df_c_internet_awareness_an")){
  if(nrow(df_c_internet_awareness_an) > 0){
    logic_output$df_c_internet_awareness_an <- df_c_internet_awareness_an
  }
}  
# If in previous qn "why do you want to have  a mobile money account?" they answered "it is safer than keeping cash at home" and they now asnwered "the system is not safe i am concerned that my money will disappear", survey needs to be checked
# reason_want_mm_acc/safer_than_home == 1 and reason_not_open_mm_acc/unsafe_system
df_c_reason_not_open_mm_acc_an <- df_tool_data_an %>% 
  filter(`reason_want_mm_acc/safer_than_home` == 1, `reason_not_open_mm_acc/unsafe_system` == 1) %>% 
  mutate(z.type = "remove_option",
         z.name = "reason_not_open_mm_acc",
         z.current_value = "unsafe_system",
         z.value = "unsafe_system",
         z.issue_id = "logic_c_reason_not_open_mm_acc",
         z.issue = "reason_want_mm_acc: safer_than_home but reason_not_open_mm_acc: unsafe_system",
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "", 
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = "",
         z.so_sm_choices = "") %>% 
  dplyr::select(starts_with("z."))%>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

if(exists("df_c_reason_not_open_mm_acc_an")){
  if(nrow(df_c_reason_not_open_mm_acc_an) > 0){
    logic_output$df_c_reason_not_open_mm_acc_an <- df_c_reason_not_open_mm_acc_an
  }
}
# if in previous question 'Why do you want to have a pre-paid or smart card?' answered "it will allow me to securely store my money" and they now chose "the system is not safe i am concerned that my money will disappear", check survey
# reason_want_card/safe_storage and reason_not_want_card/unsafe_system
df_c_reason_not_want_card_an <- df_tool_data_an %>% 
  filter(`reason_want_card/safe_storage` == 1, `reason_not_want_card/unsafe_system` == 1) %>% 
  mutate(z.type = "remove_option",
         z.name = "reason_not_want_card",
         z.current_value = "unsafe_system",
         z.value = "unsafe_system",
         z.issue_id = "logic_c_reason_not_want_card",
         z.issue = "reason_want_card: safer_than_home but reason_not_want_card: unsafe_system",
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "", 
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = "",
         z.so_sm_choices = "") %>% 
  dplyr::select(starts_with("z."))%>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

if(exists("df_c_reason_not_want_card_an")){
  if(nrow(df_c_reason_not_want_card_an) > 0){
    logic_output$df_c_reason_not_want_card_an <- df_c_reason_not_want_card_an
  }
}  
# 
# spatial checks ----------------------------------------------------------

sample_pt_nos_an <- df_sample_data_an %>% 
  mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
  pull(unique_pt_number) %>% 
  unique()

# duplicate point numbers
df_c_duplicate_pt_nos_an <- df_tool_data_an %>% 
  mutate(unique_pt_number = paste0(status, "_", point_number )) %>% 
  group_by(z.district_name, status, z.point_number) %>% 
  filter(n() > 1, unique_pt_number %in% sample_pt_nos_an) %>% 
  mutate(z.type = "change_response",
         z.name = "point_number",
         z.current_value = point_number,
         z.value = "",
         z.issue_id = "spatial_c_duplicate_pt_no",
         z.issue = glue("point_number: {point_number} is duplicated: check that its not a repeated survey"),
         z.other_text = "",
         z.checked_by = "",
         z.checked_date = as_date(today()),
         z.comment = "", 
         z.reviewed = "",
         z.adjust_log = "",
         z.uuid_cl = "",
         z.so_sm_choices = "") %>% 
  ungroup() %>%
  dplyr::select(starts_with("z."))%>% 
  rename_with(~str_replace(string = .x, pattern = "z.", replacement = ""))

if(exists("df_c_duplicate_pt_nos_an")){
  if(nrow(df_c_duplicate_pt_nos_an) > 0){
    logic_output$df_c_duplicate_pt_nos_an <- df_c_duplicate_pt_nos_an
  }
}





  