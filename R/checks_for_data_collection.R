# load packages

library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")
# read data

df_tool_data_an <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx") %>% 
 mutate(i.uuid = `_uuid`, 
        i.start_date = as_date(start),
        i.enumerator_id = enumerator_id,
        i.point_number = point_number,
        start = as_datetime(start),
        end = as_datetime(end)) %>% 
  filter(consent == "yes", i.start_date > as_date("21-08-29"))

df_survey_an <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "survey")
df_choices_an <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "choices")

df_sample_data_an <- sf::st_read("inputs/dfa_settlement_host_samples.gpkg", quiet = TRUE)        


# output holder ------------------------------------------------------------

logic_output <- list()   


# data not meeting minimum requirements -----------------------------------

# no_consent_not_hoh
df_no_consent_not_hoh <- df_tool_data_an %>% 
  filter(hoh == "no") %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "hoh",
         i.check.current_value = as.character(hoh),
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_no_consent_not_hoh",
         i.check.issue = "no_consent_not_hoh",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_consent_not_hoh")

# below age
df_respondents_not_of_age <- df_tool_data_an %>% 
  filter(respondent_age < 18) %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "respondent_age",
         i.check.current_value = as.character(respondent_age),
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_below_age",
         i.check.issue = "below_age",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_respondents_not_of_age")


# Time checks -------------------------------------------------------------

# Time interval for the survey

min_time_of_survey <- 25
max_time_of_survey <- 120

df_c_survey_time_an <- df_tool_data_an %>% 
  mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
         int.survey_time_interval = ceiling(int.survey_time_interval),
         i.type = "remove_survey",
         i.name = "point_number",
         i.current_value = "",
         i.value = "",
         i.issue_id = case_when(
           int.survey_time_interval < min_time_of_survey ~ "less_survey_time",
           int.survey_time_interval > max_time_of_survey ~ "more_survey_time",
           TRUE ~ "normal_survey_time"),
         i.issue = glue("{int.survey_time_interval} min taken to do the survey"),
         i.other_text = "",
         i.checked_by = "",
         i.checked_date = as_date(today()),
         i.comment = "",
         i.reviewed = "",
         i.adjust_log = "",
         i.uuid_cl = paste0(i.uuid, "_", i.type, "_", i.name),
         i.so_sm_choices = "") %>% 
  filter(i.issue_id %in% c("less_survey_time", "more_survey_time"))

if(exists("df_c_survey_time_an")){
  if(nrow(df_c_survey_time_an) > 0){
    logic_output$df_c_survey_time_an <- df_c_survey_time_an
  }
}

# check the time between surveys

min_time_btn_surveys <- 5

df_c_survey_time_an <- df_tool_data_an %>% 
  group_by(i.start_date, i.enumerator_id) %>% 
  filter(n()>1) %>% 
  arrange(start, .by_group = TRUE) %>% 
  mutate(int.time_between_survey = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
         int.time_between_survey = ceiling(int.time_between_survey)) %>% 
  filter(int.time_between_survey != 0 & int.time_between_survey < min_time_btn_surveys) %>% 
  mutate(i.type = "remove_survey",
         i.name = "point_number",
         i.current_value = "",
         i.value = "",
         i.issue_id = "less_time_btn_surveys",
         i.issue = glue("{int.time_between_survey} min taken between surveys"),
         i.other_text = "",
         i.checked_by = "",
         i.checked_date = as_date(today()),
         i.comment = "",
         i.reviewed = "",
         i.adjust_log = "",
         i.uuid_cl = paste0(i.uuid, "_", i.type, "_", i.name),
         i.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i", replacement = ""))
              
if(exists("df_c_time_btn_surveys_an")){
  if(nrow(df_c_survey_time_an) > 0){
    logic_output$df_c_time_btn_survey_an <- df_c_time_btn_survey_an
  }
}
  
         

# Logical checks ----------------------------------------------------------

# Anyone who selected "ugandan" and previously answered community_type = refugee, should be checked
df_c_nationality_an <- df_tool_data_an %>% 
  filter(status == "refugee", nationality == "ugandan") %>% 
  mutate(i.type = "change_response",
         i.name = "nationality",
         i.current_value = nationality,
         i.value = "",
         i.issue_id = "logic_c_nationality",
         i.issue = "nationality: ugandan but community_type: refugee",
         i.other_text = "",
         i.checked_by = "",
         i.checked_date = as_date(today()),
         i.comment = "",
         i.reviewed = "",
         i.adjust_log = "",
         i.uuid_cl = paste0(i.uuid, "_", i.type, "_", i.name),
         i.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i", replacement = ""))

if(exists("df_c_nationality_an")){
  if(nrow(df_c_nationality_an) > 0){
    logic_output$df_c_nationality_an <- df_c_nationality_an
    
    
  }
  
}        
        
# Anyone who selected host for "type of community" and answers "refugee ID" or "beneficiary ID" should be checked.
df_c_id_type_an <- df_tool_data_an %>% 
  filter(status == "host_community", str_detect(string = id_type, pattern = "unhcr_refugee_id|ug_refugee_id|benef_id_not_unhcr")) %>% 
  mutate(i.type = "change_response",
         i.name = "id_type",
         i.current_value = id_type,
         i.value = "",
         i.issue_id = "logic_c_status",
         i.issue = glue("status: host_community but refugee id_type: {id_type}"),
         i.other_text = "",
         i.checked_by = "",
         i.checked_date = as_date(today()),
         i.comment = "", 
         i.reviewed = "",
         i.adjust_log = "",
         i.uuid_cl = paste0(i.uuid, "_", i.type, "_", i.name),
         i.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i", replacement = ""))

if(exists("df_c_id_type_an")){
  if(nrow(df_c_id_type_an) > 0){
    logic_output$df_c_id_type_an <- df_c_id_type_an
  }
}

# If respondents have selected a language but have NOT selected the same language that they previously selected for their main language, we need to check the survye.
df_c_language_an <- df_tool_data_an %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "main_language",
         i.check.current_value = main_language,
         i.check.value = "",
         i.check.issue_id = ifelse(str_detect(string = language_understand, pattern = main_language, negate = TRUE) , 
                                   "logic_c_main_language", "main_language_also_understood"),
         i.check.issue = glue("main_language: {main_language} not in understood languages: {language_understand}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  filter(i.check.issue_id == "logic_c_main_language") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

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
  mutate(i.check.type = "remove_option",
         i.check.name = "type_phone_owned",
         i.check.current_value = "none",
         i.check.value = "none",
         i.check.issue_id = ifelse(int.type_phone_owned_count > 1 & `type_phone_owned/none` == 1, "logic_c_type_phone_owned", "expected_response"),
         i.check.issue = glue("none option selected with other options: {type_phone_owned}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  filter(i.check.issue_id == "logic_c_type_phone_owned") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_type_phone_owned_an")){
  if(nrow(df_c_type_phone_owned_an) > 0){
    logic_output$df_c_type_phone_owned_an <- df_c_type_phone_owned_an
  }
}
# If they previously selected "yes" to having mobile internet coverage (Q56) and now replied "no", the survey needs to be checked.
# mobile_internet == "yes" and internet_awareness == "no"

df_c_internet_awareness_an <- df_tool_data_an %>% 
  filter(mobile_internet == "yes", internet_awareness == "no") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "internet_awareness",
         i.check.current_value = internet_awareness,
         i.check.value = NA,
         i.check.issue_id = "logic_c_internet_awareness",
         i.check.issue = "mobile_internet: yes but internet_awareness: no",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_internet_awareness_an")){
  if(nrow(df_c_internet_awareness_an) > 0){
    logic_output$df_c_internet_awareness_an <- df_c_internet_awareness_an
  }
}  
# If in previous qn "why do you want to have  a mobile money account?" they answered "it is safer than keeping cash at home" and they now asnwered "the system is not safe i am concerned that my money will disappear", survey needs to be checked
# reason_want_mm_acc/safer_than_home == 1 and reason_not_open_mm_acc/unsafe_system
df_c_reason_not_open_mm_acc_an <- df_tool_data_an %>% 
  filter(`reason_want_mm_acc/safer_than_home` == 1, `reason_not_open_mm_acc/unsafe_system` == 1) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_not_open_mm_acc",
         i.check.current_value = "unsafe_system",
         i.check.value = "unsafe_system",
         i.check.issue_id = "logic_c_reason_not_open_mm_acc",
         i.check.issue = "reason_want_mm_acc: safer_than_home but reason_not_open_mm_acc: unsafe_system",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_reason_not_open_mm_acc_an")){
  if(nrow(df_c_reason_not_open_mm_acc_an) > 0){
    logic_output$df_c_reason_not_open_mm_acc_an <- df_c_reason_not_open_mm_acc_an
  }
}
# if in previous question 'Why do you want to have a pre-paid or smart card?' answered "it will allow me to securely store my money" and they now chose "the system is not safe i am concerned that my money will disappear", check survey
# reason_want_card/safe_storage and reason_not_want_card/unsafe_system
df_c_reason_not_want_card_an <- df_tool_data_an %>% 
  filter(`reason_want_card/safe_storage` == 1, `reason_not_want_card/unsafe_system` == 1) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_not_want_card",
         i.check.current_value = "unsafe_system",
         i.check.value = "unsafe_system",
         i.check.issue_id = "logic_c_reason_not_want_card",
         i.check.issue = "reason_want_card: safer_than_home but reason_not_want_card: unsafe_system",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_reason_not_want_card_an")){
  if(nrow(df_c_reason_not_want_card_an) > 0){
    logic_output$df_c_reason_not_want_card_an <- df_c_reason_not_want_card_an
  }
}  






  