# load packages

library(tidyverse)
library(lubridate)
library(glue)

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
              
if(exists("df_c_time_btn_surveys_an"))
  if(nrow(df_c_survey_time_an) > 0){
    logic_output$df_c_time_btn_survey_an <- df_c_time_btn_survey_an
  
  
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
  
  






  