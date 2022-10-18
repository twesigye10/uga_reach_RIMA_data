# Summary statistics 2019

# load library
library(tidyverse)

df_data_x <- readRDS("inputs/anon_module_x.rdata")

df_data_a <- readRDS("inputs/anon_module_a.rdata")

df_data_b <- readRDS("inputs/anon_module_b.rdata")

df_data_c <- readRDS("inputs/anon_module_c.rdata")

df_data_d <- readRDS("inputs/anon_module_d.rdata")

df_data_e <- readRDS("inputs/anon_module_e.rdata")

df_data_f <- readRDS("inputs/anon_module_f.rdata")

df_data_g1 <- readRDS("inputs/anon_module_g1.rdata")

df_data_g2 <- readRDS("inputs/anon_module_g2.rdata")

df_data_hi <- readRDS("inputs/anon_module_h1.rdata")

df_data_h2 <- readRDS("inputs/anon_module_h2.rdata")

df_data_h3 <- readRDS("inputs/anon_module_h3.rdata")

df_data_i <- readRDS("inputs/anon_module_i.rdata")

df_data_j <-  readRDS("inputs/anon_module_j.rdata")

df_data_k <- readRDS("inputs/anon_module_k.rdata")

df_data_l <-  readRDS("inputs/anon_module_l.rdata")

df_data_s <- readRDS("inputs/anon_module_s.rdata")

df_data_w <- readRDS("inputs/anon_module_w.rdata")


# Economic activity (per HH)

refugee_econ_activity_per_hh_member <- df_data_b %>%
  group_by(uhhidfs) %>% 
  rename(economic_activity = "b12") %>% 
  mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b)) %>%
  ungroup() %>% 
  filter(is.na(economic_activity), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                  "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(settlement_name) %>% 
  summarise(
    Number_hh_having_econ_activity = n())%>%
     arrange(settlement_name)


host_summary_num_econ_activity_per_hh_member <- df_data_b %>%
  group_by(uhhidfs) %>% 
  rename(district_name = "a2",
         economic_activity = "b12") %>%
  ungroup(uhhidfs) %>% 
  filter(!is.na(economic_activity),(district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(district_name, economic_activity) %>% 
  summarise(
    Number_hh_having_econ_activity = n())%>%
  arrange(district_name)


# HH incomes
# pending
 
   

 

# HH access to credit by 

refugee_access_to_credit <- df_data_e %>%
    left_join(df_data_a, by = "uhhidfs") %>% 
  rename(acces_to_credit = "e3a") %>% 
  mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b)) %>%
  filter(!is.na(acces_to_credit), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                  "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(settlement_name, acces_to_credit) %>% 
  summarise(
    hh_able_to_access_credit = n(),
    percentage_distribution = (hh_able_to_access_credit/nrow(.))*100) %>% 
  arrange(desc(hh_able_to_access_credit))


host_access_to_credit <- df_data_e %>%
  left_join(df_data_a, by = "uhhidfs") %>% 
  rename(district_name = "a2",
         acces_to_credit = "e3a") %>% 
  filter(!is.na(acces_to_credit),(district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 1))%>%
  group_by(district_name, acces_to_credit) %>% 
  summarise(
    hh_able_to_access_credit = n(),
    percentage_distribution = (hh_able_to_access_credit/nrow(.))*100) %>% 
  arrange(desc(hh_able_to_access_credit))


# Number hh accessed credit in the past 12 months

refugee_accessed_credit <- df_data_e %>%
  left_join(df_data_a, by = "uhhidfs") %>%
  rename(number_accessed_credit = "e3b") %>% 
  mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b)) %>%
  filter((number_accessed_credit > 50), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                              "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name) %>% 
  summarise(
    number_accessed_credit = n(),
    percentage_distribution = (number_accessed_credit/nrow(.))*100) %>% 
  arrange(desc(number_accessed_credit))


host_accessed_credit <- df_data_e %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  rename(district_name = "a2",
         number_accessed_credit = "e3b") %>% 
  filter((number_accessed_credit > 50), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                       & hh_type == 0))%>% 
  group_by(district_name) %>% 
  summarise(
    number_accessed_credit = n(),
    percentage_distribution = (number_accessed_credit/nrow(.))*100) %>% 
arrange(desc(number_accessed_credit))



# Completed education (per HH member)

refugee_highest_educ_level <- df_data_b %>% 
  rename(education_level = "b6") %>% 
   mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b)) %>%
  filter(!is.na(education_level), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(settlement_name, education_level) %>% 
  summarise(
    number_completed_level = n(),
    percentage_distribution = (number_completed_level/nrow(.))*100) %>% 
  arrange(education_level)


host_highest_educ_level <- df_data_b %>%
  rename(district_name = "a2",
         education_level = "b6") %>%
  filter(!is.na(education_level),(district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>%
  group_by(district_name, education_level) %>% 
  summarise(
    number_completed_level = n(),
    percentage_distribution = (number_completed_level/nrow(.))*100) %>% 
  arrange(education_level)


# Hoh educ completed

refugee_hoh_educ_level_completed <- df_data_b %>% 
  rename(education_level = "b6",
         relation_to_hoh = "b2") %>% 
  mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b)) %>%
  filter(!is.na(education_level), relation_to_hoh == 1, (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                          "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(settlement_name, education_level) %>% 
  summarise(
    number_completed_level = n(),
    percentage_distribution = (number_completed_level/nrow(.))*100) %>% 
  arrange(education_level)


host_hoh_educ_level_completed <- df_data_b %>% 
  rename(education_level = "b6",
         relation_to_hoh = "b2",
         district_name = "a2") %>% 
  filter(!is.na(education_level), relation_to_hoh == 1, (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                               & hh_type == 0))%>% 
  group_by(district_name, education_level) %>% 
  summarise(
    number_completed_level = n(),
    percentage_distribution = (number_completed_level/nrow(.))*100) %>% 
  arrange(education_level)


# Training received by type

summary_hh_training <- df_data_hi %>% 
  left_join(df_data_b, by = "uhhidfs" ) %>% 
  distinct(uhhidfs, .keep_all = TRUE) %>% 
  rename(district_name = "a2")%>%
  separate(col = h2_1, into = c("none_1", "prev_disasters_1", "use_climate_info_1", "manage_natural_resources_1", "gd_agric_practices_1", "climate_smart_agric_1",
                                "pest_disease_cntrol_1", "business_skills_1", "agri_value_addition_1", "livestock_manage_1", "animal_health_1")) %>% 
  mutate(none = ifelse(none_1 == 0 | prev_disasters_1 == 0 | use_climate_info_1 == 0 | manage_natural_resources_1 == 0 | gd_agric_practices_1 == 0 | climate_smart_agric_1 == 0 |
                         pest_disease_cntrol_1 == 0 | business_skills_1 == 0 | agri_value_addition_1 == 0 | livestock_manage_1 == 0 | animal_health_1 ==0, 1, NA),
         prev_disasters = ifelse(none_1 == 1 | prev_disasters_1 == 1 |use_climate_info_1 == 1 | manage_natural_resources_1 == 1 | gd_agric_practices_1 == 1 | climate_smart_agric_1 == 1 |
                                   pest_disease_cntrol_1 == 1 | business_skills_1 == 1 | agri_value_addition_1 == 1 | livestock_manage_1 == 1 | animal_health_1 ==1, 1, NA),
         use_climate_info = ifelse(none_1 == 2 | prev_disasters_1 == 2 |use_climate_info_1 == 2 | manage_natural_resources_1 == 2 | gd_agric_practices_1 == 2 | climate_smart_agric_1 == 2 |
                                     pest_disease_cntrol_1 == 2 | business_skills_1 == 2 | agri_value_addition_1 == 2 | livestock_manage_1 == 2 | animal_health_1 ==2, 1, NA),
         manage_natural_resources = ifelse(none_1 == 3 | prev_disasters_1 == 3 |use_climate_info_1 == 3 | manage_natural_resources_1 == 3 | gd_agric_practices_1 == 3 | climate_smart_agric_1 == 3 |
                                             pest_disease_cntrol_1 == 3 | business_skills_1 == 3 | agri_value_addition_1 == 3 | livestock_manage_1 == 3 | animal_health_1 ==3, 1, NA),
         gd_agric_practices = ifelse(none_1 == 4 | prev_disasters_1 == 4 |use_climate_info_1 == 4 | manage_natural_resources_1 == 4 | gd_agric_practices_1 == 4 | climate_smart_agric_1 == 4 |
                                       pest_disease_cntrol_1 == 4 | business_skills_1 == 4 | agri_value_addition_1 == 4 | livestock_manage_1 == 4 | animal_health_1 ==4, 1, NA),
         climate_smart_agric = ifelse(none_1 == 5 | prev_disasters_1 == 5 |use_climate_info_1 == 5 | manage_natural_resources_1 == 5 | gd_agric_practices_1 == 5 | climate_smart_agric_1 == 5 |
                                        pest_disease_cntrol_1 == 5 | business_skills_1 == 5 | agri_value_addition_1 == 5 | livestock_manage_1 == 5 | animal_health_1 ==5, 1, NA),
         pest_disease_cntrol = ifelse(none_1 == 6 | prev_disasters_1 == 6 |use_climate_info_1 == 6 | manage_natural_resources_1 == 6 | gd_agric_practices_1 == 6 | climate_smart_agric_1 == 6 |
                                        pest_disease_cntrol_1 == 6 | business_skills_1 == 6 | agri_value_addition_1 == 6 | livestock_manage_1 == 6 | animal_health_1 ==6, 1, NA),
         business_skills = ifelse(none_1 == 7 | prev_disasters_1 == 7 |use_climate_info_1 == 7 | manage_natural_resources_1 == 7 | gd_agric_practices_1 == 7 | climate_smart_agric_1 == 7 |
                                    pest_disease_cntrol_1 == 7 | business_skills_1 == 7 | agri_value_addition_1 == 7 | livestock_manage_1 == 7 | animal_health_1 ==7, 1, NA),
         agri_value_addition = ifelse(none_1 == 8 | prev_disasters_1 == 8 |use_climate_info_1 == 8 | manage_natural_resources_1 == 8 | gd_agric_practices_1 == 8 | climate_smart_agric_1 == 8 |
                                        pest_disease_cntrol_1 == 8 | business_skills_1 == 8 | agri_value_addition_1 == 8 | livestock_manage_1 == 8 | animal_health_1 ==8, 1, NA),
         livestock_manage = ifelse(none_1 == 9 | prev_disasters_1 == 9 |use_climate_info_1 == 9 | manage_natural_resources_1 == 9 | gd_agric_practices_1 == 9 | climate_smart_agric_1 == 9 |
                                     pest_disease_cntrol_1 == 9 | business_skills_1 == 9 | agri_value_addition_1 == 9 | livestock_manage_1 == 9 | animal_health_1 ==9, 1, NA),
         animal_health = ifelse(none_1 == 10 | prev_disasters_1 == 10 |use_climate_info_1 == 10 | manage_natural_resources_1 == 10 | gd_agric_practices_1 == 10 | climate_smart_agric_1 == 10 |
                                  pest_disease_cntrol_1 == 10 | business_skills_1 == 10 | agri_value_addition_1 == 10 | livestock_manage_1 == 10 | animal_health_1 ==10, 1, NA),
         settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b))


refugee_training_none <- summary_hh_training %>% 
  filter(!is.na(none), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                               "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, none) %>% 
  summarise(
    number_received_training = n()) %>% 
    arrange(desc(number_received_training))

  
host_training_none <- summary_hh_training %>% 
  filter(!is.na(none), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                        & hh_type == 0))%>%
  group_by(district_name, none) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


refugee_training_in_preventing_disasters <- summary_hh_training %>% 
  filter(!is.na(prev_disasters), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                               "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, prev_disasters) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_prev_disasters <- summary_hh_training %>% 
  filter(!is.na(prev_disasters), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                        & hh_type == 0))%>%
  group_by(district_name, prev_disasters) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


refugee_training_in_using_climate_info <- summary_hh_training %>% 
  filter(!is.na(use_climate_info), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                         "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, use_climate_info) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_use_climate_info <- summary_hh_training %>% 
  filter(!is.na(use_climate_info), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                  & hh_type == 0))%>%
  group_by(district_name, use_climate_info) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


refugee_training_in_managing_natural_resources <- summary_hh_training %>% 
  filter(!is.na(manage_natural_resources), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                         "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, manage_natural_resources) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


hos_training_to_manage_natural_resources <- summary_hh_training %>% 
  filter(!is.na(manage_natural_resources), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                    & hh_type == 0))%>%
  group_by(district_name, manage_natural_resources) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


refugee_training_in_gd_agric_practices <- summary_hh_training %>% 
  filter(!is.na(gd_agric_practices), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                                   "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, gd_agric_practices) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_in_gd_agric_practices <- summary_hh_training %>% 
  filter(!is.na(gd_agric_practices), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                            & hh_type == 0))%>%
  group_by(district_name, gd_agric_practices) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


refugee_training_in_climate_smart_agric <- summary_hh_training %>% 
  filter(!is.na(climate_smart_agric), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                             "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, climate_smart_agric) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_in_climate_smart_agric <- summary_hh_training %>% 
  filter(!is.na(climate_smart_agric), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                      & hh_type == 0))%>%
  group_by(district_name, climate_smart_agric) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


refugee_training_in_pest_disease_cntrol <- summary_hh_training %>% 
  filter(!is.na(pest_disease_cntrol), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                              "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, pest_disease_cntrol) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_in_pest_disease_cntrol <- summary_hh_training %>% 
  filter(!is.na(pest_disease_cntrol), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                       & hh_type == 0))%>%
  group_by(district_name, pest_disease_cntrol) %>% 
  summarise(
    number_hh_received_training = n()) %>% 
  arrange(desc(number_hh_received_training))


refugee_training_in_business_skills <- summary_hh_training %>% 
  filter(!is.na(business_skills), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                              "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, business_skills) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_in_business_skills <- summary_hh_training %>% 
  filter(!is.na(business_skills), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                       & hh_type == 0))%>%
  group_by(district_name, business_skills) %>% 
  summarise(
    number_hh_received_training = n()) %>% 
  arrange(desc(number_hh_received_training))


refugee_training_in_agri_value_addition <- summary_hh_training %>% 
  filter(!is.na(agri_value_addition), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                          "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, agri_value_addition) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_in_agri_value_addition <- summary_hh_training %>% 
  filter(!is.na(agri_value_addition), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                   & hh_type == 0))%>%
  group_by(district_name, agri_value_addition) %>% 
  summarise(
    number_hh_received_training = n()) %>% 
  arrange(desc(number_hh_received_training))


refugee_training_in_livestock_manage <- summary_hh_training %>% 
  filter(!is.na(livestock_manage), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                              "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, livestock_manage) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_in_livestock_manage <- summary_hh_training %>% 
  filter(!is.na(livestock_manage), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                       & hh_type == 0))%>%
  group_by(district_name, livestock_manage) %>% 
  summarise(
    number_hh_received_training = n()) %>% 
  arrange(desc(number_hh_received_training))


refugee_training_in_animal_health <- summary_hh_training %>% 
  filter(!is.na(animal_health), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                           "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name, animal_health) %>% 
  summarise(
    number_received_training = n()) %>% 
  arrange(desc(number_received_training))


host_training_in_animal_health <- summary_hh_training %>% 
  filter(!is.na(animal_health), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                    & hh_type == 0))%>%
  group_by(district_name, animal_health) %>% 
  summarise(
    number_hh_received_training = n()) %>% 
  arrange(desc(number_hh_received_training))



# Distance in km to public/private health facility
# Removed outliers
refugee_distance_to_health_facility <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b),
         d1_3 = as.numeric(d1_3),
         upper_quant_limit = quantile(d1_3, 0.95, na.rm = TRUE)) %>% 
  filter(d1_3 < upper_quant_limit, (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                  "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(settlement_name) %>% 
  summarise(
    distance_to_health_facility = mean(d1_3, na.rm = TRUE) %>% 
    round(1)) %>% 
    arrange(desc(distance_to_health_facility))


host_distance_to_health_facility <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  rename(district_name = "a2")%>% 
  mutate(d1_3 = as.numeric(d1_3),
         upper_quant_limit = quantile(d1_3, 0.95, na.rm = TRUE)) %>% 
  filter(d1_3 < upper_quant_limit, (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                       & hh_type == 0))%>% 
  group_by(district_name) %>% 
  summarise(
    distance_to_health_facility = mean(d1_3, na.rm = TRUE) %>% 
      round(1)) %>% 
  arrange(desc(distance_to_health_facility))


# Arable land size (acres)
# removed outliers (5%)

refugee_arable_land_size <- df_data_c %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b),
         c7 = as.numeric(c7),
         upper_quant_limit = quantile(c7, 0.95, na.rm = TRUE)) %>% 
  filter(c7 < upper_quant_limit, (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(settlement_name) %>% 
  summarise(
    arable_land_size_acreage  = mean(c7, na.rm = TRUE) %>% 
      round(1)) %>% 
  arrange(desc(arable_land_size_acreage))


host_arable_land_size <- df_data_c %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  rename(district_name = "a2")%>% 
  mutate(c7 = as.numeric(c7),
         upper_quant_limit = quantile(c7, 0.95, na.rm = TRUE)) %>% 
  
  filter(c7 < upper_quant_limit, (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(district_name) %>% 
  summarise(
    arable_land_size_acreage = mean(c7, na.rm = TRUE) %>% 
      round(1)) %>% 
  arrange(desc(arable_land_size_acreage))


# Livestock owned
refugee_livestock_owned <- df_data_f %>% 
  group_by(uhhidfs) %>% 
 mutate(settlement_name = case_when(a5b %in% c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b),
        f29_3 = as.numeric(f29_3)) %>% 
  ungroup() %>% 
  rename(livestock_type = f29_2) %>% 
  filter(!is.na(f29_3) & f29_3 != 0,
         (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                     "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(settlement_name, livestock_type) %>% 
  summarise(
    average_livestock_owned_per_hh = mean(f29_3) %>%
  round(0)) %>% 
    arrange(settlement_name)


host_livestock_owned <- df_data_f %>% 
  group_by(uhhidfs) %>% 
  mutate(f29_3 = as.numeric(f29_3)) %>% 
  ungroup() %>% 
  rename(district_name = a2,
         livestock_type = f29_2) %>%
  filter(!is.na(f29_3) & f29_3 != 0,
         (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
          & hh_type == 0))%>%  
  group_by(district_name, livestock_type) %>% 
  summarise(
    average_livestock_owned_per_hh = mean(f29_3) %>% 
      round(0)) %>% 
    arrange(district_name)


  # Primary source of drinking water
refugee_primary_drinking_water_source <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b)) %>%
  rename(water_source_type = "d3") %>% 
  filter(!is.na(water_source_type), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(settlement_name, water_source_type) %>% 
  summarise(
    number_using_water_source = n(), 
    percentage_distribution = (number_using_water_source/nrow(.))*100) %>% 
    arrange(desc(settlement_name))


host_primary_drinking_water_source <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  rename(district_name = "a2",
         water_source_type = "d3") %>%
  filter(!is.na(water_source_type), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(district_name, water_source_type) %>% 
  summarise(
    number_using_water_source = n(),
    percentage_distribution = (number_using_water_source/nrow(.))*100) %>% 
  arrange(desc(district_name))


# Time taken to primary source of water
refugee_time_taken_to_water_source <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>%   
  mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                     TRUE ~ a5b)) %>%
  filter(!is.na(d5), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(settlement_name) %>% 
  summarise(
  minutes_taken_to_water_source = (mean(d5, na.rm = TRUE)/2) %>% 
      round(1)) %>% 
  arrange(desc(minutes_taken_to_water_source))



host_time_taken_to_water_source <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  rename(district_name = "a2") %>%
  filter(!is.na(d5), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(district_name) %>% 
  summarise(
    minutes_taken_to_water_source = (mean(d5, na.rm = TRUE)/2) %>% 
      round(1)) %>% 
  arrange(desc(minutes_taken_to_water_source))
 

# Ownership of agricultural assets
 refugee_agric_assets_owned <- df_data_c %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rowwise() %>% 
   mutate(total_assets = sum(across(c5_1:c5_16), na.rm = TRUE),
          total_assets = as.numeric(total_assets),
          settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                       TRUE ~ a5b))%>%  
  filter(!is.na(total_assets), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(settlement_name) %>% 
   summarise(
       average_agric_assets_per_hh = (mean(total_assets, na.rm = TRUE)) %>% 
       round(0)) %>% 
   arrange(desc(average_agric_assets_per_hh))
 

  host_own_agric_assets <- df_data_c %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(district_name = "a2") %>%
   rowwise() %>% 
   mutate(total_assets = sum(across(c5_1:c5_16), na.rm = TRUE),
          total_assets = as.numeric(total_assets)) %>% 
   filter(!is.na(total_assets), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                 & hh_type == 0))%>% 
   group_by(district_name) %>% 
   summarise(
      average_agric_assets_per_hh = (mean(total_assets, na.rm = TRUE)) %>% 
      round(0)) %>% 
   arrange(desc(average_agric_assets_per_hh)) 

  
# Cell phone ownership (per HH member)
  refugee_phone_ownership_per_hh_member <- df_data_c %>% 
   left_join(df_data_b, by = "uhhidfs") %>% 
   mutate(new_value = ifelse(duplicated(uhhidfs), NA, c6_1),
          settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                      TRUE ~ a5b))%>%  
   filter(new_value != 785007753, (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                            "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
   group_by(settlement_name) %>% 
     summarise(
       Total_phones = sum(new_value),
       average_phones_per_hh   =  mean(new_value),
       percentage_distribution = (Total_phones/nrow(.))*100)%>% 
       arrange(desc(average_phones_per_hh)) 
 
      
  host_phone_ownership_per_hh_member <- df_data_c %>% 
    left_join(df_data_b, by = "uhhidfs") %>% 
    mutate(new_value = ifelse(duplicated(uhhidfs), NA, c6_1)) %>% 
    rename(district_name = a2) %>%       
    filter(new_value != 785007753, (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                    & hh_type == 0))%>% 
    group_by(district_name) %>% 
    summarise(
      Total_phones = sum(new_value),
      average_phones_per_hh   =  mean(new_value),
      percentage_distribution = (Total_phones/nrow(.))*100)%>% 
    arrange(desc(average_phones_per_hh)) 
     
 
  # Other NFI ownership aggregates
 # Failed to apply quantile to filter outliers
 refugee_non_agric_assets_owned <- df_data_c %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rowwise() %>% 
    mutate(total_assets = sum(across(c6_1:c6_18, na.rm = TRUE)),
           new_value = as.numeric(total_assets),
           settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                           TRUE ~ a5b)) %>% 
    filter(!new_value %in% c(785007779, 5015, 3689, 119, 84, 93), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                            "Rhinocamp_settlement") & hh_type == 1))%>% 
    group_by(settlement_name) %>% 
     summarise(
     number_assets_owned = sum(new_value),
     average_non_agric_assets_per_hh = (mean(new_value, na.rm = TRUE)) %>% 
       round(0)) %>% 
   arrange(desc(average_non_agric_assets_per_hh))


   
 host_non_agric_assets_owned <- df_data_c %>%  
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(district_name = "a2") %>% 
   rowwise() %>% 
   mutate(total_assets = sum(across(c6_1:c6_18, na.rm = TRUE)),
          new_value = as.numeric(total_assets)) %>% 
    filter(!new_value %in% c(785007779, 5015, 3689, 119, 84, 93), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                                                   & hh_type == 0))%>% 
   group_by(district_name) %>% 
   summarise(
     number_assets_owned = sum(new_value),
     average_non_agric_assets_per_hh = (mean(new_value, na.rm = TRUE)) %>% 
       round(0)) %>% 
   arrange(desc(average_non_agric_assets_per_hh))
 
   
   
   # Distance in km to petty trading market
 refugee_distance_to_trading_market <- df_data_d %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                      TRUE ~ a5b),
          d1_7 = as.numeric(d1_7),
          upper_quant_limit = quantile(d1_7, 0.95, na.rm = TRUE)) %>% 
   filter(d1_7 < upper_quant_limit, (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                            "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(settlement_name) %>% 
   summarise(
     distance_to_trading_market = mean(d1_7, na.rm = TRUE) %>% 
       round(1)) %>% 
   arrange(desc(distance_to_trading_market))
 
 
 host_distance_to_trading_market <- df_data_d %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(district_name = a2)%>% 
   mutate(d1_7 = as.numeric(d1_7),
          upper_quant_limit = quantile(d1_7, 0.95, na.rm = TRUE)) %>% 
   filter(d1_7 < upper_quant_limit, (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                     & hh_type == 0))%>% 
   group_by(district_name) %>% 
   summarise(
     distance_to_trading_market = mean(d1_7, na.rm = TRUE) %>% 
       round(1)) %>% 
   arrange(desc(distance_to_trading_market))
 
 
 # Level of trust with other groups
 refugee_trust_level_with_others <- df_data_s %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(trust_level = s7) %>% 
   mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                      TRUE ~ a5b)) %>% 
    filter(!trust_level %in% c(999, 998), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                           "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(settlement_name, trust_level) %>% 
   summarise(
     number_by_level_of_trust = n(),
     percentage_distribution = (number_by_level_of_trust/nrow(.))*100) %>% 
   arrange(trust_level)
 
 
 host_trust_level_with_others <- df_data_s %>%
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(district_name = a2,
          trust_level = s7) %>%
    filter(!trust_level %in% c(999, 998), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                   & hh_type == 0))%>%
   group_by(district_name, trust_level) %>% 
   summarise(
     number_by_level_of_trust = n(),
     percentage_distribution = (number_by_level_of_trust/nrow(.))*100) %>% 
   arrange(trust_level)
 
 
 # Feeling of belonging
 refugee_feel_of_belonging_to_community <- df_data_s %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(feel_of_belonging_level = s8) %>% 
   mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                      TRUE ~ a5b))%>%
   filter(!feel_of_belonging_level %in% c(999, 998), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                                   "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(settlement_name, feel_of_belonging_level) %>% 
   summarise(
     number_by_level_of_belonging = n(),
     percentage_distribution = (number_by_level_of_belonging/nrow(.))*100) %>% 
   arrange(feel_of_belonging_level)
 
 
 host_feel_of_belonging_to_community <- df_data_s %>%
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(district_name = a2,
          feel_of_belonging_level = s8) %>%
    filter(!feel_of_belonging_level %in% c(999, 998), (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                           & hh_type == 0))%>%
   group_by(district_name, feel_of_belonging_level) %>% 
   summarise(
     number_by_level_of_belonging = n(),
     percentage_distribution = (number_by_level_of_belonging/nrow(.))*100) %>% 
   arrange(feel_of_belonging_level)
 
 
 
 # Frequency Travel to urban areas (per HH_more than once in past 12 months)
 # indicator complicated
 refugee_freq_travel_tO_urban <- df_data_b %>% 
   group_by(uhhidfs) %>% 
   rename(freq_travel_tO_urban = b20b) %>% 
   mutate(settlement_name = case_when(a5b %in% c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                      TRUE ~ a5b))%>%
   ungroup() %>% 
   filter(freq_travel_tO_urban %in% c(1,2), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                                             "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(settlement_name, freq_travel_tO_urban) %>% 
   summarise(
     number_travelling_to_urban = n()) %>% 
      mutate(percentage_distribution = (number_travelling_to_urban/sum(number_travelling_to_urban)*100) %>% 
   round(1)) %>% 
      arrange(freq_travel_tO_urban)
 
 # Few host were interviewed for this indicator
 # None of host interviewed travel frequently
 
 
 # Travel to country of origin (per HH)
 # indicator complicated 
 
 refugee_freq_travel_tO_origin_ctry <- df_data_b %>% 
 group_by(uhhidfs) %>% 
   rename(freq_travel_tO_origin_ctry = b22c) %>% 
   mutate(settlement_name = case_when(a5b %in% c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                      TRUE ~ a5b))%>%
   ungroup() %>% 
   filter(freq_travel_tO_origin_ctry %in% c(1,2), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                                    "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(settlement_name, freq_travel_tO_origin_ctry) %>% 
   summarise(
     number_travelling_to_origin_ctry = n()) %>% 
   mutate(percentage_distribution = (number_travelling_to_origin_ctry/sum(number_travelling_to_origin_ctry)*100) %>% 
            round(1)) %>% 
   arrange(freq_travel_tO_origin_ctry)
 
 
 
 # Percentage of hh with poor consumption score
 # HH got a total FCS of 21 or less i.e. 
  # cereals/tubers * 2 + pulses * 3 + vegetables + fruits + protein *4 + dairy *4 + sugar * 0.5 + oils * 0.5 <=21
 
 refugee_hh_with_poor_fcs <- df_data_g1 %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(cereal_days = g1b_1, tuber_days = g1b_2, pulse_days = g1b_3, orange_veg_days = g1b_4, green_leafy_veg_days = g1b_5,
          other_veg_days = g1b_6, orange_fruit_days = g1b_7, other_fruit_days = g1b_8, beaf_days = g1b_9, organ_meat_days = g1b_10, 
          fish_days = g1b_11, egg_days = g1b_12, milk_days = g1b_13, sugar_days = g1b_14, oil_days = g1b_15, 
          condiment_days = g1b_16, insect_days = g1b_17) %>% 
   mutate(cereal_tuber = ifelse(cereal_days > tuber_days, cereal_days*2, tuber_days*2),
          vegetables_1 = ifelse(orange_veg_days > green_leafy_veg_days, orange_veg_days, green_leafy_veg_days),
          final_vegetale = ifelse(vegetables_1 > other_veg_days, vegetables_1, other_veg_days),
          protein_1 = ifelse(beaf_days > fish_days, beaf_days, fish_days),
          protein_2 = ifelse(protein_1 > organ_meat_days, protein_1, organ_meat_days),
          protein_3 = ifelse(protein_2 > egg_days, protein_2, egg_days),
          final_protein = ifelse(protein_3 > milk_days, protein_3, milk_days),
          fruits = ifelse(orange_fruit_days > other_fruit_days, orange_fruit_days, other_fruit_days),
          fcs = cereal_tuber + pulse_days*3 + final_vegetale + fruits + final_protein*4 + sugar_days*0.5 + oil_days*0.5,
          settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                    TRUE ~ a5b)) %>% 
   filter(fcs <=21, (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(settlement_name) %>% 
   summarise(
     number_hh_with_poor_fcs = n(),
     percentage_distribution = (number_hh_with_poor_fcs/nrow(.))*100) %>% 
   arrange(desc(number_hh_with_poor_fcs))
 
 
 host_hh_with_poor_fcs <- df_data_g1 %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rename(cereal_days = g1b_1, tuber_days = g1b_2, pulse_days = g1b_3, orange_veg_days = g1b_4, green_leafy_veg_days = g1b_5,
          other_veg_days = g1b_6, orange_fruit_days = g1b_7, other_fruit_days = g1b_8, beaf_days = g1b_9, organ_meat_days = g1b_10, 
          fish_days = g1b_11, egg_days = g1b_12, milk_days = g1b_13, sugar_days = g1b_14, oil_days = g1b_15, 
          condiment_days = g1b_16, insect_days = g1b_17) %>% 
   mutate(cereal_tuber = ifelse(cereal_days > tuber_days, cereal_days*2, tuber_days*2),
          vegetables_1 = ifelse(orange_veg_days > green_leafy_veg_days, orange_veg_days, green_leafy_veg_days),
          final_vegetale = ifelse(vegetables_1 > other_veg_days, vegetables_1, other_veg_days),
          protein_1 = ifelse(beaf_days > fish_days, beaf_days, fish_days),
          protein_2 = ifelse(protein_1 > organ_meat_days, protein_1, organ_meat_days),
          protein_3 = ifelse(protein_2 > egg_days, protein_2, egg_days),
          final_protein = ifelse(protein_3 > milk_days, protein_3, milk_days),
          fruits = ifelse(orange_fruit_days > other_fruit_days, orange_fruit_days, other_fruit_days),
          fcs = cereal_tuber + pulse_days*3 + final_vegetale + fruits + final_protein*4 + sugar_days*0.5 + oil_days*0.5,
          settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                      TRUE ~ a5b)) %>% 
   rename(district_name = a2) %>% 
   filter(fcs <=21, (district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                     & hh_type == 0))%>%
   group_by(district_name) %>% 
   summarise(
     number_hh_with_poor_fcs = n(),
     percentage_distribution = (number_hh_with_poor_fcs/nrow(.))*100) %>% 
   arrange(desc(number_hh_with_poor_fcs))
 
 
     # Main livelihood of household
 # indicator complicated
 refugee_hh_main_livelihood <- df_data_a %>%
   rename(main_livelihood = "a16") %>% 
   mutate(settlement_name = case_when(a5b %in%c("Omugo_Rhino_ext_settlement") ~ "Rhinocamp_settlement",
                                      TRUE ~ a5b)) %>%
   filter(!is.na(main_livelihood), (settlement_name %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                                             "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(settlement_name, main_livelihood) %>% 
   summarise(
     Number_hh_by_main_livelihood = n(),
     percentage_distribution = (Number_hh_by_main_livelihood/nrow(.))*100) %>%
   arrange(settlement_name)
 
 
 
 host_summary_num_econ_activity_per_hh_member <- df_data_b %>%
   rename(district_name = "a2",
          economic_activity = "b12") %>% 
   filter(!is.na(economic_activity),(district_name %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                     & hh_type == 0))%>% 
   group_by(district_name, economic_activity) %>% 
   summarise(
     Number_people_by_econ_activity = n(),
     percentage_distribution = (Number_people_by_econ_activity/nrow(.))*100) %>% 
   arrange(district_name)
 
 
 
 
 
 
 
 
 
 





