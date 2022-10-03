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


# Economic activity (per HH member)
refugee_summary_num_econ_activity_per_hh <- df_data_b %>%
  filter(!is.na(b12), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                  "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(a5b, b12) %>% 
  summarise(
    hhs_doing_econ_activity = n(),
    percentage_distribution = (hhs_doing_econ_activity/nrow(.))*100) %>%
     arrange(desc(hhs_doing_econ_activity))


host_summary_num_econ_activity_per_hh <- df_data_b %>%
  filter(!is.na(b12),(a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(a2, b12) %>% 
  summarise(
    hhs_doing_econ_activity = n(),
    percentage_distribution = (hhs_doing_econ_activity/nrow(.))*100) %>% 
  arrange(desc(hhs_doing_econ_activity))



# HH incomes
#refugee_summary_hh_income <- df_data_hi %>% 
# left_join(df_data_b, by = "uhhidfs" ) %>% 
#mutate()


# Access to credit

refugee_access_to_credit <- df_data_e %>%
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(!is.na(e3a), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                  "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(a5b, e3a) %>% 
  summarise(
    refugees_able_to_access_credit = n(),
    percentage_distribution = (refugees_able_to_access_credit/nrow(.))*100) %>% 
  arrange(desc(refugees_able_to_access_credit))


host_access_to_credit <- df_data_e %>%
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(!is.na(e3a),(a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 1))%>%
  group_by(a2, e3a) %>% 
  summarise(
    hosts_able_to_access_credit = n(),
    percentage_distribution = (hosts_able_to_access_credit/nrow(.))*100) %>% 
  arrange(desc(hosts_able_to_access_credit))


# Credit amount accessed in the past 12 months

refugee_credit_amount_accessed <- df_data_e %>%
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter((e3b > 50), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(a5b) %>% 
  summarise(
    average_amount_of_credit_refugees_accessed = mean(e3b, na.rm = TRUE)) %>% 
  arrange(desc(average_amount_of_credit_refugees_accessed))


host_credit_amount_accessed <- df_data_e %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter((e3b > 50), (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(a2) %>% 
  summarise(
    average_amount_of_credit_host_accessed = mean(e3b, na.rm = TRUE)) %>% 
  arrange(desc(average_amount_of_credit_host_accessed))


# Completed education (per HH member)

refugee_highest_educ_level <- df_data_b %>% 
  filter(!is.na(b6), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(a5b, b6) %>% 
  summarise(
    hh_member_education_level = n(),
    percentage_distribution = (hh_member_education_level/nrow(.))*100) %>% 
  arrange(b6)


host_highest_educ_level <- df_data_b %>%
  filter(!is.na(b6),(a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>%
  group_by(a2, b6) %>% 
  summarise(
    hh_member_education_level = n(),
    percentage_distribution = (hh_member_education_level/nrow(.))*100) %>% 
  arrange(b6)


# hoh educ completed

refugee_hoh_educ_level_completed <- df_data_b %>% 
  filter(!is.na(b6), b2 == 1, (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                          "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(a5b, b6, b2) %>% 
  summarise(
    hoh_education_level = n(),
    percentage_distribution = (hoh_education_level/nrow(.))*100) %>% 
  arrange(b6)


host_hoh_educ_level_completed <- df_data_b %>% 
  filter(!is.na(b6), b2 == 1, (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                               & hh_type == 0))%>% 
  group_by(a2, b6, b2) %>% 
  summarise(
    hoh_education_level = n(),
    percentage_distribution = (hoh_education_level/nrow(.))*100) %>% 
  arrange(b6)


# hh trainings received


#refugee_trainings_received <- df_data_hi %>%
  #left_join(df_data_a, by = "uhhidfs") %>% 
 # filter(!is.na(h2_1), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement")
                        #& hh_type == 1))

# Distance in km to public/private health facility
# How do I eliminate outliers?

refugee_distance_to_health_facility <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(d1_3 != 999, (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                  "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(a5b) %>% 
  summarise(
    distance_to_health_facility = mean(d1_3, na.rm = TRUE) %>% 
    round(1)) %>% 
    arrange(desc(distance_to_health_facility))


host_distance_to_health_facility <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter((d1_3 != 999 & d1_3 !=450), (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                       & hh_type == 0))%>% 
  group_by(a2) %>% 
  summarise(
    distance_to_health_facility = mean(d1_3, na.rm = TRUE) %>% 
      round(1)) %>% 
  arrange(desc(distance_to_health_facility))


# Arable land size (acres)
# remove outliers

refugee_arable_land_size <- df_data_c %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(!is.na(c7), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(a5b) %>% 
  summarise(
    arable_land_size = mean(c7, na.rm = TRUE) %>% 
      round(1)) %>% 
  arrange(desc(arable_land_size))


host_arable_land_size <- df_data_c %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(!is.na(c7), (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(a2) %>% 
  summarise(
    arable_land_size = mean(c7, na.rm = TRUE) %>% 
      round(1)) %>% 
  arrange(desc(arable_land_size))


# Livestock owned
# remove outliers

refugee_livestock_owned <- df_data_f %>% 
  filter((!is.na(f29_3) & f29_3 != 999 & f29_3 !=80000 & f29_3 != 0), (!is.na(f29_2)),
         (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                     "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(a5b, f29_2) %>% 
  summarise(
    number_livestock_owned = n(),
      percentage_distribution = (number_livestock_owned/nrow(.))*100) %>% 
    arrange(desc(a5b))


host_livestock_owned <- df_data_f %>% 
  filter((!is.na(f29_3) & f29_3 != 999 & f29_3 != 0), (!is.na(f29_2)), (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                        & hh_type == 0))%>%  
  group_by(a2, f29_2) %>% 
  summarise(
    number_livestock_owned = n(), 
      percentage_distribution = (number_livestock_owned/nrow(.))*100) %>% 
  arrange(desc(a2))


# Primary source of drinking water
refugee_primary_drinking_water_source <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(!is.na(d3), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(a5b, d3) %>% 
  summarise(
    primary_drinking_water_source = n(), 
    percentage_distribution = (primary_drinking_water_source/nrow(.))*100) %>% 
    arrange(desc(d3))


host_primary_drinking_water_source <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(!is.na(d3), (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(a2, d3) %>% 
  summarise(
    primary_drinking_water_source = n(),
    percentage_distribution = (primary_drinking_water_source/nrow(.))*100) %>% 
  arrange(desc(d3))


# Time taken to primary source of water
# remove outliers
refugee_time_taken_to_water_source <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(!is.na(d5), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                 "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
  group_by(a5b) %>% 
  summarise(
  minutes_taken_to_water_source = (mean(d5, na.rm = TRUE)/2) %>% 
      round(1)) %>% 
  arrange(desc(minutes_taken_to_water_source))


host_time_taken_to_water_source <- df_data_d %>% 
  left_join(df_data_a, by = "uhhidfs") %>% 
  filter(!is.na(d5), (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(a2) %>% 
  summarise(
    minutes_taken_to_water_source = (mean(d5, na.rm = TRUE)/2) %>% 
      round(1)) %>% 
  arrange(desc(minutes_taken_to_water_source))
 

# Ownership of agricultural assets
# remove outliers

 refugee_own_agric_assets <- df_data_c %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rowwise() %>% 
    mutate(total_assets = sum(across(c5_1:c5_16), na.rm = TRUE)) %>% 
  filter(!is.na(total_assets), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", 
                                "Omugo_Rhino_ext_settlement", "Rhinocamp_settlement") & hh_type == 1))%>% 
   group_by(a5b) %>% 
   summarise(
     average_agric_assets_per_hh = (mean(total_assets, na.rm = TRUE)) %>% 
       round(0)) %>% 
   arrange(desc(average_agric_assets_per_hh))
 

 host_own_agric_assets <- df_data_c %>% 
   left_join(df_data_a, by = "uhhidfs") %>% 
   rowwise() %>% 
   mutate(total_assets = sum(across(c5_1:c5_16), na.rm = TRUE)) %>% 
   filter(!is.na(total_assets), (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                                 & hh_type == 0))%>% 
   group_by(a2) %>% 
   summarise(
     average_agric_assets_per_hh = (mean(total_assets, na.rm = TRUE)) %>% 
       round(0)) %>% 
   arrange(desc(average_agric_assets_per_hh)) 
 









