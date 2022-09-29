
# load library
library(tidyverse)

df_data_x <- readRDS("R/anon_module_x.rdata")

df_data_a <- readRDS("R/anon_module_a.rdata")

df_data_b <- readRDS("R/anon_module_b.rdata")

df_data_c <- readRDS("R/anon_module_c.rdata")

df_data_d <- readRDS("R/anon_module_d.rdata")

df_data_e <- readRDS("R/anon_module_e.rdata")

df_data_f <- readRDS("R/anon_module_f.rdata")

df_data_g1 <- readRDS("R/anon_module_g1.rdata")

df_data_g2 <- readRDS("R/anon_module_g2.rdata")

df_data_hi <- readRDS("R/anon_module_h1.rdata")

df_data_h2 <- readRDS("R/anon_module_h2.rdata")

df_data_h3 <- readRDS("R/anon_module_h3.rdata")

df_data_i <- readRDS("R/anon_module_i.rdata")

df_data_j <-  readRDS("R/anon_module_j.rdata")

df_data_k <- readRDS("R/anon_module_k.rdata")

df_data_l <-  readRDS("R/anon_module_l.rdata")

df_data_s <- readRDS("R/anon_module_s.rdata")

df_data_w <- readRDS("R/anon_module_w.rdata")


# Economic activity (per HH member)
refugee_summary_num_econ_activity_per_hh <- df_data_b %>%
  filter(!is.na(b12), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement")
                       & hh_type == 1))%>% 
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
  left_join(df_data_b, by = "uhhidfs") %>% 
  filter(!is.na(e3a), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement")
                       & hh_type == 1)) %>% 
  group_by(a5b, e3a) %>% 
  summarise(
    refugees_able_to_access_credit = n(),
    percentage_distribution = (refugees_able_to_access_credit/nrow(.))*100) %>% 
  arrange(desc(refugees_able_to_access_credit))


host_access_to_credit <- df_data_e %>%
  left_join(df_data_b, by = "uhhidfs") %>% 
  filter(!is.na(e3a),(a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 1))%>%
  group_by(a2, e3a) %>% 
  summarise(
    hosts_able_to_access_credit = n(),
    percentage_distribution = (hosts_able_to_access_credit/nrow(.))*100) %>% 
  arrange(desc(hosts_able_to_access_credit))


# Credit amount accessed in the past 12 months

refugee_credit_amount_accessed <- df_data_e %>%
  left_join(df_data_b, by = "uhhidfs") %>% 
  filter((e3b > 50), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement")
                      & hh_type == 1)) %>% 
  group_by(a5b) %>% 
  summarise(
    average_amount_of_credit_refugees_accessed = mean(e3b, na.rm = TRUE)) %>% 
  arrange(desc(average_amount_of_credit_refugees_accessed))


host_credit_amount_accessed <- df_data_e %>% 
  left_join(df_data_b, by = "uhhidfs") %>% 
  filter((e3b > 50), (a2 %in% c("Yumbe", "Isingiro", "Lamwo", "Arua")
                      & hh_type == 0))%>% 
  group_by(a2) %>% 
  summarise(
    average_amount_of_credit_host_accessed = mean(e3b, na.rm = TRUE)) %>% 
  arrange(desc(average_amount_of_credit_host_accessed))



# Completed education (per HH member)

refugee_highest_educ_level <- df_data_b %>% 
  filter(!is.na(b6), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement")
                      & hh_type == 1))%>% 
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
  filter(!is.na(b6), b2 == 1, (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement")
                & hh_type == 1))%>% 
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






