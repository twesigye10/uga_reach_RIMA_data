library(tidyverse)

df_data_a <- readRDS("inputs//anon_module_a.rdata")

df_data_b <- readRDS("inputs//anon_module_b.rdata")

df_data_c <- readRDS("inputs//anon_module_d.rdata")

df_data_d <- readRDS("inputs//anon_module_d.rdata")

df_data_e <- readRDS("inputs//anon_module_e.rdata")

df_data_f <- readRDS("inputs//anon_module_f.rdata")

df_data_g1 <- readRDS("inputs//anon_module_g1.rdata")

df_data_g2 <- readRDS("inputs//anon_module_g2.rdata")

df_data_h1 <- readRDS("inputs//anon_module_h1.rdata")

df_data_h2 <- readRDS("inputs//anon_module_h2.rdata")

df_data_h3 <- readRDS("inputs//anon_module_h3.rdata")

df_data_j <- readRDS("inputs//anon_module_j.rdata")

df_data_k <- readRDS("inputs//anon_module_k.rdata")

df_data_s <- readRDS("inputs//anon_module_s.rdata")

df_data_w <- readRDS("inputs//anon_module_w.rdata")

df_data_x <- readRDS("inputs//anon_module_x.rdata")


# Taking care of the loops

# df_data_b

df_data_b <- read.csv(file = "inputs/df_data_b.csv")

df_data_b_with_location <- read.csv(file = "inputs/df_data_a.csv") %>%
  select(a5b, a2, hh_type, uhhidfs) %>% 
  left_join(df_data_b, by = "uhhidfs")
write.csv(x = df_data_b_with_location, file = "outputs/df_data_b_with_location.csv")

 


# Economic activity (per hh member) ---------------------------------------
refugee_econ_activity_per_hh_member <- df_data_b_with_location %>%
  filter(!is.na(b12), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement","Omugo_Rhino_ext_settlement", 
                                  "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(a5b, b12) %>% 
  summarise(hh_member_engaged_in_econ_activity = n())


host_econ_activity_per_hh_member <- df_data_b_with_location %>% 
  filter(!is.na(b12), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>% 
  group_by(a2, b12) %>% 
  summarise(hh_member_engaged_in_econ_activity = n())
    

    





