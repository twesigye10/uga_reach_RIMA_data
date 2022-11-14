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

 #df_data_h1

df_data_h1 <- read.csv(file = "inputs/df_data_h1.csv")

df_data_h1_with_location <- read.csv(file = "inputs/df_data_a.csv") %>% 
  select(a5b, a2, hh_type, uhhidfs) %>% 
  left_join(df_data_h1, by = "uhhidfs")
write.csv(x = df_data_h1_with_location, file = "outputs/df_data_h1_with_location.csv")

# df_data_h2

df_data_h2 <- read.csv(file = "inputs/df_data_h2.csv")

df_data_h2_with_location <- read.csv(file = "inputs/df_data_a.csv") %>% 
  select(a5b, a2, hh_type, uhhidfs) %>% 
  left_join(df_data_h2, by = "uhhidfs")
write.csv(x = df_data_h2_with_location, file = "outputs/df_data_h2_with_location.csv")

# df_data_h3

df_data_h3 <- read.csv(file = "inputs/df_data_h3.csv")

df_data_h3_with_location <- read.csv(file = "inputs/df_data_a.csv") %>% 
  select(a5b, a2, hh_type, uhhidfs) %>% 
  left_join(df_data_h3, by = "uhhidfs")
write.csv(x = df_data_h3_with_location, file = "outputs/df_data_h3_with_location.csv")

# df_data_w

df_data_w <- read.csv(file = "inputs/df_data_w.csv")

df_data_w_with_location <- read.csv(file = "inputs/df_data_a.csv") %>% 
  select(a5b, a2, hh_type, uhhidfs) %>% 
  left_join(df_data_w, by = "uhhidfs")
write.csv(x = df_data_w_with_location, file = "outputs/df_data_w_with_location.csv")


# Economic activity (per hh member) ---------------------------------------
# refugee
refugee_econ_activity_per_hh_member <- df_data_b_with_location %>%
  filter(!is.na(b12), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>% 
  group_by(a5b, b12) %>% 
  summarise(hh_member_engaged_in_econ_activity = n())
write.csv(x = refugee_econ_activity_per_hh_member, file = "outputs/refugee_econ_activity_per_hh_member.csv" )

# host
host_econ_activity_per_hh_member <- df_data_b_with_location %>% 
  filter(!is.na(b12), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>% 
  group_by(a2, b12) %>% 
  summarise(hh_member_engaged_in_econ_activity = n())
write.csv(x = host_econ_activity_per_hh_member, file = "outputs/host_econ_activity_per_hh_member.csv" )


#Income from renting/selling assets
# refugee
# agricultural_land
refugee_income_from_agric_land <- df_data_h3_with_location %>% 
  filter(!is.na(h6_1), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>%
  group_by(a5b, h6_1) %>% 
  summarise(income_from_agric_land = n())
write.csv(x = refugee_income_from_agric_land, file = "outputs/refugee_income_from_agric_land.csv")
  
# apartments_house_shops_stores
refugee_income_from_apartments_house_shops_store <- df_data_h3_with_location %>% 
  filter(!is.na(h6_2), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>%
  group_by(a5b, h6_2) %>% 
  summarise(income_from_apartments_house_shops_store = n())
write.csv(x = refugee_income_from_apartments_house_shops_store, file = "outputs/refugee_income_from_apartments_house_shops_store.csv")

# car_truck_tractor_cart
refugee_income_from_car_truck_tractor_cart <- df_data_h3_with_location %>% 
  filter(!is.na(h6_3), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>%
  group_by(a5b, h6_3) %>% 
  summarise(income_from_car_truck_tractor_cart = n())
write.csv(x = refugee_income_from_car_truck_tractor_cart, file = "outputs/refugee_income_from_car_truck_tractor_cart.csv")

# livestock
refugee_income_from_livestock <- df_data_h3_with_location %>% 
  filter(!is.na(h6_4), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>%
  group_by(a5b, h6_4) %>% 
  summarise(income_from_livestock = n())
write.csv(x = refugee_income_from_livestock, file = "outputs/refugee_income_from_livestock.csv")

# sale_of_house
refugee_income_from_sale_house <- df_data_h3_with_location %>% 
  filter(!is.na(h6_5), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>%
  group_by(a5b, h6_5) %>% 
  summarise(income_from_sale_house = n())
write.csv(x = refugee_income_from_sale_house, file = "outputs/refugee_income_from_sale_house.csv")

# sale_durable_goods
refugee_income_from_sale_durable_goods <- df_data_h3_with_location %>% 
  filter(!is.na(h6_6), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>%
  group_by(a5b, h6_6) %>% 
  summarise(income_from_sale_durable_goods = n())
write.csv(x = refugee_income_from_sale_durable_goods, file = "outputs/refugee_income_from_sale_durable_goods.csv")

# host

# agricultural_land
host_income_from_agric_land <- df_data_h3_with_location %>% 
  filter(!is.na(h6_1), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>%
  group_by(a2, h6_1) %>% 
  summarise(income_from_agric_land = n())
write.csv(x = host_income_from_agric_land, file = "outputs/host_income_from_agric_land.csv")

# apartments_house_shops_stores
host_income_from_apartments_house_shops_store <- df_data_h3_with_location %>% 
  filter(!is.na(h6_2), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>%
  group_by(a2, h6_2) %>% 
  summarise(income_from_apartments_house_shops_store = n())
write.csv(x = host_income_from_apartments_house_shops_store, file = "outputs/host_income_from_apartments_house_shops_store.csv")

# car_truck_tractor_cart
host_income_from_car_truck_tractor_cart <- df_data_h3_with_location %>% 
  filter(!is.na(h6_3), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>%
  group_by(a2, h6_3) %>% 
  summarise(income_from_car_truck_tractor_cart = n())
write.csv(x = host_income_from_car_truck_tractor_cart, file = "outputs/host_income_from_car_truck_tractor_cart.csv")

# livestock
host_income_from_livestock <- df_data_h3_with_location %>% 
  filter(!is.na(h6_4), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>%
  group_by(a2, h6_4) %>% 
  summarise(income_from_livestock = n())
write.csv(x = host_income_from_livestock, file = "outputs/host_income_from_livestock.csv")

# sale_of_house
host_income_from_sale_house <- df_data_h3_with_location %>% 
  filter(!is.na(h6_5), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>%
  group_by(a2, h6_5) %>% 
  summarise(income_from_sale_house = n())
write.csv(x = host_income_from_sale_house, file = "outputs/host_income_from_sale_house.csv")

# sale_durable_goods
host_income_from_sale_durable_goods <- df_data_h3_with_location %>% 
  filter(!is.na(h6_6), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>%
  group_by(a2, h6_6) %>% 
  summarise(income_from_sale_durable_goods = n())
write.csv(x = host_income_from_sale_durable_goods, file = "outputs/host_income_from_sale_durable_goods.csv")

# income from wage labor 

# refugee
refugee_income_from_wage_labor <- df_data_w_with_location %>% 
  filter(!is.na(w1_4), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>%
  group_by(a5b, w1_4) %>% 
  summarise(income_from_wage_labor = n())
write.csv(x = refugee_income_from_wage_labor, file = "outputs/refugee_income_from_wage_labor.csv")

# host
host_income_from_wage_labor <- df_data_w_with_location %>% 
  filter(!is.na(w1_4), (a2 %in% c("Lamwo", "Arua", "Yumbe","Isingiro") & hh_type == 0)) %>%
  group_by(a2, w1_4) %>% 
  summarise(income_from_wage_labor = n())
write.csv(x = host_income_from_wage_labor, file = "outputs/host_income_from_wage_labor.csv")

# income from casual labor

# refugee
refugee_income_from_casual_labor <- df_data_w_with_location %>% 
  filter(!is.na(w2_4), (a5b %in% c("Bidibidi_settlement", "Nakivale_settlement", "Palabeck_settlement", "Rhinocamp_settlement") & hh_type == 1)) %>%
  group_by(a5b, w2_4) %>% 
  summarise(income_from_casual_labor = n())
write.csv(x = refugee_income_from_casual_labor, file = "outputs/refugee_income_from_casual_labor.csv")





  


