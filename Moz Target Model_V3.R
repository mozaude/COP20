#### TX_CURR TARGET SETTING MAIN V3
#### Author: Randy Yee (pcx5@cdc.gov)
#### Date 2/12/2020
#### Inputs: Province_Targets, Province_PLHIV, Province_AgeSex, 
####         District_txcurrR, District_txcurrT, District_PLHIV, District_IPWeights
#### Comments: DO NOT change headers of the input file! 
####           REMOVE all pronunciation marks from data (i.e. provinicia, Zambezia, Manhica)!
####           Maputo should be Maputo Provincia; Cidade de Maputo should be Maputo Cidade

## TODO: Remove unused tabs from input files
## TODO: Change SNU2 to PSNU

######################################################################################
########################################## MAIN ######################################
######################################################################################
rm(list = ls())
cat("\014")


#### Libraries
library(tidyverse)
library(readxl)
library(openxlsx)

source("spell_check.R")


#### Set File Paths
datapack_data <- "Data Pack_Mozambique_20200120225201_2.11.20 PLHIV_HTS_PMTCT_0815hrs.xlsx"
worksheet <- "TX_CURR Targets_PSNU_age_2.11.20.xlsx"


#### Import & Reformat FY21 SNU2-Age-Sex Targets ------ Source: TX_CURR Worksheet
## Columns ONLY: snu1, age, sex, FY21 Revised Targets
FY21_Targets_SNU1_Import <- read_excel(path=worksheet, 
                             sheet = "Age_SexGrowth_FY21",
                             range = "A2:U266")
colnames(FY21_Targets_SNU1_Import) <- tolower(colnames(FY21_Targets_SNU1_Import))
FY21_Targets_SNU1_Import <- FY21_Targets_SNU1_Import %>%
  select("snu1", 
         "age" = "ageasentered",
         "sex",
         "revised_txcurr_t_fy21_snu1_age_sex" = "revised fy21 target (adjusted by growth)"
         )%>%
  mutate(age = recode(age, "LessThan1" = "<1"))
FY21_Targets_SNU1_Import <- moz_recode(FY21_Targets_SNU1_Import)


#### Import & Reformat Datapack PLHIV ------ Source: Datapack
## Columns ONLY: snu1, snu2, age, sex, plhiv
PLHIV_Source <- read_excel(path=datapack_data, 
                             sheet = "Epi Cascade I",
                             range = "A14:Q3902")
colnames(PLHIV_Source) <- tolower(colnames(PLHIV_Source))
PLHIV_Source <- PLHIV_Source %>%
  select("snu1", 
         "snu2",
         "age",
         "sex",
         "plhiv" = "plhiv.na.age/sex/hivstatus.t")
PLHIV_Source <- moz_recode(PLHIV_Source)


#### Import & Reformat TXCURR FY19 Results & TX CURR FY20 Target ------ Source: Datapack
## Columns ONLY: snu1, snu2, age, sex, TXCURR FY19 Results, TXCURR FY20 Targets
FY20_Target_Datapack <- read_excel(path=datapack_data, 
                             sheet = "TX",
                             range = "A14:M3902")
colnames(FY20_Target_Datapack) <- tolower(colnames(FY20_Target_Datapack))
FY20_Target_Datapack <- FY20_Target_Datapack %>%
  select("snu1", 
         "snu2" = "psnu",
         "age",
         "sex",
         "txcurr_r_fy19_snu2_age_sex" = "tx_curr.n.age_sex_hivstatus.r",
         "txcurr_t_fy20_snu2_age_sex" = "tx_curr.n.age_sex_hivstatus.t_1")
FY20_Target_Datapack$snu2 <- gsub("\\[.*$", "", FY20_Target_Datapack$snu2)
FY20_Target_Datapack$snu2 <- trimws(FY20_Target_Datapack$snu2, which = "right")
FY20_Target_Datapack <- moz_recode(FY20_Target_Datapack) 



###################################################################################################################################################################
##########################################################################SNU1 Allocation##########################################################################
###################################################################################################################################################################

## a) Merge PLHIV, FY19 TX CURR, and FY20 Targets (to be adjusted)
a_tx_master <- merge(PLHIV_Source, 
                      FY20_Target_Datapack[,c("snu2","age", "sex", "txcurr_r_fy19_snu2_age_sex")], 
                     by = c("snu2", "age", "sex"))

## b) Aggregate up to SNU1: Get txcurr_r_fy19_snu1_age_sex
a_tx_snu1_master <- a_tx_master %>% 
  group_by(snu1, age, sex) %>% 
  summarise(txcurr_r_fy19_snu1_age_sex = sum(txcurr_r_fy19_snu2_age_sex)) %>%
  ungroup()

a_tx_snu1_master2 <- merge(a_tx_snu1_master, 
                      FY21_Targets_SNU1_Import,
                      by = c("snu1", "age", "sex")) %>%
  mutate(apr19_to_fy21_netnew = revised_txcurr_t_fy21_snu1_age_sex - txcurr_r_fy19_snu1_age_sex)


apr19_to_fy21_netnew_d <- a_tx_snu1_master2 %>% 
  group_by(snu1) %>% 
  summarise(apr19_to_fy21_netnew_d = sum(apr19_to_fy21_netnew, na.rm = T), # Net New Weight Denominator
            txcurr_r_fy19_snu1 = sum(txcurr_r_fy19_snu1_age_sex, na.rm =T)) %>% #SNU1 TXCURR R Total FY19
  ungroup()


a_tx_snu1_master3 <- merge(a_tx_snu1_master2, 
                           apr19_to_fy21_netnew_d, 
                           by = "snu1") %>%
  mutate(apr19_to_fy21_netnew_weight = apr19_to_fy21_netnew/apr19_to_fy21_netnew_d)


FY20_Target_Datapack_SNU1 <- FY20_Target_Datapack %>% 
  group_by(snu1) %>% 
  summarise(txcurr_t_fy20_snu1 = sum(txcurr_t_fy20_snu2_age_sex, na.rm = T)) %>% 
  ungroup()

a_tx_snu1_master4 <- merge(a_tx_snu1_master3, 
                           FY20_Target_Datapack_SNU1, 
                           by = c("snu1")) %>%
  mutate(netnew_t_fy20_snu1 = txcurr_t_fy20_snu1 - txcurr_r_fy19_snu1,
         revised_netnew_t_fy20_snu1_age_sex = netnew_t_fy20_snu1 * apr19_to_fy21_netnew_weight,
         revised_txcurr_t_fy20_snu1_age_sex = revised_netnew_t_fy20_snu1_age_sex + txcurr_r_fy19_snu1_age_sex) %>%
  select(snu1, age, sex, 
         txcurr_r_fy19_snu1_age_sex,
         apr19_to_fy21_netnew, apr19_to_fy21_netnew_d, apr19_to_fy21_netnew_weight,
         txcurr_r_fy19_snu1,
         txcurr_t_fy20_snu1,
         revised_netnew_t_fy20_snu1_age_sex,
         revised_txcurr_t_fy20_snu1_age_sex,
         revised_txcurr_t_fy21_snu1_age_sex)



###################################################################################################################################################################
##########################################################################SNU2 Allocation##########################################################################
###################################################################################################################################################################
## a) If TX_CURR Result >= PLHIV, use TX_CURR Result as target
z_Final_Targets_a <- merge(a_tx_master, 
                           a_tx_snu1_master4, 
                           by = c("snu1", "age", "sex"))  %>%
  select(c(snu1, 
           snu2,
           age,
           sex,
           plhiv,
           
           txcurr_r_fy19_snu2_age_sex, # SNU2-Age-Sex TX CURR Results FY19APR
           txcurr_r_fy19_snu1_age_sex, # SNU1-Age-Sex TX CURR Results FY19APR
           
           revised_txcurr_t_fy20_snu1_age_sex, #SNU1-Age-Sex Target FY20
           revised_txcurr_t_fy21_snu1_age_sex, #SNU1-Age-Sex Target FY21
           
           revised_netnew_t_fy20_snu1_age_sex) # SNU1-Age-Sex NET_NEW Target FY20

         ) %>%
  mutate(
         revised_netnew_t_fy21_snu1_age_sex = revised_txcurr_t_fy21_snu1_age_sex - txcurr_r_fy19_snu1_age_sex, # SNU1-Age-Sex NET_NEW Target FY21
         
         unmet_need = plhiv - txcurr_r_fy19_snu2_age_sex, # Difference between PLHIV and TXCURR_R
         unmet_need_binary = ifelse((plhiv - txcurr_r_fy19_snu2_age_sex) <= 0, 0, 1)) #0 = exceed plhiv, no unmet need; 1 = has unmet need


z_Final_Targets_b <- z_Final_Targets_a %>%
  filter(unmet_need_binary == 1) %>%
  group_by(snu1, 
           age, 
           sex) %>% 
  summarise(unmet_d = sum(unmet_need, na.rm = T)) %>% # Unmet need denominator
  ungroup()


z_Final_Targets_c <- merge(z_Final_Targets_a, 
                           z_Final_Targets_b, 
                           by = c("snu1", "age", "sex")) %>%
  mutate(unmet_weight = unmet_need/unmet_d, # Unmet need weight
         
         fy20_net_new_target = ifelse(unmet_need_binary==0, txcurr_r_fy19_snu2_age_sex, unmet_weight * revised_netnew_t_fy20_snu1_age_sex), # TX_Net_New Target SNU2-Age-Sex FY20
         fy21_net_new_target = ifelse(unmet_need_binary==0, txcurr_r_fy19_snu2_age_sex, unmet_weight * revised_netnew_t_fy21_snu1_age_sex), # TX_Net_New Target SNU2-Age-Sex FY21
         
         fy20_tx_curr_target = ifelse(unmet_need_binary==0, txcurr_r_fy19_snu2_age_sex, fy20_net_new_target + txcurr_r_fy19_snu2_age_sex),  # New TXCURR Target FY20
         fy21_tx_curr_target = ifelse(unmet_need_binary==0, txcurr_r_fy19_snu2_age_sex, fy21_net_new_target + txcurr_r_fy19_snu2_age_sex),  # New TXCURR Target FY21
         
         fy20_art_coverage = fy20_tx_curr_target/plhiv, # New target coverage FY20
         fy21_art_coverage = fy21_tx_curr_target/plhiv, # New target coverage FY21
         
         fy20_fy21_growth = (fy21_tx_curr_target-fy20_tx_curr_target)/fy20_tx_curr_target
         )

  

sum(z_Final_Targets_c$fy20_tx_curr_target, na.rm = T)
sum(z_Final_Targets_c$fy21_tx_curr_target, na.rm = T)

openxlsx::write.xlsx(z_Final_Targets_c, "Mozambique_Datapack_TX.xlsx")
