#### TX_CURR TARGET SETTING
#### Author: Randy Yee

####################################################################################
######################## Algorithm: TX_CURR TARGET SETTING ######################### 
# procedure(Province_Targets, Province_PLHIV, Province_AgeSex)
#   COP_target <- input
#   for each district
#     if TX_CURR >= PLHIV
#       target_new <- target_prev
#     else target_new <- NA
#       remainder <- COP_target - allocated targets
#       calculate unallocated unmet_weight then
#       target_new <- remainder * unmet_weight
#   end for
#   calculate IP, AJUDA, AgeSex Weights
#   apply weights to target_new
# end procedure
####################################################################################

################################ Libraries #########################################
library(tidyverse)
library(readxl)
library(openxlsx)

################################ Import XLSX Inputs ################################
#### Set File Paths
xl_data <- "Inputs.xlsx"
tab_names <- excel_sheets(path = xl_data)

#### Import Allocations to Separate Dataframes
for (i in 1:length(tab_names[])){  
  tempdf <- read_excel(path=xl_data, 
                       sheet = i)
  tempdf$sheetname <- tab_names[i]
  colnames(tempdf) <- tolower(colnames(tempdf))
  tempdf <- select(tempdf,-sheetname)
  assign(tab_names[[i]], 
         tempdf)
  rm(tempdf)
} 

#### Get Overall COP Target
OU_TXCURR_Target <- Province_Targets[[which(Province_Targets$province=="TOTAL"),2]]
cat("TX_CURR Target is", OU_TXCURR_Target)

################################ Allocate District Targets ################################
#### 1) Compare Previous Quarter TX_CURR to PLHIV

## a) Group TX_Curr results for district top line
District_TXCURR_Summary <- District_TX_CurrR_FY19Q4 %>% 
  group_by(district) %>% 
  summarise(txcurr_result = sum(value, 
                        na.rm = T)) %>% 
  ungroup

## b) Merge TX_Curr previous targets
District_TXCURR_Summary <- merge(District_TXCURR_Summary, 
                                 District_TX_CurrT_FY19Q4, 
                                 by.x = "district",
                                 by.y = "psnu")

## c) If TX_CURR Result >= PLHIV, target remains the same
TXCURR_PLHIV_Compare <- merge(District_PLHIV, 
                              District_TXCURR_Summary, 
                              by = "district") %>%
  select(c(province, 
           district, 
           plhiv, 
           txcurr_result,
           txcurr_target)) %>%
  mutate(new_target = ifelse(txcurr_result >= plhiv, 
                             txcurr_target, 
                             NA))

#### 2) Calculate unallocated remainder from OU overall target
remainder <- OU_TXCURR_Target - sum(TXCURR_PLHIV_Compare$new_target, 
                                    na.rm = T)
cat("Remaining TX_CURR targets to be allocated:", remainder)

#### 3) Allocate targets for unallocated districts weighted by unmet need
## a) Calculate unmet need weights for unallocated districts
unallocated_districts <- TXCURR_PLHIV_Compare %>%
  filter(is.na(new_target))

unallocated_districts <- unallocated_districts %>%
  mutate(unmet = plhiv/sum(unallocated_districts$plhiv)) %>%
  mutate(new_target = remainder*unmet) %>%
  select(-unmet)

## b) Append the newly allocated districts to the district target set
allocated_districts <- filter(TXCURR_PLHIV_Compare, !is.na(new_target))

District_Targets_Revised <- rbind(allocated_districts, unallocated_districts)

OU_TXCURR_Target == sum(District_Targets_Revised$new_target)

#### 4) Get revised province targets
Province_Targets_Revised <- District_Targets_Revised %>% 
  group_by(province) %>% 
  summarise(target_total = sum(new_target)) %>%
  ungroup()


################################ Allocate District Sub-Group Targets (Groups) ################################
#### 1) IP allocation

## a) Apply weights to District_Targets_Revised
IP_Targets_Revised <- merge(District_Targets_Revised, 
                            District_IPWeights[,c("district", "moh", "ip")], 
                            by = "district") %>%
  mutate(moh_new_target = new_target*moh,
         ip_new_target = new_target*ip)

## b) Checks
cat("Does IP allocation match COP target?", OU_TXCURR_Target == sum(IP_Targets_Revised$moh_new_target) + sum(IP_Targets_Revised$ip_new_target))

#### 2) AJUDA phase allocation

## a) Get weights from previous TX_CURR results 
AJUDA_TXCURR <- District_TX_CurrR_FY19Q4 %>%
  filter(grepl(".PEPFAR.", attribute)) 

AJUDA_District_TXCURR <- AJUDA_TXCURR %>%
  group_by(district) %>%
  summarise(tx_total = sum(value, 
                           na.rm = T)) %>%
  ungroup

AJUDA_District_Weights <- merge(AJUDA_TXCURR, 
                               AJUDA_District_TXCURR, 
                               by = "district",
                               all.x = T) %>%
  mutate(AJUDA_weight = value/tx_total) %>%
  select(c(district,
           attribute,
           value,
           tx_total,
           AJUDA_weight))

## b) Apply weights to District_Targets_Revised
AJUDA_Targets_Revised <- merge(AJUDA_District_Weights,
                               IP_Targets_Revised[ , c("district", "ip_new_target")],
                               by = "district") %>%
  mutate(AJUDA_new_target = ip_new_target*AJUDA_weight)


## c) Checks
cat("Does AJUDA allocation match COP target?", OU_TXCURR_Target == sum(IP_Targets_Revised$moh_new_target) + sum(AJUDA_Targets_Revised$AJUDA_new_target))

#### 3) Age-Sex allocation
## a) Use province distributions for district distributions
Age_Sex_Targets_Revised <- merge(District_Targets_Revised,
                                 Province_AgeSex,
                                 by = "province") %>%
  mutate(agesex_new_target = new_target*value)

## b) Checks
cat("Does Age-Sex allocation match COP target?", OU_TXCURR_Target == sum(Age_Sex_Targets_Revised$agesex_new_target))


################################ Allocate District Sub-Group Targets (Tree) ################################
## 1) IP (MOH, PEPFAR)
## 2) AJUDA Phase (PEPFAR Only)
## 3) Age-Sex (PEPFAR Only)

#### 1) Tree Model
## a) District_Targets_Revised + IP Weights
District_IPWeights_long <- pivot_longer(District_IPWeights, 
                                        cols = c("moh", "ip"), 
                                        names_to = "ip_type") %>%
  select(-province) %>%
  rename(ip_weight = "value")

catagoryoptioncombo_IP <- merge(District_Targets_Revised, 
                             District_IPWeights_long, 
                             by = "district",
                             all.x = T) %>%
  mutate(ip_alloc = new_target*ip_weight)

## b) Checks
catagoryoptioncombo_IP %>% 
  group_by(province, district) %>% 
  summarise(total = sum(ip_weight))

cat("Does IP allocation match COP target?", OU_TXCURR_Target == sum(catagoryoptioncombo_IP$ip_alloc))

## c) + AJUDA Weights
AJUDA_Misau_TX_CURR <- merge(District_TX_CurrR_FY19Q4, 
                             District_TXCURR_Summary, 
                             by = "district") %>%
  mutate(phase_weight = value/txcurr_result)
  

catagoryoptioncombo_IP_AJUDA <- merge(catagoryoptioncombo_IP, 
                                      AJUDA_Misau_TX_CURR[, c("district", "attribute", "phase_weight")], 
                          by = "district") %>%
  mutate(phase_alloc = ip_alloc*phase_weight)

## d) Check
catagoryoptioncombo_IP_AJUDA %>% 
  group_by(province, district, ip_type) %>% 
  summarise(total = sum(phase_weight))

cat("Does Phase allocation match COP target?", OU_TXCURR_Target == sum(catagoryoptioncombo_IP_AJUDA$phase_alloc, na.rm = T))

## e) + Age-Sex Weights
catagoryoptioncombo_IP_AJUDA_AGESEX <- merge(catagoryoptioncombo_IP_AJUDA, 
                                             Age_Sex_Targets_Revised[,c("district", "agesex", "value")],
                                             by = "district",
                                             all = T)%>%
  select(c("province", 
           "district",
           "attribute",
           "ip_type",
           "agesex",
           #"plhiv", 
           #"txcurr_result", 
           #"txcurr_target", 
           "new_target",
           "ip_weight",
           "ip_alloc",
           "phase_weight",
           "phase_alloc",
           "value")) %>%
  rename("agesex_weight" = value) %>%
  mutate(agesex_alloc = phase_alloc*agesex_weight)

## f) Checks
catagoryoptioncombo_IP_AJUDA_AGESEX %>% 
  group_by(province, district, ip_type, attribute) %>% 
  summarise(total = sum(agesex_weight))

cat("Does Age-Sex allocation match COP target?", OU_TXCURR_Target == sum(catagoryoptioncombo_IP_AJUDA_AGESEX$agesex_alloc))

#### 2) Final (probability) Tree Allocation Dataset
final <- catagoryoptioncombo_IP_AJUDA_AGESEX %>% 
  mutate(final_target = new_target*ip_weight*phase_weight*agesex_weight)

## a) Checks
cat("Does linked allocation match COP target?", OU_TXCURR_Target == sum(final$final_target))

## Branches
ip_branches <- catagoryoptioncombo_IP %>% 
  group_by(province, district) %>% 
  summarise(n = n()) %>%
  ungroup()

phase_branches <- catagoryoptioncombo_IP_AJUDA %>% 
  group_by(province, district, ip_type) %>% 
  summarise(n = n())

agesex_branches <- catagoryoptioncombo_IP_AJUDA_AGESEX %>% 
  group_by(province, district, ip_type, attribute) %>% 
  summarise(n = n())

################################ Exports ################################
list_df <- c("Province_Targets_Revised" = Province_Targets_Revised,
             "District_Targets_Revised" = District_Targets_Revised,
             "IP_Targets_Revised" = IP_Targets_Revised,
             "AJUDA_Targets_Revised" = AJUDA_Targets_Revised,
             "Age_Sex_Targets_Revised" = Age_Sex_Targets_Revised,
             "Tree Targets" = final)

write.xlsx(list_df, file = "MozambiqueTargetModel.xlsx")


