#### TX_CURR TARGET SETTING
#### Author: Randy Yee (pcx5@cdc.gov)
#### Date 1/29/2020
#### Inputs: Province_Targets, Province_PLHIV, Province_AgeSex, 
####         District_txcurrR, District_txcurrT, District_PLHIV, District_IPWeights
#### Comments: DO NOT change headers of the input file! 
####           REMOVE all pronunciation marks from data (i.e. provinicia, Zambezia, Manhica)!


####################################################################################
######################## Algorithm: TX_CURR TARGET SETTING ######################### 
# procedure
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


####################################################################################
################################ Libraries #########################################
####################################################################################
library(tidyverse)
library(readxl)
library(openxlsx)



####################################################################################
################################ Import XLSX Inputs ################################
####################################################################################

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
OU_TXCURR_Target_COP19 <- Province_Targets[[which(Province_Targets$province=="TOTAL"),3]]
OU_TXCURR_Target_COP20 <- Province_Targets[[which(Province_Targets$province=="TOTAL"),2]]
cat("TX_CURR COP19 Target is", OU_TXCURR_Target_COP19)
cat("TX_CURR COP20 Target is", OU_TXCURR_Target_COP20)




##########################################################################################
################################ Extract Province Targets ################################
##########################################################################################

## DF format: col1 = province; col2 = target

COP19_Province_Targets <- Province_Targets %>% select(c("province", "target" = "fy20cop19targetsadj"))
COP20_Province_Targets <- Province_Targets %>% select(c("province", "target" = "fy21cop20targetsadj"))



##########################################################################################
################################ Allocate District Targets ###############################
##########################################################################################

#### Function

targetsetter <- function(province_target_df){

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
  
  #### 2) Calculate unallocated remainder for each province ########## NOTE: province_target_df used here
  province_summary <- TXCURR_PLHIV_Compare %>% 
    group_by(province) %>% 
    summarise(allocated = sum(new_target,
                              na.rm = T)) %>%
    ungroup()
  
  remainder <- merge(province_summary, 
                     province_target_df, 
                     by = "province") %>%
    mutate(remainder = target - allocated) 
  
  ## 1) Checks
  cat("\nTotal TX_CURR target:", sum(remainder$allocated)+sum(remainder$remainder))
  cat("\nTX_CURR targets allocated:", sum(remainder$allocated))
  cat("\nRemaining TX_CURR targets to be allocated:", sum(remainder$remainder))
  
  #### 3) Allocate targets for unallocated districts weighted by unmet need
  ## a) Calculate unmet need weights for unallocated districts by province
  unallocated_districts <- TXCURR_PLHIV_Compare %>%
    filter(is.na(new_target))
  
  ## b) Unmet denominator
  district_weight_d <- unallocated_districts %>%
    group_by(province) %>%
    summarise(weight_d = sum(plhiv - txcurr_result)) %>%
    ungroup()
  
  unallocated_districts <- merge(unallocated_districts,
                                 district_weight_d, 
                                 by = "province")
  
  ## c) Unmet calculation (district_plhiv - tx_currR)/sum_all(district_plhiv - tx_currR)
  District_Weights <- merge(unallocated_districts, 
                                 remainder, 
                                 by = "province") %>%
    mutate(unmet = (plhiv - txcurr_result)/weight_d,
           new_target = unmet*remainder) 
  
  ## d) Check new allocation to remainder total
  cat("\nDo newly allocated district targets match province remainder?", 
      sum(remainder$remainder) == sum(District_Weights$new_target))
  
  ## e) Append the newly allocated districts to the district target set
  allocated_districts <- filter(TXCURR_PLHIV_Compare, !is.na(new_target))
  
  District_Targets_Revised <- rbind(allocated_districts, 
                                    District_Weights[,c("province", 
                                                        "district", 
                                                        "plhiv", 
                                                        "new_target", 
                                                        "txcurr_result", 
                                                        "txcurr_target")])
  
  #### 4) Check revised district targets match province input totals
  Province_Targets_Revised <- District_Targets_Revised %>% 
    group_by(province) %>% 
    summarise(target_total = sum(new_target)) %>%
    ungroup()
  
  cat("\nDo revised targets match COP target total?", 
      sum(remainder$allocated) + sum(remainder$remainder) == sum(Province_Targets_Revised$target_total))
  
  return(District_Targets_Revised)
}
  


##########################################################################################
#################### Allocate District Sub-Group Targets (Groups) ########################
##########################################################################################

#### #### #### #### #### 
#### 1) IP allocation ## 
#### #### #### #### #### 

#### Function

ip_allocate <- function(District_Targets_Revised){
  
  ## a) Apply weights to District_Targets_Revised
  IP_Targets_Revised <- merge(District_Targets_Revised, 
                              District_IPWeights[,c("district", "moh", "ip")], 
                              by = "district") %>%
    mutate(moh_new_target = new_target*moh,
           ip_new_target = new_target*ip)
  
  ## b) Checks
  cat("\nDoes IP allocation match COP target?", sum(IP_Targets_Revised$moh_new_target) + sum(IP_Targets_Revised$ip_new_target))
  
  return(IP_Targets_Revised)
}



#### #### #### #### #### #### #### 
#### 2) AJUDA phase allocation ### 
#### #### #### #### #### #### #### 

#### Function

phase_allocate <- function(IP_Targets_Revised){
  
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
  cat("\nDoes AJUDA allocation match COP target?", sum(IP_Targets_Revised$moh_new_target) + sum(AJUDA_Targets_Revised$AJUDA_new_target))
  
  return(AJUDA_Targets_Revised)
  
}



#### #### #### #### #### #### 
#### 3) Age-Sex allocation ##
#### #### #### #### #### #### 

#### Function

agesex_allocate <- function(District_Targets_Revised){

  ## a) Use province distributions for district distributions
  Age_Sex_Targets_Revised <- merge(District_Targets_Revised,
                                   Province_AgeSex,
                                   by = "province") %>%
    mutate(agesex_new_target = new_target*value)
  
  ## b) Checks
  cat("\nDoes Age-Sex allocation match COP target?", sum(Age_Sex_Targets_Revised$agesex_new_target))
  
  return(Age_Sex_Targets_Revised)

}



##########################################################################################
##################### Allocate District Sub-Group Targets (Tree) #########################
##########################################################################################
## 1) IP (MOH, PEPFAR)
## 2) AJUDA Phase (PEPFAR Only)
## 3) Age-Sex (PEPFAR Only)

#### #### #### #### #### 
#### 1) Tree Model #####
#### #### #### #### #### 

#### Function

treemodel <- function(District_Targets_Revised){

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
  
  cat("Does IP allocation match COP target?", sum(catagoryoptioncombo_IP$ip_alloc, na.rm = T))
  
  ## c) + AJUDA Weights
  District_TXCURR_Summary <- District_TX_CurrR_FY19Q4 %>% 
    group_by(district) %>% 
    summarise(txcurr_result = sum(value, 
                                  na.rm = T)) %>% 
    ungroup
  
  District_TXCURR_Summary <- merge(District_TXCURR_Summary, 
                                   District_TX_CurrT_FY19Q4, 
                                   by.x = "district",
                                   by.y = "psnu")
  
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
  
  cat("\nDoes Phase allocation match COP target?", sum(catagoryoptioncombo_IP_AJUDA$phase_alloc, na.rm = T))
  
  ## e) + Age-Sex Weights
  Age_Sex_Targets_Revised <- agesex_allocate(District_Targets_Revised)
  
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
  
  cat("\nDoes Age-Sex allocation match COP target?", sum(catagoryoptioncombo_IP_AJUDA_AGESEX$agesex_alloc, na.rm = T))
  
  #### 2) Final (probability) Tree Allocation Dataset
  final <- catagoryoptioncombo_IP_AJUDA_AGESEX %>% 
    mutate(final_target = new_target*ip_weight*phase_weight*agesex_weight)
  
  ## a) Checks
  cat("\nDoes linked allocation match COP target?", sum(final$final_target, na.rm = T))

  return(final)
  
}