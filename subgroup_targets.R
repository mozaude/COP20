#### TX_CURR TARGET SETTING: Subgroup Allocation
#### Author: Randy Yee (pcx5@cdc.gov)
#### Date 1/29/2020
#### Inputs: Province_Targets, Province_PLHIV, Province_AgeSex, 
####         District_txcurrR, District_txcurrT, District_PLHIV, District_IPWeights
#### Comments: DO NOT change headers of the input file! 
####           REMOVE all pronunciation marks from data (i.e. provinicia, Zambezia, Manhica)!



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
  
  Age_Sex_Targets_Revised <- Age_Sex_Targets_Revised %>% 
    separate(agesex, into = c("sex", "age"), sep = 1) %>%
    mutate(sex = recode(sex, 
                        "F" = "Female", 
                        "M" = "Male"),
           age = recode(age, 
                        "_0_1" = "<01",
                        "_1_4" = "01-04",
                        "_5_9" = "05-09",
                        "_10_14" = "10-14",
                        "_15_19" = "15-19",
                        "_20_24" = "20-24",
                        "_25_29" = "25-29",
                        "_30_34" = "30-34",
                        "_35_39" = "35-39",
                        "_40_44" = "40-44",
                        "_45_49" = "45-49",
                        "_50" = "50+"))
  
  ## b) Checks
  cat("\nDoes Age-Sex allocation match COP target?", sum(Age_Sex_Targets_Revised$agesex_new_target))
  
  return(Age_Sex_Targets_Revised)
  
}
