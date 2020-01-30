#### TX_CURR TARGET SETTING: Tree Allocation
#### Author: Randy Yee (pcx5@cdc.gov)
#### Date 1/29/2020
#### Inputs: Province_Targets, Province_PLHIV, Province_AgeSex, 
####         District_txcurrR, District_txcurrT, District_PLHIV, District_IPWeights
#### Comments: DO NOT change headers of the input file! 
####           REMOVE all pronunciation marks from data (i.e. provinicia, Zambezia, Manhica)!



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