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
    mutate(ip_type = ifelse(str_detect(attribute, "PEPFAR"), "ip", "moh")) %>%
    group_by(district, ip_type) %>% 
    summarise(district_PEPFAR_txcurrR = sum(value, 
                                            na.rm = T)) %>% 
    ungroup
  
  # Filter just PEPFAR for distribution, calculate weights for each phase
  District_TXCURR_Summary_PEPFAR <- filter(District_TXCURR_Summary, 
                                           ip_type == "ip")
  
  District_TX_CurrR_FY19Q4_PEPFAR <- filter(District_TX_CurrR_FY19Q4,
                                            str_detect(attribute, "PEPFAR"))
  
  District_PEPFAR_AJUDAWeight <- merge(District_TXCURR_Summary_PEPFAR, 
                                       District_TX_CurrR_FY19Q4_PEPFAR, 
                                       by = "district") %>%
    mutate(phase_weight = value/district_PEPFAR_txcurrR)
  
  
  # Filter just PEPFAR for distribution, calculate weights for each phase (weight should be 1)
  District_TXCURR_Summary_MISAU <- filter(District_TXCURR_Summary, 
                                          ip_type == "moh")
  
  District_TX_CurrR_FY19Q4_MISAU <- filter(District_TX_CurrR_FY19Q4,
                                           str_detect(attribute, "MISAU"))
  
  District_MISAU_AJUDAWeight <- merge(District_TXCURR_Summary_MISAU, 
                                      District_TX_CurrR_FY19Q4_MISAU, 
                                      by = "district") %>%
    mutate(phase_weight = value/district_PEPFAR_txcurrR)
  
  # Append PEPFAR and MISAU phase weights
  AJUDA_Misau_TX_CURR <- rbind(District_MISAU_AJUDAWeight, District_PEPFAR_AJUDAWeight)
  
  catagoryoptioncombo_IP_AJUDA <- merge(catagoryoptioncombo_IP, 
                                        AJUDA_Misau_TX_CURR[, c("district", "attribute", "ip_type", "phase_weight")], 
                                        by = c("district", "ip_type")) %>%
    mutate(phase_alloc = ip_alloc*phase_weight)
  
  ## d) Check
  catagoryoptioncombo_IP_AJUDA %>% 
    group_by(province, district) %>% 
    summarise(total = sum(phase_weight))
  
  cat("\nDoes Phase allocation match COP target?", sum(catagoryoptioncombo_IP_AJUDA$phase_alloc, na.rm = T))
  
  ## e) + Age-Sex Weights
  Age_Sex_Targets_Revised <- agesex_allocate(District_Targets_Revised)
  
  catagoryoptioncombo_IP_AJUDA_AGESEX <- merge(catagoryoptioncombo_IP_AJUDA, 
                                               Age_Sex_Targets_Revised[,c("district", "age", "sex", "value")],
                                               by = "district",
                                               all = T)%>%
    select(c("province", 
             "district",
             "attribute",
             "ip_type",
             "age",
             "sex",
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
    group_by(province, district, ip_type) %>% 
    summarise(total = sum(agesex_weight))
  
  cat("\nDoes Age-Sex allocation match COP target?", sum(catagoryoptioncombo_IP_AJUDA_AGESEX$agesex_alloc, na.rm = T))
  
  #### 2) Final (probability) Tree Allocation Dataset
  final <- catagoryoptioncombo_IP_AJUDA_AGESEX %>% 
    mutate(final_target = new_target*ip_weight*phase_weight*agesex_weight)
  
  ## a) Checks
  cat("\nDoes linked allocation match COP target?", sum(final$final_target, na.rm = T))
  
  
  return(final)
  
}
