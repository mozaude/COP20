#### TX_CURR TARGET SETTING: District Target-Setting
#### Author: Randy Yee (pcx5@cdc.gov)
#### Date 1/29/2020
#### Inputs: Province_Targets, Province_PLHIV, Province_AgeSex, 
####         District_txcurrR, District_txcurrT, District_PLHIV, District_IPWeights
#### Comments: DO NOT change headers of the input file! 
####           REMOVE all pronunciation marks from data (i.e. provinicia, Zambezia, Manhica)!



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