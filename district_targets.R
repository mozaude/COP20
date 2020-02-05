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

province_target_df <- COP19_Province_Targets
district_previous_target <- District_TX_CurrT_FY19Q4
district_plhiv <- District_PLHIV


#### Function

targetsetter <- function(province_target_df, district_previous_target, district_plhiv){
  
  #### 1) Compare Previous Quarter TX_CURR to PLHIV
  
  ## a) Group TX_Curr results for district top line
  District_TXCURR_Summary <- District_TX_CurrR_FY19Q4 %>% 
    group_by(district) %>% 
    summarise(txcurr_result = sum(value, 
                                  na.rm = T)) %>% 
    ungroup
  
  ## b) Merge TX_Curr previous targets
  District_TXCURR_Summary <- merge(District_TXCURR_Summary, 
                                   district_previous_target, 
                                   by.x = "district",
                                   by.y = "psnu")
  
  ## c) If TX_CURR Result >= PLHIV, use TX_CURR as target
  TXCURR_PLHIV_Compare <- merge(district_plhiv, 
                                District_TXCURR_Summary, 
                                by = "district") %>%
    filter(!is.na(plhiv)) %>% # Filter out Military Mozambique
    select(c(province, 
             district, 
             plhiv, 
             txcurr_result,
             txcurr_target)) %>%
    mutate(new_district_target = ifelse(txcurr_result >= plhiv,
                                txcurr_result, 
                                NA))
  
  #### 2) Calculate unallocated remainder for each province
  province_summary <- TXCURR_PLHIV_Compare %>% 
    group_by(province) %>% 
    summarise(allocated = sum(new_district_target,
                              na.rm = T)) %>%
    ungroup()
  
  remainder <- merge(province_summary, 
                     province_target_df, # NOTE: province_target_df used here
                     by = "province") %>%
    mutate(remainder = target - allocated) 
  
  ## 1) Checks
  cat("\nTotal TX_CURR target:", sum(remainder$allocated) + sum(remainder$remainder))
  cat("\nTX_CURR targets allocated:", sum(remainder$allocated))
  cat("\nRemaining TX_CURR targets to be allocated:", sum(remainder$remainder))
  
  #### 3) Allocate targets for unallocated districts weighted by unmet need
  ## a) Calculate unmet need weights for unallocated districts by province
  unallocated_districts <- TXCURR_PLHIV_Compare %>%
    filter(is.na(new_district_target))
  
  ## b) Unmet denominator
  district_weight_d <- unallocated_districts %>%
    group_by(province) %>%
    summarise(weight_d = sum(plhiv - txcurr_result)) %>%
    ungroup()
  
  unallocated_districts <- merge(unallocated_districts,
                                 district_weight_d, 
                                 by = "province")
  
  ## c) Unmet calculation (plhiv - txcurr_result)/sum_all(plhiv - txcurr_result)
  District_Weights <- merge(unallocated_districts, 
                            remainder, 
                            by = "province") %>%
    mutate(unmet = (plhiv - txcurr_result)/weight_d,
           new_district_target = unmet*remainder,
           coverage = new_district_target/plhiv)
  
  ##################################################################################
  ## d-alt) If coverage > 100% cap target to plhiv remove from District_Weights, remainder back to province level, and reallocate to the rest
  newlyAllocated_districts <- filter(District_Weights, coverage >= 1) %>%
    mutate(new_district_target = plhiv,
           coverage = new_district_target/plhiv) %>%
    select(province, district, plhiv, txcurr_result, txcurr_target, new_district_target)
  
  ## d-alt) Filter out the districts w/ new capped allocations from the comparison dataset
  newlyAllocated_list <- unique(newlyAllocated_districts$district)
  
  TXCURR_PLHIV_Compare2 <- filter(TXCURR_PLHIV_Compare, !district %in% newlyAllocated_list)
  
  TXCURR_PLHIV_Compare3 <- rbind(TXCURR_PLHIV_Compare2, newlyAllocated_districts) 
  
  ## d-alt) Calculate unallocated remainder for each province
  province_summary2 <- TXCURR_PLHIV_Compare3 %>% 
    group_by(province) %>% 
    summarise(allocated = sum(new_district_target,
                              na.rm = T)) %>%
    ungroup()
  
  remainder2 <- merge(province_summary2, 
                     province_target_df, # NOTE: province_target_df used here
                     by = "province") %>%
    mutate(remainder = target - allocated)
  
  
  ## d-alt) Calculate unmet need weights for unallocated districts by province
  unallocated_districts2 <- TXCURR_PLHIV_Compare3 %>%
    filter(is.na(new_district_target))
  
  ## d-alt) Unmet denominator
  district_weight_d2 <- unallocated_districts2 %>%
    group_by(province) %>%
    summarise(weight_d = sum(plhiv - txcurr_result)) %>%
    ungroup()
  
  unallocated_districts2 <- merge(unallocated_districts2,
                                  district_weight_d2, 
                                 by = "province")
  
  ## d-alt) Unmet calculation (plhiv - txcurr_result)/sum_all(plhiv - txcurr_result)
  District_Weights2 <- merge(unallocated_districts2, 
                             remainder2, 
                            by = "province") %>%
    mutate(unmet = (plhiv - txcurr_result)/weight_d,
           new_district_target = unmet*remainder,
           coverage = new_district_target/plhiv)
  
  ## d-alt) Check new allocation vs remainder total
  cat("\nDo newly allocated district targets match province remainder?", 
      sum(remainder2$remainder) == sum(District_Weights2$new_district_target))
  
  ## d-alt) Append the newly allocated districts to the district target set
  allocated_districts2 <- filter(TXCURR_PLHIV_Compare3, !is.na(new_district_target)) %>%
    mutate(coverage = new_district_target/plhiv)
  
  District_Targets_Revised2 <- rbind(allocated_districts2, 
                                    District_Weights2[,c("province", 
                                                        "district", 
                                                        "plhiv", 
                                                        "txcurr_result", 
                                                        "txcurr_target",
                                                        "new_district_target",
                                                        "coverage")])
  
  #### 4) Check revised district targets match province input totals
  Province_Targets_Revised2 <- District_Targets_Revised2 %>% 
    group_by(province) %>% 
    summarise(target_total = sum(new_district_target)) %>%
    ungroup()
  
  cat("\nDo revised targets match COP target total?", 
      sum(remainder2$allocated, na.rm = T) + sum(remainder2$remainder, na.rm = T) == sum(Province_Targets_Revised2$target_total))
  ##################################################################################
  
  ## d) Check new allocation vs remainder total
  cat("\nDo newly allocated district targets match province remainder?", 
      sum(remainder$remainder) == sum(District_Weights$new_district_target))
  
  ## e) Append the newly allocated districts to the district target set
  allocated_districts <- filter(TXCURR_PLHIV_Compare, !is.na(new_district_target)) %>%
    mutate(coverage = new_district_target/plhiv)
  
  District_Targets_Revised <- rbind(allocated_districts, 
                                    District_Weights[,c("province", 
                                                        "district", 
                                                        "plhiv", 
                                                        "txcurr_result", 
                                                        "txcurr_target",
                                                        "new_district_target",
                                                        "coverage")])
  
  #### 4) Check revised district targets match province input totals
  Province_Targets_Revised <- District_Targets_Revised %>% 
    group_by(province) %>% 
    summarise(target_total = sum(new_district_target)) %>%
    ungroup()
  
  cat("\nDo revised targets match COP target total?", 
      sum(remainder$allocated, na.rm = T) + sum(remainder$remainder, na.rm = T) == sum(Province_Targets_Revised$target_total))
  
  return(District_Targets_Revised)
}
