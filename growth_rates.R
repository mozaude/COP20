#### TX_CURR TARGET SETTING: Growth Rates
#### Author: Randy Yee (pcx5@cdc.gov)
#### Date 1/29/2020
#### Inputs: Province_Targets, Province_PLHIV, Province_AgeSex, 
####         District_txcurrR, District_txcurrT, District_PLHIV, District_IPWeights
#### Comments: DO NOT change headers of the input file! 
####           REMOVE all pronunciation marks from data (i.e. provinicia, Zambezia, Manhica)!



##########################################################################################
############################### Calculate Growth Rates ###################################
##########################################################################################

growth_calculator <- function(prev_targets, new_targets, disagg){
  
  target_compare <- merge(prev_targets, new_targets, by = disagg) %>%
    mutate(growth = (agesex_new_target - txcurr_fy20_target)/txcurr_fy20_target) %>%
    select(province, district, age, sex, txcurr_fy20_target, agesex_new_target, growth)
  
  
  return(target_compare)
  
  
}