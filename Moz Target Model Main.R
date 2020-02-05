#### TX_CURR TARGET SETTING MAIN
#### Author: Randy Yee (pcx5@cdc.gov)
#### Date 1/29/2020
#### Inputs: Province_Targets, Province_PLHIV, Province_AgeSex, 
####         District_txcurrR, District_txcurrT, District_PLHIV, District_IPWeights
#### Comments: DO NOT change headers of the input file! 
####           REMOVE all pronunciation marks from data (i.e. provinicia, Zambezia, Manhica)!
####           Maputo should be Maputo Provincia; Cidade de Maputo should be Maputo Cidade
    


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


######################################################################################
########################################## MAIN ######################################
######################################################################################

#### Libraries
library(tidyverse)
library(readxl)
library(openxlsx)

source("district_targets.R")
source("subgroup_targets.R")
source("tree_targets.R")
source("growth_rates.R")


#### Set File Paths
xl_data <- "MozTargetModelInputs.xlsx"
tab_names <- excel_sheets(path = xl_data)

datapack_data <- "DataPack_Mozambique.xlsx"


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


#### Import Datapack PLHIV (to calculate target distribution)
District_PLHIV <- read_excel(path=datapack_data, 
                     sheet = "Epi Cascade I",
                     range = "A14:Q3902")

colnames(District_PLHIV) <- tolower(colnames(District_PLHIV))

District_PLHIV <- District_PLHIV %>%
  select("province" = "snu1", 
         "district" = "snu2",
         "plhiv" = "plhiv.na.age/sex/hivstatus.t") %>% 
  group_by(province, district) %>% 
  summarise(plhiv = sum(plhiv)) %>%
  ungroup()

District_PLHIV <- District_PLHIV %>% 
  mutate(province = recode(District_PLHIV$province,
                           Maputo = "Maputo Provincia",
                           `Cidade De Maputo` = "Maputo Cidade"))


#### Import Datapack TX (to calculate Age-Sex Growth Rates)
TX_DataPack_Tab <- read_excel(path=datapack_data, 
                             sheet = "TX",
                             range = "A14:H3902")

colnames(TX_DataPack_Tab) <- tolower(colnames(TX_DataPack_Tab))

TX_DataPack_Tab <- TX_DataPack_Tab %>%
  select("province" = "snu1", 
         "district" = "psnu",
         "age",
         "sex",
         "txcurr_fy20_target" = "tx_curr.n.age_sex_hivstatus.t_1")

TX_DataPack_Tab <- TX_DataPack_Tab %>% 
  mutate(province = recode(TX_DataPack_Tab$province,
                           Maputo = "Maputo Provincia",
                           `Cidade De Maputo` = "Maputo Cidade"))

TX_DataPack_Tab$district <- gsub("\\[.*$", "", TX_DataPack_Tab$district)
TX_DataPack_Tab$district <- trimws(TX_DataPack_Tab$district, which = "right")

#### Get Overall COP Target
OU_TXCURR_Target_COP19 <- Province_Targets[[which(Province_Targets$province=="TOTAL"),3]]
OU_TXCURR_Target_COP20 <- Province_Targets[[which(Province_Targets$province=="TOTAL"),2]]
cat("TX_CURR COP19 Target is", OU_TXCURR_Target_COP19)
cat("TX_CURR COP20 Target is", OU_TXCURR_Target_COP20)


#### Extract Province Targets
## DF format: col1 = province; col2 = target
COP19_Province_Targets <- Province_Targets %>% 
  select(c("province", "target" = "fy20cop19targetsadj"))


#### Run Functions for COP19
COP19 <- targetsetter(COP19_Province_Targets, District_TX_CurrT_FY19Q4, District_PLHIV) # District Targets Revised & FY19Q4 Targets
COP19ips <- ip_allocate(COP19) # Takes District Targets Revised
COP19phases <- phase_allocate(COP19ips) # IP Targets Revised
COP19agesex <- agesex_allocate(COP19) # Takes District Targets Revised
COP19tree <- treemodel(COP19) # Takes District Targets Revised


#### Extract Province Targets and COP19 District Targets for COP20
## DF format: col1 = province; col2 = target
COP20_Province_Targets <- Province_Targets %>% 
  select(c("province", "target" = "fy21cop20targetsadj"))
District_TX_CurrT_COP19 <- COP19 %>% 
  select(c("psnu" = "district", "txcurr_target" = "new_district_target"))


#### Run Functions for COP20
COP20 <- targetsetter(COP20_Province_Targets, District_TX_CurrT_COP19, District_PLHIV) # District Targets Revised & COP19 Targets
COP20ips <- ip_allocate(COP20) # Takes District Targets Revised
COP20phases <- phase_allocate(COP20ips) # IP Targets Revised
COP20agesex <- agesex_allocate(COP20) # Takes District Targets Revised
COP20tree <- treemodel(COP20) # Takes District Targets Revised


#### Calculate Growth Rate
COP19growth <- growth_calculator(TX_DataPack_Tab, 
                  COP19agesex, 
                  c("province", "district", "age", "sex"))

COP20growth <- growth_calculator(TX_DataPack_Tab, 
                                 COP20agesex, 
                                 c("province", "district", "age", "sex"))



#### Exports
list_df_19 <- list("COP19" = COP19,
                "COP19ips" = COP19ips,
                "COP19phases" = COP19phases,
                "COP19agesex" = COP19agesex,
                "COP19growth" = COP19growth,
                "COP19tree" = COP19tree)

list_df_20 <- list("COP20" = COP20,
                "COP20ips" = COP20ips,
                "COP20phases" = COP20phases,
                "COP20agesex" = COP20agesex,
                "COP20growth" = COP20growth,
                "COP20tree" = COP20tree)


openxlsx::write.xlsx(list_df_19, file = "MozambiqueTargetModel_19.xlsx", append=TRUE)
openxlsx::write.xlsx(list_df_20, file = "MozambiqueTargetModel_20.xlsx", append=TRUE)
