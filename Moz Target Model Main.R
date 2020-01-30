#### TX_CURR TARGET SETTING MAIN
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

source("district_targets.R")
source("subgroup_targets.R")
source("tree_targets.R")

######################################################################################
########################################## MAIN ######################################
######################################################################################

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


#### Extract Province Targets
## DF format: col1 = province; col2 = target
COP19_Province_Targets <- Province_Targets %>% select(c("province", "target" = "fy20cop19targetsadj"))
COP20_Province_Targets <- Province_Targets %>% select(c("province", "target" = "fy21cop20targetsadj"))


#### Run Functions
COP19 <- targetsetter(COP19_Province_Targets) # District Targets Revised
COP19ips <- ip_allocate(COP19) # Takes District Targets Revised
COP19phases <- phase_allocate(COP19ips) # IP Targets Revised
COP19agesex <- agesex_allocate(COP19) # Takes District Targets Revised
COP19tree <- treemodel(COP19) # Takes District Targets Revised

COP20 <- targetsetter(COP20_Province_Targets) # District Targets Revised
COP20ips <- ip_allocate(COP20) # Takes District Targets Revised
COP20phases <- phase_allocate(COP20ips) # IP Targets Revised
COP20agesex <- agesex_allocate(COP20) # Takes District Targets Revised
COP20tree <- treemodel(COP20) # Takes District Targets Revised


#### Exports
list_df_19 <- list("COP19" = COP19,
                "COP19ips" = COP19ips,
                "COP19phases" = COP19phases,
                "COP19agesex" = COP19agesex,
                "COP19tree" = COP19tree)

list_df_20 <- list("COP20" = COP20,
                "COP20ips" = COP20ips,
                "COP20phases" = COP20phases,
                "COP20agesex" = COP20agesex,
                "COP20tree" = COP20tree)


openxlsx::write.xlsx(list_df_19, file = "MozambiqueTargetModel_19.xlsx", append=TRUE)
openxlsx::write.xlsx(list_df_20, file = "MozambiqueTargetModel_20.xlsx", append=TRUE)


