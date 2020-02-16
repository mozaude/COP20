#### Datapack IM DATASET (Main)
#### FORMAT: PSNUxIM Datapack
#### AUTHOR: Randy Yee
#### CREATION DATE: 2/13/2019
#### DESCRIPTION: To determine PSNUxIM Distributions
#### NOTE: MSD Should be in ANSI Encoded!


######################################################################################
########################################## MAIN ######################################
######################################################################################
rm(list = ls())
cat("\014")


##########################################################################
#### Load Libraries
library(tidyverse)
library(openxlsx)
library(ICPIHelpers) ## TODO Need to add read_new_msd function 
library(readxl)

source("spell_check.R")
##########################################################################
#### Specify Columns to Drop
drop_extraneous_columns <- 
  c("operatingunituid",
    "region",
    "regionuid",
    "countryname",
    "operatingunituid", 
    "snu1uid",
    "pre_rgnlztn_hq_mech_code",
    "dreams", 
    "primepartner", 
    "prime_partner_duns", 
    "award_number",
    "standardizeddisaggregate",
    "trendsfine",
    "trendssemifine",
    "trendscoarse",
    "categoryoptioncomboname",
    "hiv_treatment_status",
    "population",
    "coarsedisaggregate",
    "modality")

value_columns <- 
  c(
    "2019 cumulative",
    "2019 qtr1", 
    "2019 qtr2", 
    "2019 qtr3", 
    "2019 qtr4", 
    "2019 targets"
    )

##########################################################################
#### Read PSNUxIM MSD
new_df <- read_new_msd(file.choose(),
                       save_rds = FALSE,
                       remove_txt = FALSE)

##########################################################################
#### Filter for PSNUxIM specific indicator combinations

psnuim_combinations <- new_df %>% 
  filter(indicator %in% c("CXCA_SCRN",
                          
                          "GEND_GBV",
                          
                          "HTS_INDEX_COM",
                          "HTS_INDEX_FAC",
                          "HTS_RECENT",
                          "HTS_TST",
                          
                          "OVC_HIVSTAT",
                          "OVC_SERV",
                          
                          "PMTCT_ART",
                          "PMTCT_EID",
                          "PMTCT_STAT",
                          
                          "PP_PREV",
                          
                          "TB_ART",
                          "TB_PREV",
                          "TB_STAT",
                          
                          "TX_CURR",
                          "TX_NEW",
                          "TX_PVLS",
                          "TX_TB",
                          
                          "VMMC_CIRC"
  )) %>%
  ## Changed Total Numerator to Total just for OVC_HIVSTAT
  mutate(disaggregate = ifelse(indicator=="OVC_HIVSTAT", "Total", disaggregate)) %>%
  
  filter(disaggregate %in% c(
    "Age",
    "Age/NewExistingArt/HIVStatus",
    "Age/NewExistingART/Sex/HIVStatus",
    "Age/TherapyType/NewExistingArt/HIVStatus",
    "Age/Sex",
    "Age/Sex/HIVStatus",
    "Age/Sex/Indication/HIVStatus",
    "Age/Sex/KnownNewPosNeg",
    "Age/Sex/KnownNewResult",
    "Age/Sex/NewExistingART/HIVStatus",
    "Age/Sex/ProgramStatus",
    "Age/Sex/Result",
    "Age/Sex/TBScreen/NewExistingART/HIVStatus",
    "Total",
    "ViolenceServiceType",
    
    "Inpat/Age/Sex/Result",
    "VCT/Age/Sex/Result",
    "Pediatric/Age/Sex/Result",
    "Emergency Ward/Age/Sex/Result",
    "OtherPITC/Age/Sex/Result",
    "Index/Age/Sex/Result",
    "IndexMod/Age/Sex/Result",
    "PMTCTPost ANC/Age/Sex/Result",
    "MobileMod/Age/Sex/Result"
    
  )) %>%
  
  filter(psnu != "Data reported above PSNU level")%>%
  filter(indicatortype != "Not Applicable")%>%
  filter(mech_name!="Dedup") %>%
  filter(fiscal_year == "2019")

##########################################################################
#### Convert MSD to Semi-Wide Format with FY-QTR Combos
convert_df <- tibble::rowid_to_column(psnuim_combinations)

convert_df <- convert_df %>%
  gather("period","value",targets:cumulative) %>%
  mutate(fiscalperiod = paste(fiscal_year,period)) %>%
  select(-fiscal_year, -period)

convert_df <- convert_df %>%
  group_by_at(vars(region:modality, fiscalperiod)) %>% #for site level start at orgunituid
  summarise(Value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(fiscalperiod,Value)

convert_df <- convert_df %>% 
      na_if(0)%>% 
      select(-drop_extraneous_columns) 
    
##############################################################
#### Create Simple Long Format
long_df <- pivot_longer(convert_df, 
                        value_columns,
                        names_to = "Period",
                        values_to = "Values")

long_df <- long_df %>%
  group_by_at(vars(-Values)) %>%
  summarise(Total = sum(Values, na.rm = T)) %>%
  ungroup() %>%
  select(psnu,
         typemilitary,
         psnuuid,
         age = ageasentered,
         sex,
         mech_code, 
         indicatortype,
         fundingagency,
         mech_name,
         indicator, 
         numeratordenom, 
         disaggregate,
         statushiv,
         statustb,
         statuscx,
         otherdisaggregate, 
         Period, 
         Total)

simple_long <- long_df %>%
  unite(name,
        mech_code:mech_name, 
        sep = "_", 
        remove = TRUE, 
        na.rm = TRUE) %>%
  unite(code,
        indicator:otherdisaggregate, 
        sep = ".", 
        remove = TRUE, 
        na.rm = TRUE) %>% 
  filter(Period == "2019 qtr4")
# %>%
#   filter(str_detect(code, "qtr4") | str_detect(code, "targets")) # Change for other periods

##########################################################################
#### Create Wide by IMs Dataset
MSD_IM <- simple_long %>%
  pivot_wider(
              names_from = "name", 
              values_from = "Total") %>%
  na_if(0) %>%
  mutate(typemilitary = recode(typemilitary,"Y" = "#Military", "N" = "#SNU")) %>%
  select(-contains("Peace Corps")) # Drop Peace Corps Columns to be re-added






####################################################################################################################################################
#Test
#Drop Age & Sex
#Consolidate Rows
#Check Missing

Test_MSD_IM <- MSD_IM %>%
  select(-c(age,sex, Period)) %>%
  group_by(psnu,typemilitary,psnuuid,code) %>%
  summarise_all(funs(sum(., na.rm = TRUE)))%>%
  ungroup() %>%
  mutate(rowtotal = rowSums(Test_MSD_IM[,5:(length(Test_MSD_IM))], na.rm=T))




####################################################################################################################################################

##########################################################################
#### Add MISAU DIstributions to their 7 indicators (TXCURR, HTS_TST, HTS_POS, PVLS, TXNEW, PMTCTSTAT, ART)

PEPFAR_MISAU_Source <- read_excel(path= "PSNU_IM_Distribution.xlsx", 
                                  sheet = "PEPFAR_MISAU_Dist") %>%
  select(PSNU = DISTRICTS,
         misau_contribution = `Contributions of MISAU-supported sites to TXCURRFy19Q4`)
colnames(PEPFAR_MISAU_Source) <- tolower(colnames(PEPFAR_MISAU_Source))
PEPFAR_MISAU_Source <- moz_recode(PEPFAR_MISAU_Source)%>% 
  mutate(psnu = recode(PEPFAR_MISAU_Source$psnu,
                       "Ilha De Mozambique" = "Ilha De Moçambique",
                       "Manhica" = "Manhiça"))


MSD_IM_MOH <- merge(MSD_IM, PEPFAR_MISAU_Source, by = "psnu") %>% 
  mutate("81776_MISAU_Percent" = ifelse(str_detect(code, "HTS_TST") |
                                  str_detect(code, "TX_CURR") |
                                  str_detect(code, "TX_NEW") |
                                  str_detect(code, "TX_PVLS") |
                                  str_detect(code, "PMTCT_STAT") |
                                  str_detect(code, "TB_ART"),
                                misau_contribution, 
                                NA
                                )) %>%
  select(-misau_contribution) 

## Clear Other Columns if Misau Percent is 100%
MSD_IM_MOH_Clear <- MSD_IM_MOH %>%
  filter(`81776_MISAU_Percent` == 1) %>%
  select(psnu, typemilitary, age, sex, code, `81776_MISAU_Percent`)

MSD_IM_MOH_Clear2 <- MSD_IM_MOH %>%
  filter(`81776_MISAU_Percent` != 1 | is.na(`81776_MISAU_Percent`))

MSD_IM_MOH_Clear3 <- bind_rows(MSD_IM_MOH_Clear2, MSD_IM_MOH_Clear)

## Calculate rowtotal
MSD_IM_MOH_Clear3 <- MSD_IM_MOH_Clear3 %>% 
  mutate(rowtotal = rowSums(MSD_IM_MOH_Clear3[,7:(length(MSD_IM_MOH_Clear3)-1)], na.rm=T))

## Calculate MISAU ghost value for achieving MISAU percent
## If MISAU percent is 1, ghost value set at 1, else calculate ghost value
MSD_IM_MOH_Final <- MSD_IM_MOH_Clear3 %>%
   mutate(`81776_MISAU` = ifelse(`81776_MISAU_Percent` == 1,
                                 1,
                                 (rowtotal*`81776_MISAU_Percent`)/(1-`81776_MISAU_Percent`)))

##########################################################################
#### Add Peace Corps Percentages to their 3 indicators (PP_PREV, OVC_SERV, OVC_HIVSTAT)

## Import Peace Corps Worksheet Age-Sex Targets Tab: PP_PREV
## Note: column names should be combined in the Excel Tool as Female.10-14, Male.10-14, etc.
## 1) Unpivot
## 2) Separate age, sex
## 3) Add indicator column with PP_PREV
PC_PP_PREV_Source <- read_excel(path= "MOZAMBIQUE_PC COP20 Target Support Tool 20200124_with all PSNUs (002).xlsx", 
                        sheet = "2. PP_PREV AgeSex Target",
                        range = "B19:T133")

PC_PP_PREV_Source2 <- PC_PP_PREV_Source %>% 
  pivot_longer(colnames(PC_PP_PREV_Source[,2:length(PC_PP_PREV_Source)]), names_to = "age_sex", values_to = "pc_percent")

PC_PP_PREV_Source3 <- separate(PC_PP_PREV_Source2, age_sex, c("sex", "age"), sep = "[.]") %>%
  mutate(indicator = "PP_PREV") %>%
  mutate(PSNU = recode(PSNU, "lha De Motambique" = "Ilha De Moçambique", "Manhi??a" = "Manhiça"))

colnames(PC_PP_PREV_Source3) <- tolower(colnames(PC_PP_PREV_Source3)) 


## Import Peace Corps Worksheet Age-Sex Targets Tab: OVC_SERV
## Note: column names should be combined in the Excel Tool as Female.10-14, Male.10-14, etc.
## 1) Unpivot
## 2) Separate age, sex
## 3) Add indicator column with OVC_SERV
PC_OVC_SERV_Source <- read_excel(path= "MOZAMBIQUE_PC COP20 Target Support Tool 20200124_with all PSNUs (002).xlsx", 
                                sheet = "4. OVC_SERV AgeSex Target",
                                range = "B19:N133")

PC_OVC_SERV_Source2 <- PC_OVC_SERV_Source %>% 
  pivot_longer(colnames(PC_OVC_SERV_Source[,2:length(PC_OVC_SERV_Source)]), names_to = "age_sex", values_to = "pc_percent")

PC_OVC_SERV_Source3 <- separate(PC_OVC_SERV_Source2, age_sex, c("sex", "age"), sep = "[.]") %>%
  mutate(indicator = "OVC_SERV")%>%
  mutate(PSNU = recode(PSNU, "lha De Motambique" = "Ilha De Moçambique", "Manhita" = "Manhiça"))

colnames(PC_OVC_SERV_Source3) <- tolower(colnames(PC_OVC_SERV_Source3))

## Append PC Indicators
PC_Source <- rbind(PC_PP_PREV_Source3, PC_OVC_SERV_Source3)


## Merge PC_Source to MSD_IM
MSD_IM_MOH_ind <- separate(MSD_IM_MOH_Final, code, into = c("indicator", "code"), sep = "[.]", extra = "merge")

MSD_IM_MOH_PC <- merge(MSD_IM_MOH_ind, PC_Source, by = c("psnu", "age", "sex", "indicator"), all.x = T)

##########################################################################
#### Calculate Final Row Percentages

## Clear Other Columns if PC Percent is 100%
MSD_IM_MOH_PC_Clear <- MSD_IM_MOH_PC %>%
  filter(pc_percent == 1) %>%
  select(psnu, typemilitary, age, sex, indicator, code, pc_percent)

MSD_IM_MOH_PC_Clear2 <- MSD_IM_MOH_PC %>%
  filter(pc_percent != 1 | is.na(pc_percent))

MSD_IM_MOH_PC_Clear3 <- bind_rows(MSD_IM_MOH_PC_Clear2, MSD_IM_MOH_PC_Clear)

## Redo rowtotal
MSD_IM_MOH_PC_Clear3 <- MSD_IM_MOH_PC_Clear3 %>% 
  mutate(rowtotal = rowSums(MSD_IM_MOH_PC_Clear3[,8:(length(MSD_IM_MOH_PC_Clear3)-1)], na.rm=T))

## Calculate PC ghost value for achieving PC percent
## If PC percent is 1, ghost value set at 1, else calculate ghost value
MSD_IM_MOH_PC_Final <- MSD_IM_MOH_PC_Clear3 %>%
  mutate(`11463_PC` = ifelse(pc_percent == 1,
                                1,
                                (rowtotal*pc_percent)/(1-pc_percent)))


## Clean
MSD_IM_Final <- MSD_IM_MOH_PC_Final %>% 
  filter(!code %in% c(
    "N.Age/Sex/Result.Positive.Known at Entry.2019 qtr4",
    "N.Index/Age/Sex/Result.Negative.2019 qtr4",
    "N.Index/Age/Sex/Result.Negative.Newly Identified.2019 qtr4",
    "N.Index/Age/Sex/Result.Positive.2019 qtr4",
    "N.Index/Age/Sex/Result.Positive.Newly Identified.2019 qtr4",
    "N.IndexMod/Age/Sex/Result.Negative.2019 qtr4",
    "N.IndexMod/Age/Sex/Result.Negative.Newly Identified.2019 qtr4",
    "N.IndexMod/Age/Sex/Result.Positive.2019 qtr4",
    "N.IndexMod/Age/Sex/Result.Positive.Newly Identified.2019 qtr4",
    "N.Total.Negative.2019 qtr4",
    "N.Total.Positive.Not Receiving ART.2019 qtr4",
    "N.Total.Positive.Receiving ART.2019 qtr4",
    "N.Total.Unknown.2019 qtr4",
    "N.Total.Unknown.No HIV Status.2019 qtr4",
    "N.Total.Unknown.Test Not Required.2019 qtr4",
    "N.Age/Sex.Unknown.2019 qtr4",
    "N.Age/Sex/KnownNewResult.Negative.Recent.2019 qtr4",
    "D.Age/Sex/Indication/HIVStatus.Positive.Targeted.2019 qtr4",
    "D.Age/Sex/Indication/HIVStatus.Positive.Undocumented Test Indication.2019 qtr4",
    "N.Age/Sex/Indication/HIVStatus.Positive.Targeted.2019 qtr4",
    "N.Age/Sex/Indication/HIVStatus.Positive.Undocumented Test Indication.2019 qtr4"
    
)) %>%
  select(-c(rowtotal, pc_percent, `81776_MISAU_Percent`)) 

MSD_IM_Final<- MSD_IM_Final %>%
  mutate(rowtotal = rowSums(MSD_IM_Final[,8:(length(MSD_IM_Final))], na.rm=T)) %>%
  na_if(0)

test <- MSD_IM_Final[,8:(length(MSD_IM_Final)-1)] / MSD_IM_Final[,(length(MSD_IM_Final))] 

test <- test %>% mutate(rowtotal = rowSums(test[1:(length(test))], na.rm=T))
#openxlsx::write.xlsx(MSD_IM_Final, "MSD_IM_Final.xlsx")
#openxlsx::write.xlsx(test, "test.xlsx")
