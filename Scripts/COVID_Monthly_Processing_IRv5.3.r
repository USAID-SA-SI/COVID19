# LOAD LIBRARIES
library(googledrive)
library(gargle)
library(googlesheets4)
library(tidyverse)
library(tidyr)
library(readr)
library(readxl)
library(gagglr)
library(Wavelength)
library(lubridate)
library(here)
library(janitor)

# devtools::install_github("USAID-OHA-SI/gagglr")

# SET CREDENTIALS & GLOBALS
myuser<-()
set_email()
load_secrets()
#after this step copy and paste into the R profile script and save
#now stored so don't have to authenticate
drive_auth()
gs4_auth()

current_mo_full<-"2022-07-31"



# EXTRACT FROM LIVE GOOGLE SHEET -----------------------------------------------
ANOVA<-read_sheet(as_sheets_id('1d4O3gZwdD1qglk1FW442sYnoHQ9SBH6V60s09kEQrjs'), sheet = "Tracker v3",
                  col_types="c")


GUIDEHOUSE<-read_sheet(as_sheets_id('1As-AVLcqhduKL0jA-c92D0vlO0gh1wIuumfMzYwNzo0'), sheet = "Tracker v3",
                       col_types="c")

GETF_NEXTMILE<-read_sheet(as_sheets_id('1WHdEzsyelf20N60JoP2ascrKQynDvaOEnfV3SRVcdBA'), sheet = "Tracker v3",
                          col_types="c") 

  
ADAPT_RTC <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet = "RTC_v3",
                       col_types="c") %>% 
  rename(Prime=Partner)


ADAPT_Aurum <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet = "Aurum_v3",
                         col_types="c") %>% 
  rename(Sub=Partner)

ADAPT_FPD <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="FPD_v3",
                       col_types="c") %>% 
  rename(Sub=Partner)

ADAPT_Genesis <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="Genesis_v3",
                           col_types="c") %>% 
  rename(Sub=Partner)


ADAPT_Intrahealth <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="IntraHealth_v3",
                        col_types="c") %>% 
  rename(Sub=Partner)


ADAPT_ReAction <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="ReAction!_v3",
                            col_types="c") %>% 
  rename(Sub=Partner)



ADAPT <-bind_rows(ADAPT_RTC,ADAPT_Aurum, ADAPT_FPD,
                  ADAPT_Genesis,ADAPT_Intrahealth,ADAPT_ReAction)%>%  
  rename(Partner=Prime) %>% 
  select(Partner,Sub,everything()) %>% 
  mutate(Partner="Right to Care")

  
  
  
PARTNER_TOOLS <-bind_rows(ADAPT,ANOVA,GUIDEHOUSE,GETF_NEXTMILE)




## Restructure dataset so it's long and dates are in correct format ------------

numeric<- PARTNER_TOOLS %>%  
  filter(!`Original Dataelement Code`=="VACC_0.8") %>% 
  gather(date,value,"31-Jul-22":"31-Dec-23") %>% 
  clean_names() %>% 
  mutate(date=as.Date(date, format="%d-%b-%y"),
         table="usaid arpa",
         mon_yr= format(date, "%Y-%m")) %>% 
  filter(!is.na(value)) %>% 
  mutate(value=str_remove_all(value,"[,]"),
    value=as.numeric(value))
  



# PULL HIERARCHY 
hierarchy<-pull_hierarchy("cDGPF739ZZr", myuser, askpass::askpass()) %>%
  mutate(facility=str_sub(orgunit,4)) %>%
  select(snu1,psnu,psnuuid) %>% 
  distinct(snu1,psnu,psnuuid) %>% 
  filter(!is.na(psnu))




# COMBINE & CLEAN!
arpa<-numeric %>% 
  rename(standardizeddisaggregate=disagg_category,
         indicator=data_element,
          indicator_code=dataelement_code,
          historic_indicator_code=original_dataelement_code,
          numeratordenom=num_denom) %>% 
  mutate(mech_code=case_when(
    partner=="ANOVA" ~ "70310",
    partner=="GETF" ~ "83001",
    partner=="Guidehouse" ~ "18321",
    partner=="Right to Care" ~ "ADAPT",
    TRUE ~ ""
  )) %>% 
  mutate(period=quarter(date, with_year = TRUE, fiscal_start = 10),
         period=stringr::str_remove(period, "20"),
         period=str_replace_all(period,"\\.","Q"),
         period=paste0("FY",period),
         operatingunit="South Africa") %>% 
  select(-date,-partner,-start_date,
         -end_date) %>% 
  rename_official() %>% 
  mutate(prime_partner_name=case_when(
    mech_code=="ADAPT" ~ "RIGHT TO CARE ADAPT",
    TRUE ~ prime_partner_name
  ),
  funding_agency=case_when(
    mech_code=="ADAPT" ~ "USAID",
    TRUE ~ funding_agency
  )) %>% 
  rename(sub_partner=sub, 
         psnu=org_unit) %>% 
  mutate(psnu=case_when(
    psnu=="IP-level" ~ "Data reported above PSNU level",
    TRUE ~ psnu)) %>% 
  left_join(hierarchy, by="psnu") %>% 
  mutate(indicator2=indicator,
         value2=value) %>%
  spread(indicator2,value2) %>%
  clean_psnu() %>% 
  mutate(last_refreshed=paste0(Sys.Date()))



# EXPORT
filename<-paste(current_mo_full,"Data_USAID_ARPA_GVAX.txt",sep="_")


write_tsv(arpa, file.path(here("Dataout"),filename),na="")



# EXPORT SUMMARY FILES
summary<-arpa %>% 
  group_by(prime_partner_name,indicator_code,
           indicator,standardizeddisaggregate,
           disaggregate,mon_yr) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  split_save(prime_partner_name,
             here("Dataout"),
             paste0(current_mo_full,"Summary Data"))
