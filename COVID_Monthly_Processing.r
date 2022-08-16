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
ANOVA<-read_sheet(as_sheets_id('1d4O3gZwdD1qglk1FW442sYnoHQ9SBH6V60s09kEQrjs'), sheet = "Tracker",
                  col_types="c")

BROADREACH<-read_sheet(as_sheets_id('1y--S2IOsK-NDPpMKdHDz47LhWYipm2LSwF0hhCcFGXg'), sheet = "Tracker",
                       col_types="c")

GUIDEHOUSE<-read_sheet(as_sheets_id('1As-AVLcqhduKL0jA-c92D0vlO0gh1wIuumfMzYwNzo0'), sheet = "Tracker",
                       col_types="c")

GETF_NEXTMILE<-read_sheet(as_sheets_id('1WHdEzsyelf20N60JoP2ascrKQynDvaOEnfV3SRVcdBA'), sheet = "Tracker",
                          col_types="c")

EQUIP<-read_sheet(as_sheets_id('1-LMibRE6d3cF2ZUDsOQh3bE7VkNN8qfCr53iPw1GbqA'), sheet = "Tracker",
                  col_types="c") #ended Dec 2021
  
  
ADAPT_RTC <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet = "RTC",
                       col_types="c")


ADAPT_Aurum <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet = "Aurum",
                         col_types="c")

ADAPT_FPD <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="FPD",
                       col_types="c")

ADAPT_Genesis <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="Genesis",
                           col_types="c")

ADAPT_HE2RO <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="HE2RO",
                         col_types="c")

ADAPT_IHPS <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="IHPS",
                        col_types="c")

ADAPT_NICD <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="NICD",
                        col_types="c")

ADAPT_Qode <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="Qode",
                        col_types="c")

ADAPT_ReAction <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), sheet="ReAction!",
                            col_types="c")

ADAPT_Stellenbosch <-read_sheet(as_sheets_id('1k2yDUZk49SRjTq3ceeXzogUw1TnMszjdXGtYJFwW060'), 
                                sheet="Stellenbosch",
                                col_types="c")

ADAPT <-bind_rows(ADAPT_RTC,ADAPT_Aurum, ADAPT_FPD,ADAPT_Genesis,ADAPT_HE2RO,
    ADAPT_IHPS,ADAPT_NICD,ADAPT_Qode,ADAPT_ReAction,ADAPT_Stellenbosch)%>%  
  rename(Partner=Prime) 
  
  
  
PARTNER_TOOLS <-bind_rows(ADAPT,ANOVA,GUIDEHOUSE,GETF_NEXTMILE,
                          EQUIP,BROADREACH) 




## Restructure dataset so it's long and dates are in correct format ------------

numeric<- PARTNER_TOOLS %>%  
  filter(!`Dataelement Code`=="VACC_0.8") %>% 
  gather(date,value,"31-Jan-22":"31-Mar-21") %>% 
  clean_names() %>% 
  mutate(date=as.Date(date, format="%d-%b-%y"),
         table="usaid arpa",
         mon_yr= format(date, "%Y-%m")) %>% 
  filter(!is.na(value)) %>% 
  mutate(value=str_remove_all(value,"[,]"),
    value=as.numeric(value))
  

y_n<- PARTNER_TOOLS %>%  
  filter(`Dataelement Code`=="VACC_0.8")%>% 
  # unnest(`31-Jan-22`:`30-Sep-21`) %>% 
  # select_if(~!is.logical(.)) %>% 
  gather(date,value,"31-Jan-22":"31-Mar-21") %>% 
  clean_names() %>% 
  mutate(date=as.Date(date, format="%d-%b-%y"),
         table="usaid arpa",
         mon_yr= format(date, "%Y-%m")) %>% 
  filter(!is.na(value)) %>% 
  mutate(value=case_when(
    value=="Yes" ~ "1",
    TRUE ~ "0"),
    value=as.numeric(value))



# PULL HIERARCHY 
hierarchy<-pull_hierarchy("cDGPF739ZZr", myuser, askpass::askpass()) %>%
  mutate(facility=str_sub(orgunit,4)) %>%
  select(snu1,psnu,psnuuid) %>% 
  distinct(snu1,psnu,psnuuid) %>% 
  filter(!is.na(psnu))




# COMBINE & CLEAN!
arpa<-bind_rows(numeric,y_n) %>% 
  rename(standardizeddisaggregate=disagg_category,
         indicator=data_element,
          indicator_code=dataelement_code,
          numeratordenom=num_denom) %>% 
  mutate(mech_code=case_when(
    partner=="ANOVA" ~ "70310",
    partner=="EQUIP" ~ "160610",
    partner=="Broadreach" ~ "70287",
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
  select(-date,-frequency,-partner,-start_date,
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

