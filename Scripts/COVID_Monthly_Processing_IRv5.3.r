#testing github


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
library(glamr)

# devtools::install_github("USAID-OHA-SI/gagglr")

# SET CREDENTIALS & GLOBALS
myuser<-("gsarfaty_SA")
set_email("gsarfaty@usaid.gov")
load_secrets()
#after this step copy and paste into the R profile script and save
#now stored so don't have to authenticate
drive_auth()
gs4_auth()

current_mo_full<-"2023-01-31" #change each month to be the most recent month of data


# READ IN HISTORIC DATA
historic<-read_tsv(here("Dataout","2022-06-30_Data_USAID_ARPA_GVAX_historic.txt")) %>% 
  mutate(last_refreshed=as.character(last_refreshed))


# EXTRACT FROM LIVE GOOGLE SHEET -----------------------------------------------
ANOVA<-read_sheet(as_sheets_id('1d4O3gZwdD1qglk1FW442sYnoHQ9SBH6V60s09kEQrjs'), sheet = "Tracker v3",
                  col_types="c")


GUIDEHOUSE<-read_sheet(as_sheets_id('1GGHPZGu3FjBhdtCu7-kgiATXmunkkdezLRxuHRTPI-s'), sheet = "Tracker v3",
                       col_types="c")

GETF_NEXTMILE<-read_sheet(as_sheets_id('1xDCQkg2ZJOZMrGdQ8IEfLnuuoRhBKmGjP1UQ00FbxWc'), sheet = "Tracker v3",
                          col_types="c") 

  
ADAPT_RTC <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet = "RTC_v3",
                       col_types="c") %>% 
  rename(Prime=Partner) %>% 
  mutate(Sub="Right to Care") %>% 
  relocate(Sub,.after=Prime)


ADAPT_Aurum <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet = "Aurum_v3",
                         col_types="c") %>% 
  rename(Sub=Partner)

ADAPT_FPD <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="FPD_v3",
                       col_types="c") %>% 
  rename(Sub=Partner)

ADAPT_Genesis <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="Genesis_v3",
                           col_types="c") %>% 
  rename(Sub=Partner)


ADAPT_Intrahealth <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="IntraHealth_v3",
                        col_types="c") %>% 
  rename(Sub=Partner)


ADAPT_ReAction <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="ReAction!_v3",
                            col_types="c") %>% 
  rename(Sub=Partner)


ADAPT_Anova <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="ANOVA_v3",
                            col_types="c") %>% 
  rename(Sub=Partner)


ADAPT_PulseHealth <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="PulseHealth_v3",
                         col_types="c") %>% 
  rename(Sub=Partner)


ADAPT_WDED <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="WDED_v3",
                               col_types="c") %>% 
  rename(Sub=Partner)


ADAPT_KI <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="Khethimpilo_v3",
                        col_types="c") %>% 
  rename(Sub=Partner)

ADAPT_HST <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="HST_v3",
                      col_types="c") %>% 
  rename(Sub=Partner)

ADAPT_THINK <-read_sheet(as_sheets_id('1fsbtskAYLXmqrAcZts2tR8Kg19JZPmRgXEmYzB3yYpw'), sheet="THINK_v3",
                          col_types="c") %>% 
  rename(Sub=Partner)





ADAPT <-bind_rows(ADAPT_RTC,ADAPT_Aurum, ADAPT_FPD,
                  ADAPT_Genesis,ADAPT_Intrahealth,ADAPT_ReAction,
                  ADAPT_Anova,ADAPT_PulseHealth, ADAPT_WDED,
                  ADAPT_KI, ADAPT_HST,ADAPT_THINK)%>%  
  rename(Partner=Prime) %>% 
  select(Partner,Sub,everything()) %>% 
  mutate(Partner="Right to Care",
         Disaggregate=case_when(
           Disaggregate=="60+ male" ~ "60+, male",
           TRUE ~ Disaggregate)
         ) %>% 
  separate(Disaggregate, into=c("ageasentered","sex"),
           sep=",", remove=FALSE, convert=TRUE, fill="right") %>% 
  mutate(ageasentered=case_when(
    `disagg category`=="Age/Sex" ~ ageasentered,
    TRUE ~ ""),
    sex=case_when(
      `disagg category`=="Age/Sex" ~ sex,
      `disagg category`=="Sex" ~ Disaggregate,
      TRUE ~ ""
    ),
    sex=case_when(
      sex==" male" ~ "Male",
      sex==" female" ~ "Female",
      TRUE ~ sex
    ))

  
  
  
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
  # clean_psnu() %>% 
  mutate(last_refreshed=paste0(Sys.Date())) %>% 
  unite(ind_key,historic_indicator_code,numeratordenom,sep="_",remove=FALSE) %>% 
  mutate(disaggregate=case_when(
    indicator_code=="CV.1.4-5" & str_detect(disaggregate,"Community-based") ~ "Community-based outreach vaccination sites",
    indicator_code=="CV.1.4-5" & str_detect(disaggregate,"transit team") ~ "Mobile team (or clinic) or transit team strategy",
    indicator_code=="CV.1.1-1" & str_detect(disaggregate,"Hard copy") ~ "Hard Copy Print",
    indicator_code=="CV.1.1-1" & str_detect(disaggregate,"Social") ~ "Social Media",
    indicator_code=="CV.1.1-1" & str_detect(disaggregate,"Mobile") ~ "Mobile/Telephone",
    TRUE ~ disaggregate
  ))


# IDENTIFY CLEAN INDICATOR NAMES
indicator_names<-arpa %>% 
  distinct(historic_indicator_code,indicator,numeratordenom) %>% 
  filter(!historic_indicator_code=="N/A",
         !indicator=="Number of policies, protocols, standards, or guidelines across any of the result areas\ndeveloped or adapted with USG support") %>% 
  unite(ind_key,historic_indicator_code,numeratordenom,sep="_") %>% 
  rename(indicator_clean=indicator) %>% 
  mutate(indicator_clean=case_when(
    ind_key=="RCCE_1.2_Numerator" ~ "Number of health workers and non-health workers trained on risk communication and community engagement (RCCE)",
    ind_key=="CoOp_6.1_Numerator" ~ "Number of policies, protocols, standards, or guidelines across any of the result areas developed or adapted with USG support",
    ind_key=="SURV_2.1_Numerator" ~ "Number of people trained on surveillance and/or rapid response (case investigation, contact tracing, and case finding) for COVID-19",
    ind_key=="CASE_5.1_Numerator" ~ "Number of facilities receiving technical assistance for case management, such as facility-level assessments, guidance, and/or training",
    ind_key=="IPC_4.1_Numerator" ~ "Number of health facilities where USG provided support for IPC and/or WASH for COVID-19",
    TRUE ~ indicator_clean
  ))


# MAKE INDICATOR KEY IN HISTORIC FOR JOINING
historic<-historic %>% 
  unite(ind_key,historic_indicator_code,numeratordenom,sep="_",remove=FALSE) %>% 
  left_join(indicator_names,by="ind_key")



# BIND HISTORIC AND CURRENT DATA
arpa_combined<-arpa %>% 
  left_join(indicator_names,by="ind_key") %>% 
  mutate(dis_code=indicator_code) %>% 
  bind_rows(historic) %>% 
  mutate(sex=case_when(
    standardizeddisaggregate=="Sex" ~ disaggregate,
    standardizeddisaggregate=="Age/Sex" ~ sex,
    TRUE ~ ""),
    indicator=case_when(
           is.na(indicator_clean) ~ indicator,
           TRUE ~ indicator_clean
         )) %>% 
  select(-indicator_clean)


# Sex/Aggregated for Vax Indicators then combine for final
final_df<-arpa_combined %>% 
  filter(dis_code %in% c("CV.1.4-6","CV.1.4-7","CV.1.4-8",
                               "CV.1.9-1","CV.1.9-2","CV.1.9-3"),
         standardizeddisaggregate %in% c("Sex","Age/Sex")) %>% 
  mutate(standardizeddisaggregate=case_when(
    mon_yr < "2022-12" & standardizeddisaggregate=="Sex" ~ "Sex Aggregated",
    mon_yr >= "2022-12" & standardizeddisaggregate=="Age/Sex" ~ "Sex Aggregated",
    TRUE ~ ""
  ),
  disaggregate=case_when(
    mon_yr >= "2022-12" ~ sex,
    TRUE ~ disaggregate
  )) %>% 
  filter(!standardizeddisaggregate=="") %>% 
  bind_rows(arpa_combined) %>% 
  mutate(indicator2=indicator,
         value2=value) %>%
  spread(indicator2,value2) 



ind<-distinct(final_df,historic_indicator_code,indicator,numeratordenom)

# EXPORT
filename<-paste(current_mo_full,"Data_USAID_ARPA_GVAX_COMBINED_v1.3.csv",sep="_")


write_csv(final_df, file.path(here("Dataout"),filename),na="")



# EXPORT SUMMARY FILES
# summary<-arpa_combined%>% 
#   filter(period %in% c("FY22Q4",
#                        "FY23Q1")) %>% 
#   group_by(prime_partner_name,sub_partner,
#            dis_code,indicator_code,
#            indicator,standardizeddisaggregate,
#            disaggregate,mon_yr,period) %>% 
#   summarize_at(vars(value),sum,na.rm=TRUE) %>% 
#   ungroup() %>% 
#   split_save(prime_partner_name,
#              here("Dataout"),
#              paste0(current_mo_full,"Summary Data"))
