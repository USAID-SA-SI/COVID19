# PROJECT:  duplicate rows for data collection tool

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)

# IMPORT ------------------------------------------------------------------

df_adapt_original<-read_excel("adapt_to_dup_Dec2022.xlsx", sheet = "DE")


df_adapt_dup<-do.call("rbind", replicate(52, df_adapt_original, simplify = FALSE))

write.csv(df_adapt_dup, paste0("ADAPT_COVIDentry_52districts", format(Sys.time(), "%d-%b-%Y"), ".csv"))

df_adapt_dist_original<-read_excel("adapt_to_dup_Dec2022.xlsx", sheet = "DISTRICTS")
df_adapt_dist_dup<-do.call("rbind", replicate(96,df_adapt_dist_original, simplify = FALSE))

write.csv(df_adapt_dist_dup, paste0("ADAPT_districts_52districts", format(Sys.time(), "%d-%b-%Y"), ".csv"))




