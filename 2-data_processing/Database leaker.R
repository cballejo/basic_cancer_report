## Packages

library(tidyverse)
library(lubridate)

source("2-data_processing/functions.R")

## Read database

Consultas_codificadas <- read_csv("2-data_processing/raw data table.csv") %>% 
  mutate(`FECHA RECEP` = as_date(ymd_hms(`FECHA RECEP`)))


POSITIVOS <- Consultas_codificadas %>% 
  detect_cancer() %>%
  mutate (AÑO = year(`FECHA RECEP`)) %>%  
  filter(CANCER == "SI") %>% 
  select(SEXO, `FECHA RECEP` , DIAGNOSTICO, AÑO) %>% 
  arrange(`FECHA RECEP`) 

write_excel_csv(POSITIVOS, "data_processed.csv")  





