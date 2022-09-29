## Packages

library(tidyverse)
library(lubridate)

source("data_processing/functions.R")

## Read database

Consultas_codificadas <- read_csv("data_processing/data_scrap.csv") %>% 
  mutate(`FECHA RECEP` = dmy(fecha))  %>%  select(-fecha) 

names(Consultas_codificadas) <- str_to_upper(names(Consultas_codificadas))

POSITIVOS <- Consultas_codificadas %>% 
  detect_cancer() %>%
  mutate (AÑO = year(`FECHA RECEP`)) %>%  
  filter(CANCER == "SI") %>% 
  select(SEXO, `FECHA RECEP` , DIAGNOSTICO, AÑO) %>% 
  arrange(`FECHA RECEP`) 

write_excel_csv(POSITIVOS, "data_processed.csv")  





