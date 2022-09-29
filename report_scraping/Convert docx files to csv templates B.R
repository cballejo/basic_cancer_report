library(tidyverse)
library(officer)
library(fs)

directorio <- "report_scraping"  # data folder 

archivos <- dir_ls(path = directorio, glob = "*.docx")  

datos <- data.frame(protocolo = NA, paciente = NA, edad = NA, sexo = NA, fecha = NA, material = NA, diagnostico = NA)

for (i in 1:length(archivos)) {
  
  contenido <- read_docx(archivos[i]) %>% docx_summary() %>% as_tibble()
  
  paciente <- contenido %>% filter(str_detect(text,  "PACIENTE")) %>% 
    mutate(text1 = str_sub(text, start = 10, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  
  paciente <- unique(paciente)
  
  diagnostico <- contenido %>% filter(str_detect(text,  "DIAGNOSTICO|DIAGNÓSTICO")) %>% 
    mutate(text1 = str_sub(text, start = 13, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  
  material <- contenido %>% filter(str_detect(text,  "MATERIAL REMITIDO")) %>% 
    mutate(text1 = str_sub(text, start = 19, end = nchar(text)),
           text1 = str_trim(text1, side = "both"),
           text1 = str_to_upper(text1))  %>% pull(text1)
  
  protocolo <- contenido %>% filter(str_detect(text,  "PROTOCOLO")) %>% 
    mutate(posicion = str_locate(text, pattern = "EDAD")[,"start"],
           text1 = str_sub(text, start = 11, end = posicion-1),
           text1 = str_trim(text1, side = "both"),
           text1 = str_to_upper(text1))  %>% pull(text1)
  
  protocolo <- unique(protocolo)
  
  edad <- contenido %>% filter(str_detect(text,  "EDAD")) %>% 
    mutate(posIni = str_locate(text, pattern = "EDAD:")[,"end"],
           posFin = str_locate(text, pattern = "AÑOS")[,"start"],
           text1 = str_sub(text, start = posIni+1, end = posFin-1),
           text1 = str_trim(text1, side = "both"),
           text1 = as.numeric(text1))  %>% pull(text1)
  
  edad <- unique(edad)
  
  sexo <- contenido %>% filter(str_detect(text,  "SEXO")) %>% 
    mutate(posIni = str_locate(text, pattern = "SEXO:")[,"end"],
           text1 = str_sub(text, start = posIni+1, end = nchar(text)),
           text1 = str_trim(text1, side = "both"),
           text1 = str_to_upper(text1))  %>% pull(text1)
  
  sexo <- unique(sexo)
  
  fecha <- contenido %>% filter(str_detect(text,  "FECHA")) %>% 
    mutate(posIni = str_locate(text, pattern = "FECHA:")[,"end"],
           posFin = str_locate(text, pattern = "00SS|OOSS")[,"start"],
           text1 = str_sub(text, start = posIni+1, end = posFin-1),
           text1 = str_trim(text1, side = "both"),
           text1 = str_to_upper(text1))  %>% pull(text1)
  
  fecha <- unique(fecha)
  
  datos1 <- cbind(protocolo, paciente, edad, sexo, fecha, material, diagnostico)
  
  datos <- rbind(datos, datos1)
  
}

datos <- datos[-1,]  # eliminamos primer fila con NA

 
write_excel_csv(datos, "data_scrap.csv") 





