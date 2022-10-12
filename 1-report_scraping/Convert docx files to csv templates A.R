library(tidyverse)
library(officer)
library(fs)
library(glue)

directorio <- "1-report_scraping"  # data folder 

archivos <- dir_ls(path = directorio, glob = "*.docx")  

datos <- data.frame(protocolo = NA, paciente = NA, edad = NA, medico = NA,  fecha = NA,
                    procedencia = NA, tipo = NA, topografia = NA, 
                    clinica = NA, metodo = NA, macroscopia = NA, 
                    microscopia = NA, diagnostico = NA, observaciones = NA)

for (i in 1:length(archivos)) {
  
  contenido <- read_docx(archivos[i]) %>% docx_summary() %>% as_tibble() %>% 
    mutate(ID = row_number())
  
  ubi_protocolo <- contenido %>% filter(str_detect(text,  "PROTOCOLO")) %>%
    pull(ID)
  
  protocolo <- contenido[ubi_protocolo+1,4] %>% pull()
  
  ubi_paciente <- contenido %>% filter(str_detect(text,  "PACIENTE")) %>%
    pull(ID)
  
  paciente <- contenido[ubi_paciente+1,4] %>% pull()
  
  ubi_edad <- contenido %>% filter(str_detect(text,  "EDAD")) %>%
    pull(ID)
  
  edad <- contenido[ubi_edad+1,4] %>% pull()

  
  ubi_medico <- contenido %>% filter(str_detect(text,  "MÉDICO")) %>%
    pull(ID)
  
  medico <- contenido[ubi_medico+1,4] %>% pull()
  
  ubi_fecha <- contenido %>% filter(str_detect(text,  "FECHA")) %>%
    pull(ID)
  
  fecha <- contenido[ubi_fecha+1,4] %>% pull()
  
  
  ubi_procedencia <- contenido %>% filter(str_detect(text,  "PROCEDENCIA")) %>%
    pull(ID)
  
  procedencia <- contenido[ubi_procedencia+1,4] %>% pull()
  
  ubi_tipo <- contenido %>% filter(str_detect(text,  "TIPO")) %>%
    pull(ID)
  
  tipo <- contenido[ubi_tipo+1,4] %>% pull()
  
  
  ubi_topografia <- contenido %>% filter(str_detect(text,  "TOPOGRAFÍA")) %>%
    pull(ID)
  
  topografia1 <- contenido %>% filter(str_detect(text,  "TOPOGRAFÍA")) %>% 
    mutate(text1 = str_sub(text, start = 12, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  
  topografia2 <- contenido[ubi_topografia+1,4]
  
  topografia3 <- contenido[ubi_topografia+2,4]
  
  topografia <- glue("{topografia1} {topografia2} {topografia3}")
  
  clinica <- contenido %>% filter(str_detect(text,  "CLÍNICOS")) %>% 
    mutate(text1 = str_sub(text, start = 16, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  
  
  metodo <- contenido %>% filter(str_detect(text,  "MÉTODO")) %>% 
    mutate(text1 = str_sub(text, start = 21, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  
  
  ubi_macroscopia <- contenido %>% filter(str_detect(text,  "MACROSCOPÍA")) %>%
    pull(ID)
  
  macroscopia1 <- contenido %>% filter(str_detect(text,  "MACROSCOPÍA")) %>% 
    mutate(text1 = str_sub(text, start = 12, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  
  macroscopia2 <- contenido[ubi_macroscopia+1,4]
  
  macroscopia3 <- contenido[ubi_macroscopia+2,4]
  
  macroscopia <- glue("{macroscopia1} {macroscopia2} {macroscopia3}")
  
  
  ubi_microscopia <- contenido %>% filter(str_detect(text,  "MICROSCOPÍA")) %>%
    pull(ID)
  
  microscopia1 <- contenido %>% filter(str_detect(text,  "MICROSCOPÍA")) %>% 
    mutate(text1 = str_sub(text, start = 12, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  
  microscopia2 <- contenido[ubi_microscopia+1,4]
  
  microscopia3 <- contenido[ubi_microscopia+2,4]
  
  microscopia <- glue("{microscopia1} {microscopia2} {microscopia3}")
  
  
  ubi_diagnostico <- contenido %>% filter(str_detect(text,  "DIAGNOSTICO|DIAGNÓSTICO")) %>%
    pull(ID)
  
  diagnostico1 <- contenido %>% filter(str_detect(text,  "DIAGNOSTICO|DIAGNÓSTICO")) %>% 
    mutate(text1 = str_sub(text, start = 12, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  
  diagnostico2 <- contenido[ubi_diagnostico+1,4]
  
  diagnostico3 <- contenido[ubi_diagnostico+2,4]
  
  diagnostico <- glue("{diagnostico1} {diagnostico2} {diagnostico3}")
  

  observaciones <- contenido %>% filter(str_detect(text,  "OBSERVACIONES")) %>% 
    mutate(text1 = str_sub(text, start = 16, end = nchar(text)),
           text1 = str_trim(text1, side = "both"))  %>% pull(text1)
  

  
  datos1 <- cbind(protocolo, paciente, edad, medico, fecha, procedencia, tipo, topografia, 
  clinica, metodo, macroscopia, microscopia, diagnostico, observaciones)
  
  
  datos <- rbind(datos, datos1)
  
}

datos <- datos[-1,]  # eliminamos primer fila con NA


write_excel_csv(datos, "datos.csv") 












