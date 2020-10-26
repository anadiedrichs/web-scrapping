library(rvest)
library(httr)
library(tidyverse)
library(lubridate)

set_config( config( ssl_verifypeer = 0L ))

tabla <- GET("https://www.contingencias.mendoza.gov.ar/instantaneas/") %>%
  read_html() %>%  
  html_table(fill=TRUE)  %>%  
  .[[2]]

# chequear que regreso algo
if(is.null(tabla) | is.na(tabla)){stop("Tabla no puede ser null o NA")}
if( !is.data.frame(tabla)) { stop("Tabla no es data.frame")}

# chequear que son 5 columnas
if(ncol(tabla) != 5) { stop("Nro columnas != de 5, cambio el formato de los datos.")}

# nombres de columnas
# la primer fila es el nombre de los campos
colnames(tabla) <- tabla[1,]
tabla <- tabla[-1,]


# lo siguiente no funciona
#tabla %>% select(Nombre) %>%  filter(starts_with("Ju"))


# ignorar estacion Cuadro Benegas
df <- tabla %>% 
  janitor::clean_names() %>% 
  mutate(p_rocio = if_else(p_rocio == "no disp.",NA_character_,p_rocio),
         temperatura = if_else(temperatura == "no disp.",NA_character_,temperatura),
         humedad = if_else(humedad == "no disp.",NA_character_,humedad)) %>% 
#  separate(col = hora,into = c("fecha","hora"),sep = " ") %>% 
  mutate(tiempo = parse_date_time(hora,"%d/%m/%Y %H%M")) %>%
  mutate(p_rocio = as.numeric(p_rocio),
         temperatura = as.numeric(temperatura),
         humedad = as.numeric(humedad))


#glimpse(df)

# Junin cambiarle el nombre, 
# cadena Junin como la reemplazo 
df[which(str_starts(df$nombre,"Jun")),"nombre"] <- "Junin"

# voy guardando, just in case...
write.table(df,file=str_c(now(),"-tablita.csv"),sep=",")
