library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)

#combinación de archivos de Variables ambientales
#Datos de planillas
amb_raw <- read_excel("datos/ambientales_papiit.xlsx")

#Granulometría resumida en formato D50
gran <- read_excel("datos/ambientales2_papiit.xlsx", sheet = "Granulometria")

#Macrofitos de deriva
mac <- read_excel("datos/ambientales2_papiit.xlsx", sheet = "Macrofitos")

#Perfil de playa
amb2 <- read_excel("datos/ambientales2_papiit.xlsx", sheet = "Ambientales_transf")

#Indice de urbanización
UI <- read_excel("datos/ambientales2_papiit.xlsx", sheet = "UI")

#Prepraración de datos de planilla
amb <- amb_raw %>%
  filter (variable != "date", 
          variable != "start",
          variable != "end") %>%
  mutate(valor = as.numeric(value)) %>%
  select(-value)%>%
  pivot_wider(names_from = variable, values_from = valor, values_fn = mean) %>%
  relocate(Latitude, .after = site) %>%
  relocate(Longitude, .after = Latitude) %>%
  relocate(Salinity, .after = zone)

amb$month <- factor(amb$campaign, levels = c(1,2,3), labels = c("Nov","Apr","Aug"))

amb <- amb%>%
  mutate(site = as.character(site)) %>%
  mutate(ID = str_c(month, locality, site, zone)) %>%
  mutate(Longitude = round(Longitude, 5))

#Preparación de perfil
amb3 <- amb2 %>%
  mutate(site = str_sub(site, 2,2))%>%
  rename("Dry" = "S_sup",
         "Wet" = "S_int")%>%
  pivot_longer(cols = c(7,8), names_to = "zone", values_to = "slope")%>%
  mutate(ID = str_c(month, locality, site, zone))%>%
  select(-c(Grain_size, Sample))

#Preparación de granulometria
gran <- gran %>%
  mutate(Grain_size = D50_m/1000)%>%
  mutate(site = str_sub(site, 2,2))

#Preparacion de UI
UI <- UI %>%
  mutate(site = str_sub(site, 2,2))


#Combinacion de matrices
amb4 <- inner_join(gran, amb3)
amb5 <- inner_join(amb, amb4)
amb6 <- inner_join(UI, amb5, by = c("locality", "site", "zone"))

#organizacion en formato PRIMER

amb7 <- amb6 %>%
  select(c(
    Latitude, Longitude, UI, Birds, Visitors,
    Azimut_t, Temperature, Dureza,Wrack,
    Grain_size, Salinity, slope,Beach_width,
    Swash,S_total,pH,
    campaign,month,locality,site,zone)
  ) %>% 
  rename(decimalLatitude = Latitude,
         decimalLongitude = Longitude)

write_csv(amb7, file = "datos/ambientales_curado.csv", na = "")

# Comandos para fechas
#Datos de planillas
fecha_raw <- read_excel("datos/fechas.xlsx", sheet = "fechas")
horas_raw <- read_excel("datos/fechas.xlsx", sheet = "horas")

#Prepraración de datos de planilla
fecha <- fecha_raw %>%
  pivot_wider(names_from = variable, values_from = value) 

hora <- horas_raw %>%
  pivot_wider(names_from = variable, values_from = value)

eventDate <- inner_join(fecha, hora) %>% 
  mutate(x1 = str_c(hour(start), minute(start), second(start), sep = ":")) %>% 
  mutate(eventDate = as_datetime(str_c(date(date), x1, sep = " "))) %>%
  mutate(site = as.character(site)) %>% 
  select(-c(x1, date, start, end, zone))

#Combinar eventDate con Latitude y Longitud de la matriz ambiental curada
eventos <- inner_join(amb7, eventDate) %>% 
  select(decimalLatitude, decimalLongitude, eventDate, campaign, locality, site, zone)


#Comandos para occurrences, Datos proporcionados por Ricardo Olmos
macro_raw <- read_excel("datos/dat.Abundancias.Playas.PAPIIT.xls")

macro <- macro_raw %>% 
  pivot_longer(cols = 2:60, names_to = "scientificName", values_to = "measurementValue") %>% 
  mutate(site = as.character(site)) %>% 
  filter(measurementValue != 0)

occurrence <- inner_join(x = macro, y = eventos) %>% 
  mutate(eventID = str_c("Mex","Yucatan",
                         locality,
                         site,
                         zone,
                         as.character(date(eventDate)), sep = "-"))
