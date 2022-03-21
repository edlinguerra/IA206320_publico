#Comandos para occurrences

library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)

#Datos proporcionados or Ricardo Olmos
macro_raw <- read_excel("dat.Abundancias.Playas.PAPIIT.xls")

macro <- macro_raw %>% 
  pivot_longer(cols = 2:60, names_to = "scientificName", values_to = "measurementValue") %>% 
  mutate(site = as.character(site)) %>% 
  filter(measurementValue != 0)


occurrence <- inner_join(x = macro, y = eventos) %>% 
  mutate(eventID = str_c("Mex","Yucatan",locality, site, zone, as.character(date(eventDate)), sep = "-"))
