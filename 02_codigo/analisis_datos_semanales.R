### Paquetes ----
library(pacman)
p_load(ggrepel, janitor, scales, readxl, tidyverse, treemapify, wesanderson)


### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica
