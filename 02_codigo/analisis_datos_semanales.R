### Paquetes ----
library(pacman)
p_load(ggrepel, janitor, lubridate, scales, readxl, tidyverse, treemapify, wesanderson)


### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica


### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))

tema_hm <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 32, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 25, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 25),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.88, -0.12),
        legend.direction = "horizontal",
        legend.title = element_text(size = 20, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 18, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 20),
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text.x = element_text(size = 16, face = "bold", family="Didact Gothic Regular"),
        axis.text.y = element_text(size = 12, face = "bold", family="Didact Gothic Regular"))

### Importar datos ----

# Fuente: http://estadisticashidrocarburos.energia.gob.mx/Datos_semana.aspx

bd_semanal <- read_excel("01_datos/EstadÃ_sticas EnergÃ©ticas.xlsx", sheet = "Inventarios")

bd_mensual <- read_excel("01_datos/EstadÃ_sticas EnergÃ©ticas_mensuales.xlsx", sheet = "Inventarios")


### "Limpiar" nombres de variables ----
bd_semanal <- clean_names(bd_semanal)
bd_mensual <- clean_names(bd_mensual)

### Transformaciones varias ----
bd_mensual <- bd_mensual %>% 
  mutate(mb = as.numeric(mb), # Transformar tipo de variable a numeric
         fecha = dmy_hms(fecha), # Transformar tipo de variable a dttm
         mes = fct_relevel(mes, "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")) 


bd_semanal <- bd_semanal %>% 
  mutate(mb = as.numeric(mb), # Transformar tipo de variable a numeric
         semana = dmy_hms(semana), # Transformar tipo de variable a dttm
         mes = fct_relevel(mes, "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")) 


### Gráfica: inventario semanal de gasolina en 75 terminales de almacenamiento en méxico, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento") %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#ae052b", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 10)) +
  scale_x_datetime(date_breaks = "1 week", expand = c(0, 0), date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina en 75 terminales de almacenamiento en méxico, 2018"), width = 85), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Debido al sesgo en la distribución del inventario de gasolina, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_por_terminal_log.png", path = "03_graficas/", width = 23, height = 18, dpi = 200)


### Gráfica: inventario semanal de gasolina en 75 terminales de almacenamiento en seis estados, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento",
         estado %in% c("Estado de México", "Guanajuato", "Jalisco", "Michoacán", "Querétaro", "Veracruz")) %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#ae052b", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 8)) +
  scale_x_datetime(date_breaks = "2 weeks", expand = c(0, 0), date_labels = ("%b-%d")) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_wrap(~ estado, scale = "free_y", ncol = 2) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina en las terminales de almacenamiento de seis estados, 2018"), width = 90), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Debido al sesgo en la distribución del inventario de gasolina, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(size = 30)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_por_terminal_log_estados_seleccionados.png", path = "03_graficas/", width = 23, height = 18, dpi = 200)