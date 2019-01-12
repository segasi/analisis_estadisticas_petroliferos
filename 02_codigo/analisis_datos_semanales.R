### Paquetes ----
library(pacman)
p_load(ggrepel, janitor, lubridate, scales, readxl, tidyverse, treemapify, wesanderson, zoo)


### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica


### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
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


### Generar variables para representar semana del mes en gráficas ----
bd_semanal <- 
  bd_semanal %>% 
  mutate(mes = month(semana), 
         semana_mes = ceiling((day(semana) + first_day_of_month_wday(semana) - 1) / 7),  
         semana_año = as.numeric(format(semana, "%V")),
         mes_texto = case_when(mes == 1 ~ "Ene",
                               mes == 2 ~ "Feb",
                               mes == 3 ~ "Mar",
                               mes == 4 ~ "Abr",
                               mes == 5 ~ "May",
                               mes == 6 ~ "Jun",
                               mes == 7 ~ "Jul",
                               mes == 8 ~ "Ago",
                               mes == 9 ~ "Sept",
                               mes == 10 ~ "Oct",
                               mes == 11 ~ "Nov",
                               mes == 12 ~ "Dic"),
         mes_semana = paste(mes_texto, semana_mes, sep = "-")) 

### Gráfica: inventario semanal de gasolina en 75 terminales de almacenamiento en méxico, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento") %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 1) +
  annotate(geom = "text", x = as_datetime("2018-12-18 00:00:00"), y = 67, label = "AMLO", fontface = "bold", size = 11, color = "grey30") +
  annotate(geom = "text", x = as_datetime("2018-11-22 00:00:00"), y = 67, label = "EPN", fontface = "bold", size = 11, color = "grey30") +
  scale_fill_gradient(low = "white", high = "#ae052b", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 10)) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina en 75 terminales de almacenamiento en méxico, 2018"), width = 85), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Debido al sesgo en la distribución del inventario de gasolina, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_por_terminal_log.png", path = "03_graficas/gasolina/", width = 23, height = 18, dpi = 200)


### Gráfica: inventario semanal de gasolina en terminales de almacenamiento en seis estados, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento",
         estado %in% c("Estado de México", "Guanajuato", "Jalisco", "Michoacán", "Querétaro", "Veracruz")) %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 1) +
  scale_fill_gradient(low = "white", high = "#ae052b", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 8)) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "2 weeks"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_wrap(~ estado, scale = "free_y", ncol = 2) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina en las terminales de almacenamiento de seis estados, 2018"), width = 90), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Debido al sesgo en la distribución del inventario de gasolina, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 30)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_por_terminal_log_estados_seleccionados.png", path = "03_graficas/gasolina/", width = 23, height = 18, dpi = 200)


### Gráfica: inventario semanal de gasolina de menos de 91 octanos en 75 terminales de almacenamiento en seis estados, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina < a 91 octanos",
         tipo_de_terminal == "Almacenamiento") %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "#ae052b", size = 1) +
  annotate(geom = "text", x = as_datetime("2018-12-18 00:00:00"), y = 67, label = "AMLO", fontface = "bold", size = 11, color = "grey30") +
  annotate(geom = "text", x = as_datetime("2018-11-22 00:00:00"), y = 67, label = "EPN", fontface = "bold", size = 11, color = "grey30") +
  scale_fill_gradient(low = "white", high = "#00B573", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 10)) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina de menos de 91 octanos en 75 terminales de almacenamiento en méxico, 2018"), width = 85), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Debido al sesgo en la distribución del inventario de gasolina, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_menos_91_oct_por_terminal_log.png", path = "03_graficas/gasolina/", width = 23, height = 18, dpi = 200)



### Gráfica: inventario semanal de gasolina de menos de 91 octanos en terminales de almacenamiento en seis estados, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina < a 91 octanos",
         tipo_de_terminal == "Almacenamiento",
         estado %in% c("Estado de México", "Guanajuato", "Jalisco", "Michoacán", "Querétaro", "Veracruz")) %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "#ae052b", size = 1) +
  scale_fill_gradient(low = "white", high = "#00B573", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 8)) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "2 weeks"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_wrap(~ estado, scale = "free_y", ncol = 2) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina de menos de 91 octanos en las terminales de almacenamiento de seis estados, 2018"), width = 90), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Debido al sesgo en la distribución del inventario de gasolina, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 30)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_menos_91_oct__por_terminal_log_estados_seleccionados.png", path = "03_graficas/gasolina/", width = 23, height = 18, dpi = 200)


### Gráfica: inventario semanal de gasolina de más de 91 octanos en 75 terminales de almacenamiento en seis estados, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina >= a 91 octanos",
         tipo_de_terminal == "Almacenamiento") %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "#ae052b", size = 1) +
  annotate(geom = "text", x = as_datetime("2018-12-18 00:00:00"), y = 31, label = "AMLO", fontface = "bold", size = 11, color = "grey30") +
  annotate(geom = "text", x = as_datetime("2018-11-22 00:00:00"), y = 31, label = "EPN", fontface = "bold", size = 11, color = "grey30") +
  scale_fill_gradient(low = "white", high = "#FF424E", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 10)) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina de más de 91 octanos en 75 terminales de almacenamiento en méxico, 2018"), width = 85), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Debido al sesgo en la distribución del inventario de gasolina, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_mas_91_oct_por_terminal_log.png", path = "03_graficas/gasolina/", width = 23, height = 18, dpi = 200)


### Gráfica: inventario semanal de gasolina de más de 91 octanos terminales de almacenamiento en seis estados, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina >= a 91 octanos",
         tipo_de_terminal == "Almacenamiento",
         estado %in% c("Estado de México", "Guanajuato", "Jalisco", "Michoacán", "Querétaro", "Veracruz")) %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "#ae052b", size = 1) +
  scale_fill_gradient(low = "white", high = "#FF424E", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 8)) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "2 weeks"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_wrap(~ estado, scale = "free_y", ncol = 2) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina de más de 91 octanos en las terminales de almacenamiento de seis estados, 2018"), width = 90), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Debido al sesgo en la distribución del inventario de gasolina, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 30)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_mas_91_oct__por_terminal_log_estados_seleccionados.png", path = "03_graficas/gasolina/", width = 23, height = 18, dpi = 200)

### Gráfica: número semanal de terminales de almacenamiento con cero barriles de gasolina de inventario ----
bd_semanal %>% 
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento", 
         mb == 00,
         semana > as_datetime("2018-11-01 12:00:00")) %>%
  group_by(semana) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(semana, n)) +
  geom_col(fill = "#ae052b", alpha = 0.9) +
  geom_text(aes(label = n), size = 8, color = "white", fontface = "bold", vjust = 1.7) +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 1, linetype = 2) +
  annotate(geom = "text", x = as_datetime("2018-12-07 12:00:00"), y = 10, label = "AMLO", fontface = "bold", size = 10, color = "grey50") +
  annotate(geom = "text", x = as_datetime("2018-12-01 00:00:00"), y = 10, label = "EPN", fontface = "bold", size = 10, color = "grey50") +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "2 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("número semanal de terminales de almacenamiento con inventario de cero barriles de gasolina"), width = 65),
       x = "\n", 
       y = NULL,
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj", width = 110)) +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  ggsave(filename = "num_tars_inventario_gasolina_vacio.png", path = "03_graficas/gasolina/", width = 15, height = 10, dpi = 200)



### Gráfica: número semanal de terminales de almacenamiento con cero barriles de gasolina de menos de 91 octanos de inventario ----
bd_semanal %>% 
  filter(producto == "Gasolina < a 91 octanos",
         tipo_de_terminal == "Almacenamiento", 
         mb == 00,
         semana > as_datetime("2018-11-01 12:00:00")) %>%
  group_by(semana) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(semana, n)) +
  geom_col(fill = "#00B573", alpha = 0.9) +
  geom_text(aes(label = n), size = 8, color = "white", fontface = "bold", vjust = 1.7) +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 1, linetype = 2) +
  annotate(geom = "text", x = as_datetime("2018-12-07 12:00:00"), y = 10, label = "AMLO", fontface = "bold", size = 10, color = "grey50") +
  annotate(geom = "text", x = as_datetime("2018-12-01 00:00:00"), y = 10, label = "EPN", fontface = "bold", size = 10, color = "grey50") +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "2 weeks"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("número semanal de terminales de almacenamiento con inventario de cero barriles de gasolina de menos de 91 octanos"), width = 65),
       x = "\n", 
       y = NULL,
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj", width = 110)) +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  ggsave(filename = "num_tars_inventario_gasolina_menos_91_vacio.png", path = "03_graficas/gasolina/", width = 15, height = 10, dpi = 200)


### Gráfica: número semanal de terminales de almacenamiento con cero barriles de gasolina de más de 91 octanos de inventario ----
bd_semanal %>% 
  filter(producto == "Gasolina >= a 91 octanos",
         tipo_de_terminal == "Almacenamiento", 
         mb == 00,
         semana > as_datetime("2018-11-01 12:00:00")) %>%
  group_by(semana) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(semana, n)) +
  geom_col(fill = "#FF424E", alpha = 0.9) +
  geom_text(aes(label = n), size = 8, color = "white", fontface = "bold", vjust = 1.7) +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 1, linetype = 3) +
  annotate(geom = "text", x = as_datetime("2018-12-07 12:00:00"), y = 24, label = "AMLO", fontface = "bold", size = 10, color = "grey50") +
  annotate(geom = "text", x = as_datetime("2018-12-01 00:00:00"), y = 24, label = "EPN", fontface = "bold", size = 10, color = "grey50") +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "2 weeks"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("número semanal de terminales de almacenamiento con inventario de cero barriles de gasolina de más de 91 octanos"), width = 65),
       x = "\n", 
       y = NULL,
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj", width = 110)) +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank()) +
  ggsave(filename = "num_tars_inventario_gasolina_mas_91_vacio.png", path = "03_graficas/gasolina/", width = 15, height = 10, dpi = 200)  

