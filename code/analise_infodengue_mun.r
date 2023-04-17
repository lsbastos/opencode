library(tidyverse)
library(lubridate)

# Geocode do Rio 3304557 (Código de 7 dígitos do IBGE)
geocode = 3304557

# Código para pegar os dados da API do infodengue de 2010 até 2023 por semana
url <- paste0("https://info.dengue.mat.br/api/alertcity?",
              "geocode=", geocode, "&disease=dengue&format=csv&ew_start=1&",
              "ew_end=53&ey_start=2010&ey_end=2023")

# baixando os dados
dadosRio <- read_csv(file = url) 

dadosRio %>% 
  mutate(
    ew = epiweek(data_iniSE),
    ey = epiyear(data_iniSE)
  ) %>% 
  # Filtrando até a semana 14
  filter( ew <= 14) %>% 
  # Agrupando por ano
  group_by(ey) %>% 
  reframe(
    casosAno = sum(casos)
  ) %>% 
  summarise(
    # Média ultimos 10 anos
    meanH10y = mean(casosAno[ey<2023 & ey >= 2013]),
    # Média ultimos 5 anos
    meanH5y = mean(casosAno[ey<2023 & ey >= 2018]),
    # Casos em 2022
    casos2022 = casosAno[ey == 2022],
    # Casos em 2023
    casos2023 = casosAno[ey == 2023],
  )


dadosRio %>% 
  mutate(
    ew = epiweek(data_iniSE),
    ey = epiyear(data_iniSE)
  ) %>% 
  filter( ew <= 14) %>% 
  ggplot(aes(x = ew, 
             y = casos, 
             group = ey, color = as.character(ey))) + 
  geom_line() + 
  theme_bw() + 
  labs(
    x = "Semana", 
    y = "Casos notificados", 
    color = "Ano", 
    title = "Casos notificados até a semana 14"
  )

dadosRio %>% 
  mutate(
    ew = epiweek(data_iniSE),
    ey = epiyear(data_iniSE)
  ) %>% 
  filter( ew <= 14) %>% 
  group_by(ey) %>% 
  reframe(
    casosAno = sum(casos)
  ) %>% ggplot() + 
  geom_col(aes(x = as.character(ey), 
               y = casosAno, 
               fill = as.character(ey)), 
           show.legend = F) + 
  theme_bw() +  
  labs(
    x = "Ano", 
    y = "Casos notificados", 
    fill = "Ano", 
    title = "Acumulado de casos notificados até a semana 14"
  )

dadosRio %>% 
  mutate(
    ew = epiweek(data_iniSE),
    ey = epiyear(data_iniSE)
  ) %>% 
  filter( ey == 2023) %>% 
  ggplot(aes(x = ew, 
             y = casos, 
             color = "Notificações")) + 
  geom_line() + 
  geom_line(aes(y = casos_est, color = "Nowcasting")) + 
  geom_line(aes(y = casos_est_min, color = "Nowcasting"), linetype = "dashed") + 
  geom_line(aes(y = casos_est_max, color = "Nowcasting"), linetype = "dashed") + 
  scale_x_continuous(breaks = 1:14, labels  = 1:14) +
  theme_bw() + 
  labs(
    x = "Semana", 
    y = "Casos", 
    color = "", 
    title = "Casos notificados até a semana 14"
  ) + 
  theme(legend.position = c(0.1,0.8))
