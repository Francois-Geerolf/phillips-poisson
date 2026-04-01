library(tidyverse)
library(readxl)
library(curl)

# Données maquereau history ---------

curl_download("https://www.macrohistory.net/app/download/9834512569/JSTdatasetR6.xlsx",
              destfile = "JSTdatasetR6.xlsx")

JSTdatasetR6 <- read_xlsx("JSTdatasetR6.xlsx") |>
  filter(iso == "FRA") |>
  transmute(year, chomage = unemp, 
            inflation = 100*(cpi/lag(cpi, 1)-1))

# Données IN-SEA ---------

inflation_insee <- "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001764363" |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  select(TIME_PERIOD, OBS_VALUE) |>
  mutate(OBS_VALUE = as.numeric(OBS_VALUE))  |>
  arrange(TIME_PERIOD) |>
  transmute(year = as.numeric(TIME_PERIOD),
            inflation = 100*(OBS_VALUE/lag(OBS_VALUE)-1))

chomage_insee <-"https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001688526" |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  select(TIME_PERIOD, OBS_VALUE) |>
  arrange(TIME_PERIOD) |>
  transmute(year = as.numeric(substr(TIME_PERIOD, 1, 4)),
            chomage = as.numeric(OBS_VALUE)) %>%
  group_by(year) |>
  summarise(chomage = mean(chomage))

inflation_chomage_insee <- inflation_insee |>
  left_join(chomage_insee, by = "year")

inflation_chomage <- inflation_chomage_insee |>
  filter(year > 2020)  |>
  bind_rows(JSTdatasetR6) |>
  arrange(year)

save(inflation_chomage, file = "inflation_chomage.rds")
  
