library(tidyverse)
library(readxl)
library(eurostat)
library(scales)

inflation_insee <- "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001764363" |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  select(TIME_PERIOD, OBS_VALUE) |>
  mutate(OBS_VALUE = as.numeric(OBS_VALUE))  |>
  arrange(TIME_PERIOD) |>
  transmute(year = as.numeric(TIME_PERIOD),
            inflation = 100*(OBS_VALUE/lag(OBS_VALUE)-1))

save(inflation_insee, file = "inflation_insee.RData")
write_csv(inflation_insee, file = "inflation_insee.csv")


