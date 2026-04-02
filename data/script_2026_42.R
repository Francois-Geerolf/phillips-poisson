library(tidyverse)
library(readxl)
library(ggrepel)
library(curl)

# Données maquereau history ---------

curl_download("https://www.macrohistory.net/app/download/9834512569/JSTdatasetR6.xlsx",
              destfile = "JSTdatasetR6.xlsx")

JSTdatasetR6 <- read_xlsx("JSTdatasetR6.xlsx") |>
  filter(iso == "FRA") |>
  transmute(year, chomage = unemp, 
            inflation = 100*(cpi/lag(cpi, 1)-1)) |>
  filter(!is.na(chomage))

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

save(inflation_chomage, file = "relecture/data/data_2026_42.rds")


unlink("JSTdatasetR6.xlsx")


eye_x <- 0.13
eye_y = 0.15

eye_white <- tibble(x = eye_x, y = eye_y)
eye_pupil <- tibble(x = eye_x, y = eye_y)
eye_reflect <- tibble(x = eye_x+0.002, y = eye_y+0.005)
# -----------------------------
# 5. Plot
# -----------------------------
ggplot() +
  theme_minimal() +
  geom_polygon(data = tail,
               aes(x = x, y = y),
               fill = "#2EC4B6",
               color = palette1[1],
               linewidth = 1.2) +
  
  geom_raster(data = grid_clipped,
              aes(x = x, y = y, fill = z)) +
  
  geom_polygon(data = fish_scaled,
               aes(x = x, y = y),
               fill = NA,
               color = palette1[1],
               size = 1.3) +
  
  geom_point(data = df,
             aes(x = x, y = y),
             color = "black") +
  
  geom_text_repel(
    data = df,
    aes(x = x, y = y, label = year),
    size = 2,
    max.overlaps = Inf,
    box.padding = 0.2,
    point.padding = 0.1,
    force = 1.5,          # ← écarte davantage
    segment.size = 0.2,   # ← traits plus fins
    segment.alpha = 0.5
  ) +
  
  scale_fill_gradientn(colors = palette2) +
  # oeil (blanc)
  geom_point(data = eye_white,
             aes(x, y),
             size = 6,
             color = "white") +
  
  # pupille
  geom_point(data = eye_pupil,
             aes(x, y),
             size = 3,
             color = "black") +
  
  # reflet (petit highlight)
  geom_point(data = eye_reflect,
             aes(x, y),
             size = 1,
             color = "white") +
  scale_x_continuous(
    breaks = 0.01 * seq(0, 20, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = 0.01 * seq(-20, 40, 5),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title   = "La courbe de Phillips n'est pas morte !",
    subtitle = "Estimation non linéaire de la courbe de Phillips pour la France (1895–2025).\nModèle piscicole: fonction superelliptique\nRéalisé par François Geerolf"
  ) +
  
  theme(legend.position = "none") +
  ofce_caption(
    
    note = "Le calcul est effectué sur 12 mois glissants : chaque trimestre, la capacité de financement de la France (balance courante) ou celle des administrations publiques (solde public), cumulée sur les quatre derniers trimestres, est rapportée au PIB cumulé sur la même période. Sur une base trimestrielle, la dernière observation disponible pour le quatrième trimestre 2025 ferait apparaître une balance courante excédentaire d’environ 1,5 % du PIB. Le niveau définitif du déficit public pour 2025 ne sera connu qu’à la fin du mois de mars ; dans l’intervalle, on retient la prévision du gouvernement, qui table sur un déficit public de 5,4 % du PIB pour l’ensemble de l’année 2025." ,
    x       = "Taux de chômage",
    y = "Taux d'inflation",
    source = "Maquereau History Database (1895-2020), Données IN-SEA (2021-2025), ajout had-hock.",
    note = ""
  ) +
  logo_ofce()
