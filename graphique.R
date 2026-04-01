library(tidyverse)
library(ggrepel)
library(sf)
library(akima)
#pak::pak("ofce/ofce")
library(ofce)

load("inflation_chomage.rds")

palette1 <- c("#440154FF",
              "#31688EFF",
              "#35B779FF",
              "#FDE725FF")

palette2 <- c(
  "#003049",
  "#2EC4B6",
  "#80ED99",
  "#FFD166"
)

# -----------------------------
# Oeil
# -----------------------------
eye_x <- 0.13
eye_y <- 0.15

eye_white  <- tibble(x = eye_x, y = eye_y)
eye_pupil  <- tibble(x = eye_x, y = eye_y)
eye_reflect <- tibble(x = eye_x + 0.002, y = eye_y + 0.005)

# -----------------------------
# 1. Données
# -----------------------------
df <- inflation_chomage |>
  filter(!is.na(chomage)) |>
  transmute(year,
            x = chomage/100,
            y = inflation/100) %>%
  drop_na()

# -----------------------------
# 2. Poisson (superellipse)
# -----------------------------
t <- seq(0, 2*pi, length.out = 1000)
n <- 2.1

fish <- tibble(
  x = sign(cos(t)) * abs(cos(t))^(2/n),
  y = 0.6 * sign(sin(t)) * abs(sin(t))^(2/n)
) %>%
  mutate(x = ifelse(x > 0, x^1.2, x * 1.1))

range_x <- range(df$x)
range_y <- range(df$y)

fish_scaled <- fish %>%
  mutate(
    x = scales::rescale(x, to = range_x),
    y = scales::rescale(y, to = range_y)
  )

# -----------------------------
# 3. Interpolation fine
# -----------------------------
interp_data <- with(fish_scaled,
                    akima::interp(x, y, z = y,
                                  duplicate = "mean",
                                  nx = 400, ny = 400))

grid <- expand.grid(
  x = interp_data$x,
  y = interp_data$y
)

grid$z <- as.vector(interp_data$z)

# -----------------------------
# 4. CLIPPING avec sf
# -----------------------------
fish_coords <- fish_scaled[, c("x", "y")]
fish_coords <- rbind(fish_coords, fish_coords[1, ])

fish_sf <- st_polygon(list(as.matrix(fish_coords))) %>%
  st_sfc()

grid_sf <- st_as_sf(grid, coords = c("x", "y"))

grid_clipped <- grid_sf[fish_sf, ]

coords <- st_coordinates(grid_clipped)

grid_clipped <- grid_clipped %>%
  mutate(x = coords[,1],
         y = coords[,2])

# -----------------------------
# 5. Queue
# -----------------------------
height_tail <- 16
width_tail  <- 2

tail <- tibble(
  x = c((1 - width_tail)/100, 1/100, (1 - width_tail)/100),
  y = c((6.38 - height_tail)/100,
        6.38/100,
        (6.38 + height_tail)/100)
)

# -----------------------------
# 6. Plot final
# -----------------------------


ggplot() +
  # theme_minimal() +
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
    labels = scales::percent_format(accuracy = 1) ,
    expand = expansion(add = c(0.05,0.05))
  ) +
  labs(
    title   = "La courbe de Phillips n'est pas morte !",
    subtitle = "Réalisé par François Geerolf",
    x = "Taux de chômage",
    y = "Taux d'inflation"
  ) +
  
  annotate("text" , x = -0.01, y = 0.35, label = "Estimation non linéaire de la courbe de Phillips pour la France (1895–2025).\nModèle piscicole: fonction superelliptique", hjust = 0, vjust = 0,
           size = 3, family = "Open Sans") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graphique.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)
ggsave("graphique.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
