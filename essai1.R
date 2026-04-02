library(tidyverse)
library(ggrepel)

# =========================
# 📊 DATA
# =========================

load("inflation_chomage.rds")

inflation_chomage <- inflation_chomage %>%
  na.omit() %>%
  mutate(
    periode = case_when(
      year < 1914                        ~ "1895–1913 (Belle Époque)",
      year >= 1914 & year < 1945         ~ "1914–1944 (Guerres mondiales)",
      year >= 1945 & year < 1974         ~ "1945–1973 (Trente Glorieuses)",
      year >= 1974 & year < 1984         ~ "1974–1983 (Stagflation)",
      year >= 1984 & year < 2008         ~ "1984–2007 (Grande Modération)",
      year >= 2008 & year < 2021         ~ "2008–2020 (Crises & Austérité)",
      year >= 2021                       ~ "2021–2025 (Retour de l'inflation)",
    ),
    periode = factor(periode, levels = c(
      "1895–1913 (Belle Époque)",
      "1914–1944 (Guerres mondiales)",
      "1945–1973 (Trente Glorieuses)",
      "1974–1983 (Stagflation)",
      "1984–2007 (Grande Modération)",
      "2008–2020 (Crises & Austérité)",
      "2021–2025 (Retour de l'inflation)"
    ))
  )

# =========================
# 🐟 POISSON (LOESS RÉALISTE)
# =========================

df_fish <- inflation_chomage %>%
  transmute(
    unemployment = chomage / 100,
    inflation    = inflation / 100
  ) %>%
  drop_na()

fit <- loess(inflation ~ unemployment, data = df_fish, span = 0.6)

grid <- tibble(
  unemployment = seq(min(df_fish$unemployment),
                     max(df_fish$unemployment),
                     length.out = 400)
)

grid$y <- predict(fit, newdata = grid)

# Profil poisson (plus fin aux extrémités)
thickness <- 0.035
profile <- sin(seq(0, pi, length.out = nrow(grid)))^0.7

upper <- grid$y + thickness * profile
lower <- grid$y - thickness * profile

fish_body <- tibble(
  x = c(grid$unemployment, rev(grid$unemployment)),
  y = c(upper, rev(lower))
)

# =========================
# 🐟 TÊTE
# =========================

theta <- seq(0, 2*pi, length.out = 100)

head_center_x <- max(grid$unemployment)
head_center_y <- grid$y[nrow(grid)]

fish_head <- tibble(
  x = head_center_x + 0.01 * cos(theta),
  y = head_center_y + 0.02 * sin(theta)
)

# =========================
# 🐟 QUEUE
# =========================

tail_x <- min(grid$unemployment)
tail_y <- grid$y[1]

fish_tail <- tibble(
  x = c(tail_x,
        tail_x - 0.02,
        tail_x - 0.02),
  y = c(tail_y,
        tail_y + 0.04,
        tail_y - 0.04)
)

# =========================
# 🐟 NAGEOIRE
# =========================

mid <- floor(nrow(grid) * 0.6)

fish_fin <- tibble(
  x = c(grid$unemployment[mid - 10],
        grid$unemployment[mid],
        grid$unemployment[mid + 10]),
  y = c(grid$y[mid],
        grid$y[mid] + 0.05,
        grid$y[mid])
)

# =========================
# 🐟 ŒIL
# =========================

fish_eye <- tibble(
  x = head_center_x + 0.005,
  y = head_center_y + 0.005
)

# =========================
# 🎨 COULEURS
# =========================

tropical_colors <- c(
  "1895–1913 (Belle Époque)"           = "#00A878",
  "1914–1944 (Guerres mondiales)"      = "#E91E8C",
  "1945–1973 (Trente Glorieuses)"      = "#00C9D4",
  "1974–1983 (Stagflation)"            = "#FF6B35",
  "1984–2007 (Grande Modération)"      = "#FFD700",
  "2008–2020 (Crises & Austérité)"     = "#9B5DE5",
  "2021–2025 (Retour de l'inflation)"  = "#FF1744"
)

# =========================
# 📈 PLOT
# =========================

inflation_chomage %>%
  ggplot(aes(
    x     = chomage / 100,
    y     = inflation / 100,
    color = periode,
    label = year
  )) +
  
  # 🐟 Corps
  geom_polygon(
    data = fish_body,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    fill = "#FF8C42",
    alpha = 0.2
  ) +
  
  # 🐟 Tête
  geom_polygon(
    data = fish_head,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    fill = "#FF8C42",
    alpha = 0.2
  ) +
  
  # 🐟 Queue
  geom_polygon(
    data = fish_tail,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    fill = "#FF8C42",
    alpha = 0.2
  ) +
  
  # 🐟 Nageoire
  geom_polygon(
    data = fish_fin,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    fill = "#FF8C42",
    alpha = 0.2
  ) +
  
  # 🐟 Œil
  geom_point(
    data = fish_eye,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    color = "black",
    size = 1.5,
    alpha = 0.4
  ) +
  
  # Données
  geom_point(size = 2.5, alpha = 0.85) +
  
  geom_text_repel(
    size          = 2.8,
    max.overlaps  = 20,
    segment.size  = 0.3,
    segment.alpha = 0.4,
    show.legend   = FALSE
  ) +
  
  scale_color_manual(values = tropical_colors) +
  
  scale_x_continuous(
    breaks = 0.01 * seq(0, 20, 2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  scale_y_continuous(
    breaks = 0.01 * seq(-20, 40, 5),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  coord_cartesian(
    xlim = c(min(df_fish$unemployment) - 0.03,
             max(df_fish$unemployment) + 0.01),
    ylim = c(min(df_fish$inflation),
             max(df_fish$inflation) + 0.05)
  ) +
  
  labs(
    title   = "Courbe de Phillips — France (1895–2025)",
    subtitle = "Estimation non linéaire piscicole de la courbe de Phillips 🐟",
    x       = "Taux de chômage",
    y       = "Taux d'inflation",
    caption = "Sources: IN-SEA, Maquereau History Database, calculs had-hock"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "none",
    plot.title        = element_text(face = "bold", size = 14),
    plot.caption      = element_text(size = 9, color = "#888888")
  )

ggsave("essai1.pdf", width = 1.25*6, height = 1.25*3.375)
ggsave("essai1.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
