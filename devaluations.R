library(tidyverse)
library(ggrepel)
library(gt)

devaluations_fr <- tibble::tribble(
  ~date,        ~devaluation_pct, ~regime,                ~nom,                     ~raison,
  "1928-06-25", -80,             "Troisième République", "Franc Poincaré",        "Stabilisation après inflation et dette WWI, retour à l'or",
  "1936-10-01", -35,             "Troisième République", "Franc Auriol",          "Réponse aux dévaluations UK/US et pression sur le franc",
  "1937-06-30", NA,              "Troisième République", "Franc Bonnet",          "Nouvelle pression externe et instabilité politique",
  "1938-05-04", -10,             "Troisième République", "Franc Marchandeau",     "Ajustement compétitif dans contexte international",
  "1938-11-12", NA,              "Troisième République", "Franc Reynaud",         "Abandon de l'or, ancrage à la livre pour compétitivité",
  "1940-02-29", NA,              "Troisième République", "Franc Reynaud",         "Financement de guerre et allègement de la dette",
  "1940-01-01", NA,              "Régime de Vichy",      "Taux imposé allemand",  "Taux de change imposé par l'occupation",
  "1944-09-06", -13,             "Quatrième République", "Franc Mendès France",   "Réajustement post-Libération",
  "1945-12-26", -60,             "Quatrième République", "Franc Pleven",          "Bretton Woods, relance exportations et inflation",
  "1948-01-20", -80,             "Quatrième République", "Franc Mayer",           "Déséquilibres externes et pression sur dollar",
  "1949-09-20", -22.27,          "Quatrième République", "Franc Queuille",        "Ajustement externe face au dollar",
  "1957-08-10", -20,             "Quatrième République", "Franc Gaillard",        "Dévaluation déguisée pour restaurer compétitivité",
  "1958-06-01", -20,             "Cinquième République", "De Gaulle",             "Redressement économique et crédibilité",
  "1958-12-29", -17.55,          "Cinquième République", "Franc Pinay-Rueff",     "Stabilisation + création du nouveau franc",
  "1969-08-08", -11.1,           "Cinquième République", "Franc Giscard",         "Ajustement compétitif + réévaluation du mark",
  "1981-10-04", -3,              "Cinquième République", "Mitterrand I",          "Début des tensions macro post-relance",
  "1982-06-12", -5.75,           "Cinquième République", "Mitterrand II",         "Tournant de la rigueur + coordination européenne",
  "1983-03-21", -8,              "Cinquième République", "SME ajustement",        "Maintien dans le SME malgré divergences",
  "1986-04-06", -3,              "Cinquième République", "SME ajustement",        "Réajustement des parités dans le SME",
  "1987-01-11", -3,              "Cinquième République", "SME implicite",         "Dévaluation via réévaluation partenaires"
)

devaluations_fr <- devaluations_fr %>%
  mutate(regime = recode(regime,
                         "Troisième République" = "3ème République",
                         "Quatrième République" = "4ème République",
                         "Cinquième République" = "5ème République"
  ))

devaluations_fr

save(devaluations_fr, file = "devaluations_fr.RData")
write_csv(devaluations_fr, file = "devaluations_fr.csv")


years_post_deval <- devaluations_fr %>%
  mutate(year = year(date)) %>%
  mutate(year_post = year + 1) %>%
  pull(year_post)

years_post_deval

# Tableau dévaluations ------

tab <- devaluations_fr %>%
  mutate(date = as.Date(date)) %>%
  gt() %>%
  fmt_date(columns = date, date_style = 1) %>%
  fmt_number(columns = devaluation_pct, decimals = 2) %>%
  cols_label(
    date = "Date",
    devaluation_pct = "Dévaluation (%)",
    regime = "Régime",
    nom = "Nom",
    raison = "Raison"
  )
tab

gtsave(tab, "devaluations.pdf")
gtsave(tab, "devaluations.png")

# 



load("inflation_chomage.rds")

inflation_chomage <- inflation_chomage %>%
  mutate(post_devaluation = year %in% years_post_deval)


inflation_chomage %>%
  na.omit %>%
  ggplot() + 
  theme_minimal() + 
  geom_point(aes(x = chomage/100, 
                 y = inflation/100, 
                 color = post_devaluation)) +
  xlab("Unemployment Rate") + 
  ylab("Inflation") +
  scale_x_continuous(breaks = 0.01*seq(-10, 30, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = 0.01*seq(-20, 100, 5),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("FALSE" = "grey70", 
                                "TRUE" = "red")) +
  geom_text_repel(aes(x = chomage/100, 
                      y = inflation/100, 
                      label = year), size = 2) +
  theme(legend.position = "none")

ggsave("scatter-devaluations.pdf")
ggsave("scatter-devaluations.png")
