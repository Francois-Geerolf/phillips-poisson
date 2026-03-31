library(tidyverse)
library(readxl)
library(ggrepel)

JSTdatasetR6 <- read_xlsx("macro_history.xlsx")
load("inflation_insee.RData")
load("chomage_eurostat.RData")

post_2020 <- inflation_insee %>%
  filter(year > 2020) %>%
  left_join(chomage_eurostat, by = "year")

# Sources: IN SEA, Maquereau history database, calculs had-hock🐟📊

macro_history <- JSTdatasetR6 %>%
  filter(iso == "FRA") %>%
  transmute(year, chomage = unemp, 
            inflation = 100*(cpi/lag(cpi, 1)-1)) %>%
  na.omit %>%
  bind_rows(post_2020)



save(macro_history, file = "macro_history.RData")
write_csv(macro_history, file = "macro_history.csv")

macro_history %>%
  na.omit %>%
  ggplot(.) + theme_minimal() + 
  geom_point(aes(x = chomage/100, y = inflation/100)) +
  xlab("Unemployment Rate") + ylab("Inflation") +
  scale_x_continuous(breaks = 0.01*seq(-10, 30, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = 0.01*seq(-20, 100, 5),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_color_viridis_d(option="C", name = "Period", begin = 0.15, end = 0.85)+
  #stat_smooth(aes(x = chomage/100, y = inflation/100), linetype=2, method="lm") + 
  geom_text_repel(aes(x = chomage/100, y = inflation/100, label = year), size = 2) +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.9))

ggsave("JSTdatasetR6_FRA.pdf")
write_csv(JSTdatasetR6_FRA, file = "JSTdatasetR6_FRA.csv")

JSTdatasetR6 %>%
  filter(iso == "GBR") %>%
  select(year, chomage = unemp, cpi) %>%
  mutate(inflation = 100*(cpi/lag(cpi, 1)-1)) %>%
  filter(inflation <= 100) %>%
  select(year, chomage, inflation) %>%
  na.omit %>%
  ggplot(.) + theme_minimal() + 
  geom_point(aes(x = chomage/100, y = inflation/100)) +
  xlab("Unemployment Rate") + ylab("Inflation") +
  scale_x_continuous(breaks = 0.01*seq(-10, 30, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = 0.01*seq(-20, 100, 5),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_color_viridis_d(option="C", name = "Period", begin = 0.15, end = 0.85)+
  #stat_smooth(aes(x = chomage/100, y = inflation/100), linetype=2, method="lm") + 
  geom_text_repel(aes(x = chomage/100, y = inflation/100, label = year), size = 2) +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.9))

ggsave("JSTdatasetR6_GBR.pdf")
  
