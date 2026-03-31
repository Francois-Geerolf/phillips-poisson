library(tidyverse)
library(ggrepel)

load("macro_history.RData")

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

ggsave("plot.pdf")
