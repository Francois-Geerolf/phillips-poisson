library(tidyverse)
library(readxl)
library(eurostat)
library(scales)

## Load Eurostat datasets ------
# Sources: IN SEA, 

datasets_eurostat <- c("prc_hicp_aind", "une_rt_a")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, stringsAsFactors = F, cache = F) |>
           rename(date = TIME_PERIOD)
  )
}

chomage_eurostat <- une_rt_a %>%
  filter(geo %in% c("FR"),
         age == "Y15-74",
         sex == "T",
         unit == "PC_ACT") %>%
  select_if(~ n_distinct(.) > 1) %>%
  transmute(year = year(date),
            chomage = values)

save(chomage_eurostat, file = "chomage_eurostat.RData")
write_csv(chomage_eurostat, file = "chomage_eurostat.csv")
  

inflation_eurostat <- prc_hicp_aind %>%
  filter(coicop == "CP00",
         geo == "FR",
         unit == "RCH_A_AVG") %>%
  select_if(~ n_distinct(.) > 1)

save(inflation_eurostat, file = "inflation_eurostat.RData")
write_csv(inflation_eurostat, file = "inflation_eurostat.csv")

