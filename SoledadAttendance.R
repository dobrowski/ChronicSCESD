

library(tidyverse)
library(readxl)
library(here)


soledad <- read_xlsx(here("data", "Soledad.xlsx")) %>%
    mutate(district_name = "Soledad")






graph2023 <- soledad %>% bind_rows(scesd.graph) %>% bind_rows(ausd.2023) %>% bind_rows(north2023)


write_rds(graph2023, here("Chronic", "graph2023.rds") )


