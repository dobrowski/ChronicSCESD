
# Combine the districts



graph2023 <- soledad.2023 %>% bind_rows(scesd.2023.rev) %>% bind_rows(ausd.2023) %>% bind_rows(nomo.2023.rev)


write_rds(graph2023, here("Chronic", "graph2023.rds") )




### Option for only selected grades -----

graph2023.swd <- soledad.2023 %>% bind_rows(nomo.2023.rev) %>% bind_rows(scesd.2023.rev) %>% bind_rows(ausd.2023)
