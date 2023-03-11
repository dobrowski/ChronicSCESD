

# To read from SCESD manually entered excel sheets


library(tidyverse)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(MCOE)


root <- "https://drive.google.com/drive/folders/1W2WiU-tg6L8Vz3JLwvnLH0Q_AWqSjuI2"


find.list <- drive_ls(root, recursive = TRUE) %>%
    filter(str_detect(name, "School Wide")) %>%
    mutate(n = row_number()) 


find.list.long <- find.list.long %>%
    filter(name == "")

for (i in 1:15) {
    
schooler <- find.list[i,1]   %>% unlist 

temp <- find.list %>%
    filter(n == i) %>%
    read_sheet(sheet = "Culture & Climate", range = "A32:L40") %>%
    pivot_longer(cols = c(August:June)) %>%
    mutate(School = schooler)

find.list.long <- bind_rows(find.list.long, temp)
    
}

scesd.2023 <- find.list.long %>%
    mutate(School = str_remove(School, " School Wide Dashboard"),
           district_name = "Salinas City",
           school_name = case_when(School == "K" ~ "Henry F. Kammann Elementary",
                                   School == "BM" ~ "Boronda Meadows",
                                   School == "LP" ~ "Los Padres Elementary",
                                   School == "LV" ~ "Loma Vista Elementary",
                                   School == "MiP" ~ "Mission Park Elementary",
                                   School == "LIN" ~ "Lincoln Elementary",
                                   School == "MoP" ~ "Monterey Park Elementary",
                                   School == "LW" ~ "Laurel Wood Elementary",
                                   School == "EG" ~ "El Gabilan Elementary",
                                   School == "DIAS" ~ "Boronda Elementary",
                                   School == "N" ~ "Natividad Elementary",
                                   School == "SHR" ~ "Sherwood Elementary",
                                   School == "UP" ~ "University Park Elementary",
                                   School == "SCVA" ~ "Salinas City Virtual Academy",
                                   School == "R" ~ "Roosevelt Elementary",
                                   ),
           month.date = match(name, month.name),
           year.date = if_else(month.date<=6, 2023, 2022),
           dater = mdy(paste( month.date,"1",year.date , sep = "/" ) ),
           definition = Month,
           chronic.rate = value/100
           ) 


scesd.graph <- scesd.2023 %>%
    filter(name == "January") %>%
    select(district_name, school_name, definition, chronic.rate) %>%
    add_case(district_name = "Salinas City",
             school_name = "District Office",
             definition = "Over all",
             chronic.rate = .288)






###. other stuff -----


scesd.2023 %>%
    filter(name == "January",
           district_name == "Salinas City Elementary",
           definition == "Over all" ) %>%
chronic_graph_percent(indi = value/100  , xxx = school_name, tit = "Chronic Absenteeism", subtit = "Current Year through January")



scesd.2023 %>%
    filter(name == "January",
           district_name == "Salinas City Elementary",
           school_name == "Laurel Wood Elementary" 
           ) %>%
    chronic_graph_percent(indi = value/100  , xxx = definition, tit = "Chronic Absenteeism", subtit = "Current Year through January")



graph2023 %>%
    filter(#name == "January",
           district_name == "Salinas City",
           school_name == "Laurel Wood Elementary" 
    ) %>%
    chronic_graph_percent(indi = chronic.rate  , xxx = definition, tit = "Chronic Absenteeism", subtit = "Current Year through January")


