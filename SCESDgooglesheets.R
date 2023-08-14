

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



### New Files from 3/16/2023 ----

# These files are more comprehensive and up to date 


scesd.chr <- read_xlsx(here("data", "scesd", "SCESD Chronic Abs Data File2_Att Calc.xlsx"),
                         sheet = "SCESD 080822_031523 Att Calc",
                         #       col_types = c("text","numeric","text","text","date","numeric","text","text","text")
) %>%
    mutate(chronic = if_else(`Attendance %` >= 90.0, FALSE, TRUE))

scesd.joint <- scesd.chr %>% left_join(scesd.demo2) %>%
    rename("Hispanic" = starts_with("Hispanic") ) %>%
    mutate(race =  case_when(Hispanic == "Y" ~ "Latino",
                             Race1 == "700" ~ "White",
                             Race1 ==  "100" ~ "American Indian",
                             Race1 == "200" ~ "Asian",
                             Race1 == "400" ~ "Filipino",
  #                           Race1 == "205" ~ "Asian Indian",
                             Race1 == "600" ~ "Black or African American",
                             Race1 >= "200" & Race1 <= "299" ~ "Asian",
                             Race1 >= "300" & Race1 <= "399" ~ "Pacific Islander",
                             Race1 == "500" ~ "Latino",)
    ) %>%
    mutate(foster = recode(`Foster Value`, "Yes" = "Foster", "No" = "Not Foster"),
           migrant = recode(`Migrant Value`, "Yes" = "Migrant", "No" = "Not Migrant"),
           sped = recode(`Special Ed Value`, "Yes" = "SPED", "No" = "Not SPED"),
           disadvantaged = recode(`Disadvantaged Value`, "Yes" = "Disadvantaged", "No" = "Not Disadvantaged"),
           homeless = recode(`Homeless Value`, "Yes" = "Homeless", "No" = "Not Homeless"),
           Gender = recode(Gender, "M" = "Male", "F" = "Female"),
           el.status = recode(`Language Fluency`, "I" = "IFEP", "R" = "RFEP", "L" = "EL", "E" = "EO", "T" = "EL TBD"),
           
           ) %>%
    mutate(           school_name = case_when(str_detect(School, "Kammann") ~ "Henry F. Kammann Elementary",
                                              str_detect(School, "Meadows") ~ "Boronda Meadows",
                                              str_detect(School, "Padres") ~ "Los Padres Elementary",
                                              str_detect(School, "Vista") ~ "Loma Vista Elementary",
                                              str_detect(School, "Mission") ~ "Mission Park Elementary",
                                              str_detect(School, "Lincoln") ~ "Lincoln Elementary",
                                              str_detect(School, "Monterey") ~ "Monterey Park Elementary",
                                              str_detect(School, "Laurel") ~ "Laurel Wood Elementary",
                                              str_detect(School, "Gabilan") ~ "El Gabilan Elementary",
                                              str_detect(School, "DIAS") ~ "Boronda Elementary",
                                              str_detect(School, "Natividad") ~ "Natividad Elementary",
                                              str_detect(School, "Sherwood") ~ "Sherwood Elementary",
                                              str_detect(School, "University") ~ "University Park Elementary",
                                              str_detect(School, "Virtual") ~ "Salinas City Virtual Academy",
                                              str_detect(School, "Roosevelt") ~ "Roosevelt Elementary",
    ) 
    )  #%>%
 #   filter(Grade %in% c(-1,0)) # Remember to Delete



scesd.school <- scesd.joint %>%
    group_by(school_name) %>%
    transmute(school_name,
              chronic.rate = mean(chronic)) %>%
    distinct() %>%
    ungroup() %>%
    mutate(district_name = "Salinas City",
           definition = "Over all") %>%
    select(district_name, school_name, definition, chronic.rate)


scesd.do <- scesd.joint %>%
    ungroup() %>%
    
    
    group_by(`Student ID`) %>%
    mutate(absent2 = sum(Absences),
           enroll2 = sum(Enrolled),
           absent.rate2 = absent2/enroll2,
           chronic2 = if_else(absent.rate2 >= .1, TRUE, FALSE)
    ) %>%
    
    ungroup() %>%
    select( `Student ID`, chronic2) %>%
    distinct() %>%
    
    
    
    
    transmute(chronic.rate = mean(chronic2)) %>%
    distinct() %>%
    mutate(school_name = "District Office"    ,
           district_name = "Salinas City",
           definition = "Over all")


scesd.2023.rev <- scesd.school %>%
    bind_rows(scesd.do)






########                      
                      
                      scesd.stu.grp <- scesd.joint %>%
                          group_by(definition = Gender) %>%
                          summarise(chronic.rate = mean(chronic),
                                    count_n = n()) %>%
                          distinct() %>%
                          mutate(# School = str_trim(School),
                              district_name = "Salinas City",
                              school_name = "District Office",
                              #      definition = "Over all"
                          )
                      
                      
                      scesd.stu.grp <- scesd.stu.grp %>% filter(school_name == "")
                      
                      
                      scesd.fun <- function(var) {
                          
                          
                          
                          scesd.temp <- scesd.joint %>%
                              
                              
                              group_by(`Student ID`) %>%
                              mutate(absent2 = sum(Absences),
                                     enroll2 = sum(Enrolled),
                                     absent.rate2 = absent2/enroll2,
                                     chronic2 = if_else(absent.rate2 >= .1, TRUE, FALSE)
                              ) %>%
                              
                              ungroup() %>%
                              select({{var}}, `Student ID`, chronic2) %>%
                              distinct() %>%
                              
                              
                              group_by(definition = as.character({{var}})) %>%
                              summarise(chronic.rate = mean(chronic2),
                                        count_n = n()) %>%
                              distinct() %>%
                              mutate(# School = str_trim(School),
                                  district_name = "Salinas City",
                                  school_name = "District Office",
                                  #      definition = "Over all"
                              )
                          
                          bind_rows(scesd.stu.grp,scesd.temp)
                          
                      }
                      
                      scesd.grps <- scesd.fun(Gender) %>%
                          bind_rows(scesd.fun(race) ) %>%
                          bind_rows(scesd.fun(el.status) ) %>%
                          bind_rows(scesd.fun( sped) ) %>%
                          bind_rows(scesd.fun(homeless) ) %>%
                          bind_rows(scesd.fun(foster) ) %>%
                          bind_rows(scesd.fun(migrant) ) 
                      
                      
                      

###
                      
                      
                      
                      scesd.fun.sch <- function(var, schooly) {
                          
                          scesd.joint %>%
                              filter(str_detect (school_name,schooly))  %>%
                              group_by(definition = {{var}}) %>%
                              summarise(chronic.rate = mean(chronic),
                                        count_n = n()) %>%
                              distinct() %>%
                              mutate(# School = str_trim(School),
                                  district_name = "Salinas City",
                                  school_name = schooly,
                                  #      definition = "Over all"
                              )
                          
                          
                          
                      }
                      
                      
                      
                      scesd.schools <- scesd.joint$school_name %>% unique()
                      
                      scesd.grps.sch <- scesd.fun.sch(homeless, "Monterey Park") %>%
                          filter(school_name == "")
                      
                      for (sch in scesd.schools) {
                          
                          
                          
                          scesd.grps.sch.temp <- scesd.fun.sch(homeless,sch) %>%
                              bind_rows(scesd.fun.sch(migrant,sch) ) %>%
                              bind_rows(scesd.fun.sch(sped,sch) ) %>%
                              bind_rows(scesd.fun.sch(foster,sch) ) %>%
                              
                              bind_rows(scesd.fun.sch(el.status,sch) ) %>%
                              bind_rows(scesd.fun.sch(Gender,sch) ) %>%
                              bind_rows(scesd.fun.sch(race,sch) ) 
                          
                          scesd.grps.sch <- bind_rows(scesd.grps.sch,scesd.grps.sch.temp)
                          
                      }
                      
                      scesd.grps.sch <- scesd.grps.sch %>%
                          bind_rows(scesd.grps) %>%
                          distinct() %>%
                          filter(#count_n >=10,
                                 !is.na(definition),
                                 !str_detect(definition,"TBD")) 
                      
                      
                      
                      scesd.2023.rev <- scesd.school %>%
                          bind_rows(scesd.do) %>%
                          bind_rows(scesd.grps.sch)
                      






