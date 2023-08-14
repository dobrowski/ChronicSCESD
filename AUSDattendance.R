

library(tidyverse)
library(readxl)
library(here)


ausd.tri1 <- read_xlsx(here("data", "alisal" , "22-23 Tri 1 Abs.xlsx")) %>% 
    rename_with(.cols = c(`Enrolled`:`Percentage`),
              .fn = ~paste0(.,"_tri_1")) %>%
    filter(!str_detect(School, "RBE") ) %>%
    mutate(Absence_tri_1 = as.numeric(Absence_tri_1)) 
           

ausd.vrb.tri1 <- read_xlsx(here("data","alisal" ,"22-23 Tri 1 Abs.xlsx"),
                       sheet = "VRB") %>% 
    rename_with(.cols = c(`Enrolled`:`Percentage`),
                .fn = ~paste0(.,"_tri_1"))

ausd.tri1 <- ausd.tri1 %>%
    bind_rows(ausd.vrb.tri1)


ausd.tri2 <- read_xlsx(here("data", "alisal" ,"22-23 Tri 2 Abs.xlsx")) %>% 
    rename_with(.cols = c(`Enrolled`:`Percentage`),
                .fn = ~paste0(.,"_tri_2"))



ausd.joint <- full_join(ausd.tri1,ausd.tri2) %>%
    rowwise() %>%
    mutate(Absence_tri_1 = as.numeric(Absence_tri_1), 
           enrolled =  sum(Enrolled_tri_1,Enrolled_tri_2, na.rm = TRUE) ,
           attend =  sum(Attendance_tri_1,Attendance_tri_2, na.rm = TRUE) ,
           absent =  sum(Absence_tri_1,Absence_tri_2, na.rm = TRUE), 
           percent = absent/enrolled,
           chronic = if_else(percent >= .1, TRUE, FALSE)
           ) %>%
    as_tibble()


ausd.school <- ausd.joint %>%
    group_by(School) %>%
    transmute(chronic.rate = mean(chronic)) %>%
    distinct() %>%
    mutate(School = str_trim(School),
           district_name = "Alisal Union",
           definition = "Over all",
           school_name = case_when(School == "ACS" ~ "Alisal Community",
                                   School == "BES" ~ "Bardin Elementary",
                                   School == "CES" ~ "Cesar E. Chavez Elementary",
                                   School == "CRES" ~ "Creekside Elementary",
                                   School == "LES" ~ "Oscar F. Loya Elementary",
                                   School == "FPE" ~ "Frank Paul Elementary",
                                   School == "FRE" ~ "Fremont Elementary",
                                   School == "MLK" ~ "Dr. Martin Luther King, Jr. Academy", #? 
                                   School == "MBS" ~ "Monte Bella Elementary",
                                   School == "SES" ~ "Jesse G. Sanchez Elementary",
                                   School == "STES" ~ "John E. Steinbeck Elementary",
                                   School == "RBE" ~ "Virginia Rocca Barton Elementary",
                                   School == "AVA" ~ "Alisal Virtual Academy"
                                   )
           ) %>%
    ungroup() %>%
    select(district_name, school_name, definition, chronic.rate)



# RBE is more than 120.  




library(googlesheets4)
ausd.demo <- read_sheet("https://docs.google.com/spreadsheets/d/1fr2E7KVAjFkibXS-8-yjD4pDbgdXDnV7G88pZtOPH_g/edit#gid=791155742") %>%
    rename(LocalID = `STU Local ID`) %>%
    mutate(LocalID = as.character(LocalID))




ausd.demo.rev <- ausd.demo %>%
    mutate(SWD = if_else(!is.na(S_CA_STU_X.PrimaryDisability), "SWD", "Not SWD"),
           ca_elastatus = replace_na(ca_elastatus, "TBD" ),
           gender = recode(gender, "M" = "Male", "F" = "Female"),
           
           Migrant = case_when( is.na(ca_migranted) ~ "Unknown",
                                ca_migranted == 1 ~ "Migrant",
                                ca_migranted == 0 ~ "Not Migrant"),
           Ethnicity = case_when(Ethnicity == "700" ~ "White",
                                 Ethnicity >=  "100" &  Ethnicity <= "199" ~ "American Indian",
                                 Ethnicity == "200" ~ "Asian",
                                 Ethnicity == "400" ~ "Filipino",
                                 Ethnicity >= "200" & Ethnicity <= "299" ~ "Asian",
                                 Ethnicity >= "300" & Ethnicity <= "399" ~ "Pacific Islander",                                 Ethnicity == "600" ~ "Black or African American",
                        #         Ethnicity == "299" ~ "Other Asian",
                                 Ethnicity == "500" ~ "Latino",
                                 TRUE ~ as.character(Ethnicity)
           ),
            S_CA_STU_X.PrimaryResidence = case_when(S_CA_STU_X.PrimaryResidence == "210" ~ "Foster", 
                                                    S_CA_STU_X.PrimaryResidence <= "130" ~ "Homeless", 
                                                    TRUE ~ as.character(S_CA_STU_X.PrimaryResidence)
                                                    )
           ) %>%
    as_tibble()


ausd.2joint <-  ausd.joint %>%
    mutate(LocalID = str_trim(LocalID)) %>%
    left_join(ausd.demo.rev)  # %>%
  #  filter(str_detect(Grade,"0")) # Remeber to delete
    

ausd.do <- ausd.2joint %>%
    ungroup() %>%
    
    group_by(LocalID) %>%
    mutate(absent2 = sum(absent),
           enroll2 = sum(enrolled),
           absent.rate2 = absent2/enroll2,
           chronic2 = if_else(absent.rate2 >= .1, TRUE, FALSE)
    ) %>%
    
    ungroup() %>%
    select( LocalID, chronic2) %>%
    distinct() %>%

    transmute(chronic.rate = mean(chronic2)) %>%
    distinct() %>%
    mutate(school_name = "District Office"    ,
           district_name = "Alisal Union",
           definition = "Over all")



ausd.2joint %>%
    group_by(definition = SWD) %>%
    summarise(chronic.rate = mean(chronic)) %>%
    distinct() %>%
    mutate(# School = str_trim(School),
           district_name = "Alisal Union",
           school_name = "District Office",
     #      definition = "Over all"
           )

    
    
    ### Group_by series -----
    
    
ausd.stu.grp <- ausd.2joint %>%
    group_by(definition = gender) %>%
    summarise(chronic.rate = mean(chronic),
              count_n = n()) %>%
    distinct() %>%
    mutate(# School = str_trim(School),
        district_name = "Alisal Union",
        school_name = "District Office",
        #      definition = "Over all"
    )


ausd.stu.grp <- ausd.stu.grp %>% filter(school_name == "")


ausd.fun <- function(var) {
    


ausd.temp <- ausd.2joint %>%
    
    group_by(LocalID) %>%
    mutate(absent2 = sum(absent),
           enroll2 = sum(enrolled),
           absent.rate2 = absent2/enroll2,
           chronic2 = if_else(absent.rate2 >= .1, TRUE, FALSE)
    ) %>%
    
    ungroup() %>%
    select({{var}}, LocalID, chronic2) %>%
    distinct() %>%
    
    
    
    group_by(definition = as.character({{var}})) %>%
    summarise(chronic.rate = mean(chronic2),
              count_n = n()) %>%
    distinct() %>%
    mutate(# School = str_trim(School),
        district_name = "Alisal Union",
        school_name = "District Office",
        #      definition = "Over all"
    )

bind_rows(ausd.stu.grp,ausd.temp)

}

ausd.grps <- ausd.fun(S_CA_STU_X.PrimaryResidence) %>%
    bind_rows(ausd.fun(Migrant) ) %>%
    bind_rows(ausd.fun(SWD) ) %>%
    bind_rows(ausd.fun( ca_elastatus) ) %>%
    bind_rows(ausd.fun(gender) ) %>%
    bind_rows(ausd.fun(Ethnicity) ) 
    


ausd.fun.sch <- function(var, schooly) {
    
ausd.2joint %>%
        filter(str_detect (School,schooly))  %>%
        group_by(definition = as.character({{var}})) %>%
        summarise(chronic.rate = mean(chronic),
                  count_n = n()) %>%
        distinct() %>%
        mutate(# School = str_trim(School),
            district_name = "Alisal Union",
            school_name = schooly,
            #      definition = "Over all"
        )
    
    
    
}



ausd.schools <- ausd.2joint$School %>% unique()

ausd.grps.sch <- ausd.fun.sch(SWD, "ACS") %>%
    filter(school_name == "")

for (sch in ausd.schools) {
    


ausd.grps.sch.temp <- ausd.fun.sch(S_CA_STU_X.PrimaryResidence,sch) %>%
    bind_rows(ausd.fun.sch(Migrant,sch) ) %>%
    bind_rows(ausd.fun.sch(SWD,sch) ) %>%
    bind_rows(ausd.fun.sch( ca_elastatus,sch) ) %>%
    bind_rows(ausd.fun.sch(gender,sch) ) %>%
    bind_rows(ausd.fun.sch(Ethnicity,sch) ) 

ausd.grps.sch <- bind_rows(ausd.grps.sch,ausd.grps.sch.temp)

}

ausd.grps.sch <- ausd.grps.sch %>%
    bind_rows(ausd.grps) %>%
    distinct() %>%
    filter(#count_n >=10,
           !is.na(definition),
           !str_detect(definition,"TBD"),
           !str_detect(definition,"Unk"),
           !str_detect(definition,"Foster")
           ) %>%
    mutate(definition = recode(definition,
                               "210" = "Foster", 
                               "120" = "Homeless", 
                               "130" = "Homeless",
                               "700" = "White",
                               "100" = "American Indian",
                               "200" = "Asian",
                               "400" = "Filipino",
                               "205" = "Asian Indian",
                               "600" = "Black or African American",
                               "299" = "Other Asian",
                               "500" = "Latino"),
           school_name = case_when(str_detect(school_name, "ACS") ~ "Alisal Community",
                                   str_detect(school_name, "BES") ~ "Bardin Elementary",
                                              str_detect(school_name, "CES" )~ "Cesar E. Chavez Elementary",
                                                         str_detect(school_name, "CRES") ~ "Creekside Elementary",
                                                                    str_detect(school_name, "LES") ~ "Oscar F. Loya Elementary",
                                                                               str_detect(school_name, "FPE") ~ "Frank Paul Elementary",
                                                                                          str_detect(school_name, "FRE" )~ "Fremont Elementary",
                                                                                                     str_detect(school_name, "MLK") ~ "Dr. Martin Luther King, Jr. Academy", #? 
                                                                                                                str_detect(school_name, "MBS" )~ "Monte Bella Elementary",
                                                                                                                           str_detect(school_name, "SES") ~ "Jesse G. Sanchez Elementary",
                                                                                                                                      str_detect(school_name, "STES") ~ "John E. Steinbeck Elementary",
                                                                                                                                                 str_detect(school_name, "RBE" )~ "Virginia Rocca Barton Elementary",
                                                                                                                                                            str_detect(school_name, "AVA") ~ "Alisal Virtual Academy",
                                   str_detect(school_name, "District Office") ~ "District Office"
           ))



ausd.2023 <- ausd.school %>%
    bind_rows(ausd.do) %>%
    bind_rows(ausd.grps.sch)
