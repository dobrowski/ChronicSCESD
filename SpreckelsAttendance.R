
library(tidyverse)
library(here)
library(readxl)




spreckels <- read_xls(here("data","spreckels","Attendance Data SUSD 050923.xls")) %>%
    mutate(days.enroll.real = as.numeric(expected_att_days),
           days.absent = as.numeric(expected_att_days) - as.numeric(days_attended),
           race =  case_when(stuethnicity == "Hispanic" ~ "Latino",
                             sturace1text == "White" ~ "White",
                             str_detect(sturace1text, "Black") ~ "Black or African American",
                             str_detect(sturace1text, "Native") ~ "American Indian or Alaska Native",
                             str_detect(sturace1text, "Asia|Camb|Chine|Hmong|Japan|Laot|Viet|Kor") ~ "Asian",
                             sturace1text == "Filipino" ~ "Filipino",
                             str_detect(sturace1text, "Hawa|Pacif|Samo|Guam") ~ "Pacific Islander",
                             TRUE ~ "Unknown"
           ),
           foster = recode(foster, "Y" = "Foster", "N" = "Not Foster"),
           migrant = recode(migrant, "Y" = "Migrant", "N" = "Not Migrant"),
           specialed = recode(specialed, "Y" = "SPED", "N" = "Not SPED"),
           nslp = recode(nslp, "Y" = "Low Income", "N" = "Not Low Income"),
           homeless = recode(homeless, "Y" = "Homeless", "N" = "Not Homeless"),
           
           
    )



dist.stu.grp <- tribble(~definition, ~chronic.rate, ~count_n)



district.fun <- function(df, var) {
    
    
    
    dist.temp <-     df %>%
        group_by(ssid) %>%
        mutate(absent2 = sum(days.absent),
               enroll2 = sum(days.enroll.real),
               absent.rate2 = absent2/enroll2,
               chronic2 = if_else(absent.rate2 >= .1, TRUE, FALSE)
        ) %>%
        
        ungroup() %>%
        select({{var}}, ssid, chronic2) %>%
        distinct() %>%
        
        group_by(definition = as.character({{var}})) %>%
        summarise(chronic.rate = mean(chronic2),
                  count_n = n()) %>%
        distinct() # %>%
        # mutate(# School = str_trim(School),
        #     district_name = "Soledad",
        #     school_name = "District Office",
        #     #      definition = "Over all"
        #)
    
    bind_rows(dist.stu.grp,dist.temp)
    
}

spreck.grps <- district.fun(spreckels,stugender) %>%
    bind_rows(district.fun(spreckels, stugrade) ) %>%
    bind_rows(district.fun(spreckels, race) ) %>%
    bind_rows(district.fun(spreckels, stuengprofictext) ) %>%
    
    bind_rows(district.fun(spreckels, specialed) ) %>%
    bind_rows(district.fun(spreckels, nslp) ) %>%
    bind_rows(district.fun(spreckels, homeless) ) %>%
    bind_rows(district.fun(spreckels, foster) ) %>%
    bind_rows(district.fun(spreckels, migrant) ) %>%
    mutate(chronic.rate =  round(chronic.rate *100,1)  )



write_csv(spreck.grps, "Spreckels Student Groups Chronic Absenteeism through May 9.csv")
