

library(tidyverse)
library(readxl)
library(here)


soledad <- read_xlsx(here("data", "Soledad.xlsx")) %>%
    mutate(district_name = "Soledad")




Nweekdays <- Vectorize(function(from,to) sum(!wday(seq(from, to, "days")) %in% c(1,7)))

soledad.joint <- soledad.dates.elem %>%
    bind_rows(soledad.dates.high) %>%
    filter(`All day` %in% c("I","U","X","Q","A","C","L","O","S","Z")) %>%
    group_by(`Student ID`) %>%
    summarise(days.absent = n()) %>%
    left_join(soledad.demo) %>%
    mutate(days.enrolled = Nweekdays(mdy(`Enter Date`),ymd("2023-03-15")),
           #. THese are all calculated based on mid-March,  would need to be adjusted.
           holiday.count = case_when(mdy(`Enter Date`) < mdy("08-10-2022") ~ 30,
                                      mdy(`Enter Date`) < mdy("09-05-2022") ~ 29,
                                      mdy(`Enter Date`) < mdy("10-07-2022") ~ 28,
                                      mdy(`Enter Date`) < mdy("10-10-2022") ~ 27,
                                      mdy(`Enter Date`) < mdy("11-11-2022") ~ 26,
                                      mdy(`Enter Date`) < mdy("11-25-2022") ~ 20,
                                      mdy(`Enter Date`) < mdy("01-06-2023") ~ 6,
                                      mdy(`Enter Date`) < mdy("01-09-2023") ~ 5,
                                      mdy(`Enter Date`) < mdy("01-16-2023") ~ 4,
                                      mdy(`Enter Date`) < mdy("02-13-2023") ~ 3,
                                      mdy(`Enter Date`) < mdy("02-20-2023") ~ 2,
                                      mdy(`Enter Date`) < mdy("03-10-2023") ~ 1,
                                      
                                      ),
           days.enroll.real = days.enrolled - holiday.count,
           absent.rate = days.absent/days.enroll.real,
           chronic = if_else(absent.rate >= .1, TRUE, FALSE),
           school_name = recode(School,
                                "1" = "San Vicente Elementary",
                                "2" = "Gabilan Elementary",
                                "3" = "Main Street Middle",
                                "4" = "Soledad High",
                                "5" = "Rose Ferrero Elementary",
                                "6" = "Pinnacles High",
                                "9" = "Frank Ledesma Elementary",
                                "10" = "Jack Franscioni Elementary"
                                ),
           el.status = recode(`LANG_FLUENCY Value`,
                              "1" = "EO",
                              "2" = "IFEP",
                              "3" = "EL",
                              "4" = "RFEP",
                              "5" = "EL Pending Testing"
                              ),
           Gender = recode(Gender, "M" = "Male", "F" = "Female"),
           
           race =  case_when(EthCd == "Y" ~ "Latino",
                             Description_STU_RC1 == "White" ~ "White",
                             str_detect(Description_STU_RC1, "Black") ~ "Black or African American",
                             str_detect(Description_STU_RC1, "American Indian") ~ "American Indian or Alaska Native",
                             str_detect(Description_STU_RC1, "Asia|Camb|Chine|Hmong|Japan|Laot|Viet") ~ "Asian",
                             Description_STU_RC1 == "Filipino" ~ "Filipino",
                             str_detect(Description_STU_RC1, "Hawa|Pacif|Samo") ~ "Pacific Islander",
                             ),
           foster = recode(`FOSTER Value`, "Yes" = "Foster", "No" = "Not Foster"),
                  migrant = recode(`TITLE1C_MIGRANT Value`, "Yes" = "Migrant", "No" = "Not Migrant"),
                  sped = recode(`SPECIALED Value`, "Yes" = "SPED", "No" = "Not SPED"),
                  disadvantaged = recode(`DISADVANTAGED Value`, "Yes" = "Low Income", "No" = "Not Low Income"),
                  homeless = recode(`HOMELESS Value`, "Yes" = "Homeless", "No" = "Not Homeless"),

           
           ) %>%
    filter(!is.na(chronic),
 #       Grade %in% c(-1,0)           # Remeber to delete
           )
 

soledad.school <- soledad.joint %>%
    group_by(school_name) %>%
    transmute(school_name,
              chronic.rate = mean(chronic)) %>%
    distinct() %>%
    ungroup() %>%
    mutate(district_name = "Soledad",
           definition = "Over all") %>%
    select(district_name, school_name, definition, chronic.rate)


soledad.do <- soledad.joint %>%
    ungroup() %>%
    transmute(chronic.rate = mean(chronic)) %>%
    distinct() %>%
    mutate(school_name = "District Office"    ,
           district_name = "Soledad",
           definition = "Over all")


soledad.2023 <- soledad.school %>%
    bind_rows(soledad.do)






########                      

soledad.stu.grp <- soledad.joint %>%
    group_by(definition = Gender) %>%
    summarise(chronic.rate = mean(chronic),
              count_n = n()) %>%
    distinct() %>%
    mutate(# School = str_trim(School),
        district_name = "Soledad",
        school_name = "District Office",
        #      definition = "Over all"
    )


soledad.stu.grp <- soledad.stu.grp %>% filter(school_name == "")


soledad.fun <- function(var) {
    
    
    
    soledad.temp <- soledad.joint %>%
        group_by(definition = as.character({{var}})) %>%
        summarise(chronic.rate = mean(chronic),
                  count_n = n()) %>%
        distinct() %>%
        mutate(# School = str_trim(School),
            district_name = "Soledad",
            school_name = "District Office",
            #      definition = "Over all"
        )
    
    bind_rows(soledad.stu.grp,soledad.temp)
    
}

soledad.grps <- soledad.fun(Gender) %>%
    bind_rows(soledad.fun(race) ) %>%
    bind_rows(soledad.fun(el.status) ) %>%
    bind_rows(soledad.fun( sped) ) %>%
    bind_rows(soledad.fun(disadvantaged) ) %>%
    bind_rows(soledad.fun(homeless) ) %>%
    bind_rows(soledad.fun(foster) ) %>%
    bind_rows(soledad.fun(migrant) ) 




###



soledad.fun.sch <- function(var, schooly) {
    
    soledad.joint %>%
        filter(str_detect (school_name,schooly))  %>%
        group_by(definition = {{var}}) %>%
        summarise(chronic.rate = mean(chronic),
                  count_n = n()) %>%
        distinct() %>%
        mutate(# School = str_trim(School),
            district_name = "Soledad",
            school_name = schooly,
            #      definition = "Over all"
        )
    
    
    
}



soledad.schools <- soledad.joint$school_name %>% unique()

soledad.grps.sch <- soledad.fun.sch(homeless, "Main Street Middle") %>%
    filter(school_name == "")

for (sch in soledad.schools) {
    
    
    
    soledad.grps.sch.temp <- soledad.fun.sch(homeless,sch) %>%
        bind_rows(soledad.fun.sch(migrant,sch) ) %>%
        bind_rows(soledad.fun.sch(sped,sch) ) %>%
        bind_rows(soledad.fun.sch(foster,sch) ) %>%
        
        bind_rows(soledad.fun.sch(el.status,sch) ) %>%
        bind_rows(soledad.fun.sch(Gender,sch) ) %>%
        bind_rows(soledad.fun.sch(race,sch) ) 
    
    soledad.grps.sch <- bind_rows(soledad.grps.sch,soledad.grps.sch.temp)
    
}

soledad.grps.sch <- soledad.grps.sch %>%
    bind_rows(soledad.grps) %>%
    distinct() %>%
    filter(count_n >=10,
           !is.na(definition)) 



soledad.2023 <- soledad.school %>%
    bind_rows(soledad.do) %>%
    bind_rows(soledad.grps.sch)





