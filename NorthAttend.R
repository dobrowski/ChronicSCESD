

# Spring 2023


library(tidyverse)
library(readxl)
library(here)
library(calendR)
library(lubridate)



nmcusd.demo <- read_xlsx(here("data", "nmcusd" ,"NMCUSD Student Chronic Absenteeism File_2022-2023.xlsx"),
                    sheet = "Student Demographics") %>%
    mutate(district_name = "North Monterey",
           val = TRUE) %>%
    pivot_wider(names_from = Sex, values_from = val, values_fill = FALSE) %>%
    mutate(val = TRUE) %>%
    mutate(`Race/Ethnicity` = recode(
        `Race/Ethnicity`, "Decline to state/unknown" = "Decline to state race")
    ) %>%
    pivot_wider(names_from = `Race/Ethnicity`, values_from = val, values_fill = FALSE) %>%
    mutate(val = TRUE) %>%
    mutate(`Contact's Highest Education Level` = recode(
        `Contact's Highest Education Level`, "Decline to state/unknown" = "Decline to state education")
        ) %>%
    pivot_wider(names_from = `Contact's Highest Education Level`, values_from = val, values_fill = FALSE, names_repair = "unique") %>%
    mutate(val = TRUE) %>%
    pivot_wider(names_from = `English Proficiency`, values_from = val, values_fill = FALSE) %>%
    mutate(val = TRUE) %>%
    pivot_wider(names_from = `Special ED Placement`, values_from = val, values_fill = FALSE)



nmcusd.att <- read_xlsx(here("data", "NMCUSD Student Chronic Absenteeism File_2022-2023.xlsx"),
                         sheet = "Student Attendance Summary") %>%
    mutate(district_name = "North Monterey",
           absent.rate = `Days Absent`/`Days Enrolled`,
           chronic = if_else(absent.rate >= .1 , TRUE, FALSE),
           `Student ID` = as.character(`Student ID`)
    ) %>%
    select(-Grade)



nmcusd <- left_join(nmcusd.att, nmcusd.demo) # %>%
   # filter(Grade %in% c("TK","K","01","02")) ## Added for TK-2 exploration


nm.cols <- colnames(nmcusd)

nm.colss <- setdiff(nm.cols[24:58] , c("Guamanian", "Vietnamese","Cambodian","Asian Indian","Chinese","Native American"
,"Japanese","X","Samoan","TBD","Other Asian"))




nm.rate <- function(colly) {
    
    nmcusd %>%
    filter( {{colly}}  == TRUE) %>%
    summarise(definition = colly ,
              chronic.rate = mean(.data$chronic, na.rm = TRUE))
    
    
    }

nm.rate("EO")





temp.nm <- nmcusd %>%
    filter(`EO` == TRUE) %>%
    summarise(definition = "EO" ,
              chronic.rate = mean(chronic, na.rm = TRUE),
              count_n = n()) %>%
    unique()



nm.sum <- temp.nm %>%
    filter(definition == "")

nm.schools <- nmcusd %>%
    select(School) %>%
    distinct() %>%
    unlist()





for (i in nm.colss) {

        temp.nm <- nmcusd %>%
            filter(nmcusd[[i]] == TRUE) %>%
            summarise(school_name = "District Office",
                      definition = i ,
                      chronic.rate = mean(chronic, na.rm = TRUE),
                      count_n = n()) %>%
            unique()
        
        nm.sum <- bind_rows(nm.sum, temp.nm)
        
}





for (i in nm.colss) {
    for (school in nm.schools) {
        
    
    
temp.nm <- nmcusd %>%
    filter(nmcusd[[i]] == TRUE,
           School == school) %>%
    summarise(school_name = school,
              definition = i ,
              chronic.rate = mean(chronic, na.rm = TRUE),
              count_n = n()) %>%
    unique()

nm.sum <- bind_rows(nm.sum, temp.nm)


}
}



for (i in 1:1) {
    
    temp.nm <- nmcusd %>%
   #     filter(nmcusd[[i]] == TRUE) %>%
        summarise(school_name = "District Office",
                  definition = "Over all" ,
                  chronic.rate = mean(chronic, na.rm = TRUE),
                  count_n = n()) %>%
        unique()
    
    nm.sum <- bind_rows(nm.sum, temp.nm)
    
}




    for (school in nm.schools) {
        
        
        temp.nm <- nmcusd %>%
            filter(
                   School == school) %>%
            summarise(school_name = school,
                      definition = "Over all" ,
                      chronic.rate = mean(chronic, na.rm = TRUE),
                      count_n = n()) %>%
            unique()
        
        nm.sum <- bind_rows(nm.sum, temp.nm)
        
        
    }






north2023.tk <- nm.sum %>%
    filter(count_n >= 10) %>%
    mutate(district_name = "North Monterey",
           definition = recode(definition,
                               "M" = "Male" ,
                               "F" = "Female"	,
                                "X" = "Non Binary",
                               "RFEP" =	"Reclassified Fluent English Proficient",
                               "EL"	= "English Learner",
                               "EO"	= "English Only",
                               "IFEP" = "Initially Fluent English Proficient",
                               "TBD" = "To Be Determined EL Status",
                               "ND"	 = "Blank EL Status",
                               "SDC" = "Special Day Class",
                               "RSP" = "Resource Specialist Program",
                               "DIS" = "Designated Instructional Services",
                               "N/A" = "Not SPED"
                               
                               ))



### TK-2 x SPED ----


north2023.tk


nmcusd.tk <- left_join(nmcusd.att, nmcusd.demo) %>%
    filter(Grade %in% c("TK","K","01","02")) ## Added for TK-2 exploration


# #for (i in nm.colss) {
#     for (school in nm.schools) {
# 
#         temp.nm <- nmcusd.tk %>%
#             filter(#nmcusd[[i]] == TRUE,
#                    School == school) %>%
#             summarise(school_name = school,
#                       definition = i ,
#                       chronic.rate = mean(chronic, na.rm = TRUE),
#                       count_n = n()) %>%
#             unique()
#         
#         nm.sum <- bind_rows(nm.sum, temp.nm)
#         
#         
#     }
# #}

nmcusd.tk %>%
    group_by(School, SWD) %>%
    summarise(#school_name = school,
              #definition = i ,
              chronic.rate = mean(chronic, na.rm = TRUE),
              count_n = n()) %>%
    ggplot(aes(x= fct_reorder(School,chronic.rate) , y = chronic.rate, fill = SWD)) +
    geom_col(position = position_dodge2( preserve = "single"))+
    scale_y_continuous(
        labels = label_percent(),
        expand = expansion(c(0.1, 0.1))
    ) +
    MCOE::mcoe_theme +
    coord_flip() +
    labs(title = "Chronic Absenteeism Rates for TK-2 students\nBased on Disability Status",
         subtitle = "From Attendance through February 28")


ggsave(here("output","nmcusd","Absenteeism TK-2 by SWD by School.png"), width = 9, height = 6)

### Alt Approach Same technique as other schools  ----





nomo.demo <- read_xlsx(here("data", "nmcusd" ,"NMCUSD Student Chronic Absenteeism File_2022-2023.xlsx"),
                         sheet = "Student Demographics") %>%
    mutate(district_name = "North Monterey") %>%
    mutate(race = recode(`Race/Ethnicity`, "Decline to state/unknown" = "Decline to state race"),
           `Contact's Highest Education Level` = recode(`Contact's Highest Education Level`, "Decline to state/unknown" = "Decline to state education")
    )


nomo.joint <- left_join(nmcusd.att, nomo.demo) %>%
           # school_name = recode(School,
           #                      "1" = "San Vicente Elementary",
           #                      "2" = "Gabilan Elementary",
           #                      "3" = "Main Street Middle",
           #                      "4" = "Soledad High",
           #                      "5" = "Rose Ferrero Elementary",
           #                      "6" = "Pinnacles High",
           #                      "9" = "Frank Ledesma Elementary",
           #                      "10" = "Jack Franscioni Elementary"
           # ),
           mutate(
               el.status = `English Proficiency`,
           Gender = recode(Sex, "M" = "Male", "F" = "Female", "X" = "Nonbinary"),
           
           race =  `Race/Ethnicity`,
           foster = recode(`Foster Student`, "Y" = "Foster", "N" = "Not Foster"),
           migrant = recode(`Migrant Student`, "Y" = "Migrant", "N" = "Not Migrant"),
           sped = recode(SWD, "Y" = "SWD", "N" = "Not SWD"),
           disadvantaged = recode(`Elligible for FRPM`, "Y" = "Low Income", "N" = "Not Low Income"),
           homeless = recode(`Homeless Student`, "Y" = "Homeless", "N" = "Not Homeless"),
           school_name = School
           
           
    ) %>%
    filter(!is.na(chronic),
           Grade %in% c("TK","K","01","02") # Remember to delete
           )


nomo.school <- nomo.joint %>%
    group_by(school_name) %>%
    transmute(school_name,
              chronic.rate = mean(chronic)) %>%
    distinct() %>%
    ungroup() %>%
    mutate(district_name = "North Monterey",
           definition = "Over all") %>%
    select(district_name, school_name, definition, chronic.rate)


nomo.do <- nomo.joint %>%
    ungroup() %>%
    
    group_by(`Student ID`) %>%
    mutate(absent2 = sum(`Days Absent`),
           enroll2 = sum(`Days Enrolled`),
           absent.rate2 = absent2/enroll2,
           chronic2 = if_else(absent.rate2 >= .1, TRUE, FALSE)
    ) %>%
    
    ungroup() %>%
    select( `Student ID`, chronic2) %>%
    distinct() %>%
    
    
    transmute(chronic.rate = mean(chronic2)) %>%
    distinct() %>%
    mutate(school_name = "District Office"    ,
           district_name = "North Monterey",
           definition = "Over all")


nomo.2023 <- nomo.school %>%
    bind_rows(nomo.do)






########                      

nomo.stu.grp <- nomo.joint %>%
    group_by(definition = Gender) %>%
    summarise(chronic.rate = mean(chronic),
              count_n = n()) %>%
    distinct() %>%
    mutate(# School = str_trim(School),
        district_name = "North Monterey",
        school_name = "District Office",
        #      definition = "Over all"
    )


nomo.stu.grp <- nomo.stu.grp %>% filter(school_name == "")


nomo.fun <- function(var) {
    
    
    
    nomo.temp <- nomo.joint %>%
        
        group_by(`Student ID`) %>%
        mutate(absent2 = sum(`Days Absent`),
               enroll2 = sum(`Days Enrolled`),
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
            district_name = "North Monterey",
            school_name = "District Office",
            #      definition = "Over all"
        )
    
    bind_rows(nomo.stu.grp,nomo.temp)
    
}

nomo.grps <- nomo.fun(Gender) %>%
    bind_rows(nomo.fun(race) ) %>%
    bind_rows(nomo.fun(el.status) ) %>%
    bind_rows(nomo.fun( sped) ) %>%
    bind_rows(nomo.fun(disadvantaged) ) %>%
    bind_rows(nomo.fun(homeless) ) %>%
    bind_rows(nomo.fun(foster) ) %>%
    bind_rows(nomo.fun(migrant) ) 




###



nomo.fun.sch <- function(var, schooly) {
    
    nomo.joint %>%
        filter(str_detect (school_name,schooly))  %>%
        group_by(definition = {{var}}) %>%
        summarise(chronic.rate = mean(chronic),
                  count_n = n()) %>%
        distinct() %>%
        mutate(# School = str_trim(School),
            district_name = "North Monterey",
            school_name = schooly,
            #      definition = "Over all"
        )
    
    
    
}



nomo.schools <- nomo.joint$school_name %>% unique()

nomo.grps.sch <- nomo.fun.sch(homeless, "Elkhorn") %>%
    filter(school_name == "")

for (sch in nomo.schools) {
    
    
    
    nomo.grps.sch.temp <- nomo.fun.sch(homeless,sch) %>%
        bind_rows(nomo.fun.sch(migrant,sch) ) %>%
        bind_rows(nomo.fun.sch(sped,sch) ) %>%
        bind_rows(nomo.fun.sch(foster,sch) ) %>%
        
        bind_rows(nomo.fun.sch(el.status,sch) ) %>%
        bind_rows(nomo.fun.sch(Gender,sch) ) %>%
        bind_rows(nomo.fun.sch(race,sch) ) 
    
    nomo.grps.sch <- bind_rows(nomo.grps.sch,nomo.grps.sch.temp)
    
}

nomo.grps.sch <- nomo.grps.sch %>%
    bind_rows(nomo.grps) %>%
    distinct() %>%
    filter(#count_n >=10,
           !is.na(definition),
           definition != "ND",
           !str_detect(definition,"Decline")) 



nomo.2023.rev <- nomo.school %>%
    bind_rows(nomo.do) %>%
    bind_rows(nomo.grps.sch)












### END -------
