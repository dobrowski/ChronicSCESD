




library(tidyverse)
library(readxl)
library(here)


nmcusd.demo <- read_xlsx(here("data", "NMCUSD Student Chronic Absenteeism File_2022-2023.xlsx"),
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

### Calendar ----

library(calendR)
library(lubridate)


nmcusd.dates <- read_xlsx(here("data", "NMCUSD Student Chronic Absenteeism File_2022-2023.xlsx"),
                         sheet = "Student Absence Details",
                         col_types = c("text","numeric","text","text","date","numeric","text","text","text"))


### All Calendar -----


nmcusd.dates.out <- nmcusd.dates %>%
    filter(`Full Day Amount` == 1) %>%
    mutate(att.date = as.Date(`Attendance Date`)) %>%
    group_by(att.date) %>%
    count() %>%
    ungroup()
    


dat_pr <- nmcusd.dates.out %>% 
    complete(att.date = seq.Date(ymd("2022-08-10"), 
                                 ymd("2023-02-28"), 
                        "day"))  %>%
    mutate(n = replace_na(n,0))  %>%
    
    mutate(weekday = wday(att.date, label = T, week_start = 1), 
           month = month(att.date, label = T, abbr = F),
           week = isoweek(att.date),
           day = day(att.date),
           text_col = "black",
           perc = n/max(n)
    )




calendR(start_date = "2022-08-10", # Custom start date
        end_date = "2023-02-28" , # "2023-02-28",
        special.days = dat_pr$perc[1:length(dat_pr$perc)], # Vector of the same length as the number of days of the year
        gradient = TRUE,      # Set gradient = TRUE to create the heatmap
        special.col = "red", # Color of the gradient for the highest value
        low.col = "white",
        title = "North Monterey County - All Students",
        subtitle = "The more intense the Red color, the larger the number of students absent"
            )                       # Color of the gradient for the lowest value


ggsave(here("output","nmcusd","Calendar All Students.png"), width = 9, height = 6)

### TK Calendar ------

nm.cal.heat <- function(df, tit) {
    

nmcusd.dates.out <- df %>%
    filter(`Full Day Amount` == 1) %>%
    mutate(att.date = as.Date(`Attendance Date`)) %>%
    group_by(att.date) %>%
    count() %>%
    ungroup()


dat_pr <- nmcusd.dates.out %>% 
    complete(att.date = seq.Date(min(att.date), 
                                 max(att.date), 
                                 "day"))  %>%
    mutate(n = replace_na(n,0))  %>%
    mutate(text_col = "black",
           perc = n/max(n))




calendR(start_date = min(dat_pr$att.date), # Custom start date
        end_date = max(dat_pr$att.date) , # "2023-02-28",
        special.days = dat_pr$perc[1:length(dat_pr$perc)], # Vector of the same length as the number of days of the year
        gradient = TRUE,      # Set gradient = TRUE to create the heatmap
        special.col = "red", # Color of the gradient for the highest value
        low.col = "white",
        title = tit, 
        subtitle = "The more intense the Red color, the larger the number of students absent"
)                       # Color of the gradient for the lowest value


ggsave(here("output","nmcusd",paste0(tit,".png")), width = 9, height = 6)

}

nmcusd.dates %>%
    filter(Grade %in% c("TK","K","1","2")) %>%
    nm.cal.heat("North Monterey County - TK-2 Students")


nmcusd.dates %>%
#    filter(Grade %in% c("TK","K","1","2")) %>%
    nm.cal.heat("North Monterey County - All Students")




nmcusd.dates %>%
    mutate(`Student ID` = as.character(`Student ID`)) %>%
    left_join(nmcusd.demo) %>%
    filter(Grade %in% c("TK","K","1","2"),
           SWD == "Y") %>%
    nm.cal.heat("North Monterey County - TK-2 Students with Disabilities")


###




nm.holiday <- tribble(~ att.date, ~n,
                      "2022-08-01", "Holiday",
                      "2022-08-02", "Holiday",
                      "2022-08-03", "Holiday",
                      "2022-08-04", "Holiday",
                      "2022-08-05", "Holiday",
                      "2022-08-08", "Holiday",
                      "2022-08-09", "Holiday",
                      "2022-09-05", "Holiday",
                      "2022-10-10", "Holiday",
                      "2022-11-11", "Holiday",
                      "2022-11-21", "Holiday",
                      "2022-11-22", "Holiday",
                      "2022-11-23", "Holiday",
                      "2022-11-24", "Holiday",
                      "2022-11-25", "Holiday",
                      "2022-12-19", "Holiday",
                      "2022-12-20", "Holiday",
                      "2022-12-21", "Holiday",
                      "2022-12-22", "Holiday",
                      "2022-12-23", "Holiday",
                      "2022-12-26", "Holiday",
                      "2022-12-27", "Holiday",
                      "2022-12-28", "Holiday",
                      "2022-12-29", "Holiday",
                      "2022-12-30", "Holiday",
                      "2023-01-02", "Holiday",
                      "2023-01-03", "Holiday",
                      "2023-01-04", "Holiday",
                      "2023-01-05", "Holiday",
                      "2023-01-06", "Holiday",
                      "2023-01-09", "Holiday",
                      "2023-01-16", "Holiday",
                      "2023-02-20", "Holiday"
                      ) %>%
    mutate(att.date = ymd(att.date))


nm.stu.cal <- function(stu.id) {
    
    
    nmcusd.dates.out <- nmcusd.dates %>%
        filter(`Full Day Amount` == 1,
               `Student ID` %in% stu.id) %>%
        mutate(att.date = as.Date(`Attendance Date`)) %>%
        group_by(att.date) %>%
        count() %>%
        ungroup() %>%
        mutate(n = "Absent") %>%
        bind_rows(nm.holiday)
    
    
    
    dat_pr <- nmcusd.dates.out %>% 
        complete(att.date = seq.Date(ymd("2022-08-01"), 
                                     ymd("2023-02-28"),  
                                     "day")) # %>%
    #    mutate(n = replace_na(n,0)) # %>%
        
        # mutate(weekday = wday(att.date, label = T, week_start = 1), 
        #        month = month(att.date, label = T, abbr = F),
        #        week = isoweek(att.date),
        #        day = day(att.date),
        #        text_col = "black",
        #        perc = n/1000
        #        )
    
    
    calendR(start_date = "2022-08-01", # Custom start date
            end_date = "2023-02-28" , # "2023-02-28",
            special.days = dat_pr$n[1:length(dat_pr$n)], # Vector of the same length as the number of days of the year
            # gradient = TRUE,      # Set gradient = TRUE to create the heatmap
            special.col = c("red","lightblue"),
          #  special.col = "red", # Color of the gradient for the highest value
            low.col = "white",
            title = paste0("Absences for ",stu.id),
            subtitle = "Missed days are in Red, Nonschool Days are in Light Blue"
                )   
    
    
}


nm.stu.cal(27742)

ggsave(here("output","nmcusd",paste0("Student 27742.png")), width = 9, height = 6)

nm.stu.cal(28856)

ggsave(here("output","nmcusd",paste0("Student 28856.png")), width = 9, height = 6)

nm.stu.cal(7000021717)

ggsave(here("output","nmcusd",paste0("Student 7000021717.png")), width = 9, height = 6)


###

nm.stud.count <- nmcusd.dates %>%
    group_by(`Student ID`) %>%
    summarise(sum(`Full Day Amount`))



### END -------
