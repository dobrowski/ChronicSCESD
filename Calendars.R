
# This file is to create heat maps based on strudent absenses and also indiviudal student absense calendars


### Libraries -------


library(tidyverse)
library(readxl)
library(here)
library(calendR)
library(lubridate)
library(googlesheets4)


### Import files ------



alisal.dates <- read_sheet("https://docs.google.com/spreadsheets/d/1pNICW5ld8yvoHBTrChtjiHDxwDWzGNw7PwQrk18ureA/edit#gid=808158362",
                           range = "A:D")




nmcusd.dates <- read_xlsx(here("data","nmcusd" , "NMCUSD Student Chronic Absenteeism File_2022-2023.xlsx"),
                          sheet = "Student Absence Details",
                          col_types = c("text","numeric","text","text","date","numeric","text","text","text"))




soledad.dates.elem <- read_xlsx(here("data", "soledad" ,"Elementary ATT data.xlsx"),
                    #      sheet = "Student Absence Details",
                     #     col_types = c("text","numeric","text","text","date","numeric","text","text","text")
                          ) %>%
    mutate(Date = mdy(Date))


soledad.dates.high <- read_xlsx(here("data", "soledad" ,"SHS AND MSMS ATT DATA.xlsx"),
                                #      sheet = "Student Absence Details",
                                     col_types = c("numeric","numeric", rep( "text",13) )
) %>%
    mutate(Date = mdy(Date))


soledad.demo <- read_xlsx(here("data", "soledad" ,"DEMOGRAPHICS.xlsx"),
                          #      sheet = "Student Absence Details",
                    #      col_types = c("numeric","numeric", rep( "text",13) )
)





scesd.dates <- read_xlsx(here("data", "scesd", "SCESD Chronic Abs Data File1_Abs Days 01.22.24.xlsx"),
                           sheet = "Salinas City 080923_012224",
                    #       col_types = c("text","numeric","text","text","date","numeric","text","text","text")
                         ) %>%
    mutate(Date = mdy(Date))

scesd.demo2 <- read_xlsx(here("data", "scesd", "SCESD Chronic Abs Data File3_ Demographics 01.22.24.xlsx"),
                         sheet = "SCESD 080923_012224 Demographic",
                         #       col_types = c("text","numeric","text","text","date","numeric","text","text","text")
) 



### Clean date sets -------


soledad.dates <- soledad.dates.elem %>%
    bind_rows(soledad.dates.high) %>%
    filter(`All day` %in% c("I","U","X","Q","A","C","O","S","Z")) %>%
    left_join(soledad.demo)
    

scesd.dates <- scesd.dates %>%
    left_join(scesd.demo2)


nmcusd.dates <- nmcusd.dates %>%
    mutate(`Student ID` = as.character(`Student ID`)) %>%
    left_join(nmcusd.demo) %>%
    filter((`Full Day Amount` == 1)) %>%
    rename(Date = `Attendance Date`)



ausd.grade <- ausd.joint %>%
    transmute(`Student ID` = as.character(LocalID) %>%
                  str_trim()
              , Grade = str_trim(Grade) 
              )%>%
    distinct()


alisal.dates <- alisal.dates %>%
    mutate(`Student ID` = as.character(`Student ID`)) %>%
    left_join(ausd.demo.rev, by = c("Student ID" = "LocalID")) %>%
    left_join(ausd.grade)

### Heat Maps ---------


cal.heat <- function(df, date.col , dist , tit) {
    
    
    dates.out <- df %>%
 #       filter(`Full Day Amount` == 1) %>%
        mutate(att.date = as_date({{date.col}})) %>%
        group_by(att.date) %>%
        count() %>%
        ungroup()
    
    
    dat_pr <- dates.out %>% 
        complete(att.date = seq.Date(min(att.date), 
                                     max(att.date), 
                                     "day"))  %>%
        mutate(n = replace_na(n,0))  %>%
        mutate(text_col = "black",
               perc = n/max(n))
    
    write_csv(dat_pr,
              here("output",dist,paste0(dist, " - " ,tit,".csv")) 
              )
    
    
    calendR( from = min(dat_pr$att.date), # Custom start date
             to = max(dat_pr$att.date) , # "2023-02-28",
             special.days = dat_pr$perc[1:length(dat_pr$perc)], # Vector of the same length as the number of days of the year
            gradient = TRUE,      # Set gradient = TRUE to create the heatmap
            special.col = "darkblue", # Color of the gradient for the highest value
            low.col = "white",
            title = paste0(dist, " - " ,tit), 
            subtitle = "The more intense the Blue color, the larger the number of students absent"
    )                       # Color of the gradient for the lowest value
    
    
    ggsave(here("output",dist,paste0(dist, " - " ,tit,".png")), width = 9, height = 6)
    
}

### NMCUSD ----

nmcusd.dates %>%
    filter(Grade %in% c("TK","K","1","2")
           ) %>%
    cal.heat(date.col =  `Date` , "North Monterey" , "TK-2 Students")



nmcusd.demo %>%
    filter(Grade %in% c("TK","K","01","02"),
           `Homeless Student` == "Y"
    )


nmcusd.dates %>%
    filter(Grade %in% c("TK","K")
           ) %>%
    cal.heat(date.col =  `Date` , "North Monterey" , "TK-K Students")


nmcusd.demo %>%
    filter(Grade %in% c("TK","K")
    )


nmcusd.dates %>%
    filter(Grade %in% c("TK","K","1","2"),
           SWD == "Y"
           ) %>%
    cal.heat(date.col =  `Date` , "North Monterey" , "TK-2 Students with Disabilities")

nmcusd.dates %>%
    filter(Grade %in% c("TK","K","1","2"),
           `Homeless Student` == "Y"
           ) %>%
    cal.heat(date.col =  `Date` , "North Monterey" , "TK-2 Homeless Students")



### SCESD -----

scesd.dates2 <- scesd.dates %>% 
    filter(Date <= mdy("1-22-2024")) %>%
    mutate(dated = Date)

scesd.dates2 %>%
    cal.heat(date.col =  dated , "Salinas City" ,"All Students")


scesd.demo2%>%
    filter(Grade %in% c(-1,0))


scesd.dates %>%
    filter(`Special Ed Value` == "Yes") %>%
    cal.heat(date.col =  Date , "Salinas City" ,"SPED")

scesd.dates %>%
    filter(`Homeless Value` == "Yes") %>%
    cal.heat(date.col =  Date , "Salinas City" ,"Homeless")

scesd.dates %>%
    filter(Grade %in% c(-1,0)) %>%
    cal.heat(date.col =  Date , "Salinas City" ,"TK-K")

scesd.dates %>%
    filter(School == "Boronda DIAS") %>%
    cal.heat(date.col =  Date , "Salinas City" ,"Boronda DIAS")


### Soledad ----

soledad.dates %>%
    cal.heat(date.col =  Date , "Soledad" ,"All Students")


soledad.demo%>%
    filter(Grade %in% c(-1,0))


soledad.dates %>%
    filter(`SPECIALED Value` == "Yes") %>%
    cal.heat(date.col =  Date , "Soledad" ,"SPED Students")

soledad.dates %>%
    filter(`HOMELESS Value` == "Yes") %>%
    cal.heat(date.col =  Date , "Soledad" ,"Homeless Students")

soledad.dates %>%
    filter(Grade %in% c(-1,0)) %>%
    cal.heat(date.col =  Date , "Soledad" ,"TK-K Students")

# Alisal


alisal.dates %>%
    filter(Date <=ymd("2023-03-30")) %>%. # Because of future absenses listed
    cal.heat(date.col =  Date , "Alisal" ,"All Students")


ausd.demo.rev %>%
    left_join(ausd.grade, by = c("LocalID" = "Student ID" )) %>%
    filter(Grade == "0")

ausd.demo.rev %>%
    filter(S_CA_STU_X.PrimaryResidence == "Homeless")

alisal.dates %>%
    filter(Grade == "0") %>%
    cal.heat(date.col =  Date , "Alisal" ,"TK-K Students")


alisal.dates %>%
    filter(SWD == "SWD") %>%
    cal.heat(date.col =  Date , "Alisal" ,"SWD Students")


alisal.dates %>%
    filter(S_CA_STU_X.PrimaryResidence == "Homeless") %>%
    cal.heat(date.col =  Date , "Alisal" ,"Homeless Students")



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


### Student Holidays -------


nm.holiday <- tribble(~ Date, ~n,
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
    mutate(Date = ymd(Date)) %>%
    add_column(LEA = "nmcusd")




soledad.holiday <- tribble(~ Date, ~n,
                      "2022-08-01", "Holiday",
                      "2022-08-02", "Holiday",
                      "2022-08-03", "Holiday",
                      "2022-08-04", "Holiday",
                      "2022-08-05", "Holiday",
                      "2022-08-08", "Holiday",
                      "2022-08-09", "Holiday",
                      "2022-09-05", "Holiday",
                      "2022-10-07", "Holiday",
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
                      "2023-02-13", "Holiday",
                      "2023-02-20", "Holiday",
                      "2023-03-10", "Holiday"
                      
) %>%
    mutate(Date = ymd(Date)) %>%
    add_column(LEA = "soledad")







scesd.holiday <- tribble(~ Date, ~n,
                           "2022-08-01", "Holiday",
                           "2022-08-02", "Holiday",
                           "2022-08-03", "Holiday",
                           "2022-08-04", "Holiday",
                           "2022-08-05", "Holiday",
                         "2022-09-05", "Holiday",
                         "2022-09-16", "Holiday",
                           "2022-10-10", "Holiday",
                         "2022-11-01", "Holiday",
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
                           "2023-01-16", "Holiday",
                           "2023-02-13", "Holiday",
                           "2023-02-20", "Holiday",
                         "2023-02-24", "Holiday"
) %>%
    mutate(Date = ymd(Date)) %>%
    add_column(LEA = "scesd")




alisal.holiday <- tribble(~ Date, ~n,
                         "2022-08-01", "Holiday",
                         "2022-08-02", "Holiday",
                         "2022-09-05", "Holiday",
                         "2022-09-23", "Holiday",
                         "2022-10-10", "Holiday",
                         "2022-10-28", "Holiday",

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
                         "2023-01-16", "Holiday",
                         "2022-01-23", "Holiday",
                         
                         "2023-02-13", "Holiday",
                         "2023-02-20", "Holiday",
                         "2023-02-24", "Holiday"
                         
) %>%
    mutate(Date = ymd(Date)) %>%
    add_column(LEA = "alisal")


holidays <- bind_rows(nm.holiday,soledad.holiday) %>%
    bind_rows(alisal.holiday) %>%
    bind_rows(scesd.holiday)




#### Student Calendar Function -------



stu.cal <- function(df, dist, stu.id, enr_date = "2022-08-01", end_date = "2023-03-15") {

    
holidays <-  holidays %>%
    filter(dist == LEA)
        
    
    dates.out <- df %>%
        filter(`Student ID` %in% stu.id) %>%
        group_by(Date) %>%
        count() %>%
        ungroup() %>%
        mutate(n = "Absent") %>%
        bind_rows(holidays)



    dat_pr <- dates.out %>%
        complete(Date = seq.Date(ymd(enr_date),
                                     ymd(end_date),
                                     "day"))


    calendR(start_date = ymd(enr_date), # Custom start date
            end_date = ymd(end_date), # "2023-02-28",
            special.days = dat_pr$n[1:length(dat_pr$n)], # Vector of the same length as the number of days of the year
            special.col = c("red","lightblue"),
            low.col = "white",
            title = paste0("Absences for ",stu.id),
            subtitle = "Missed days are in Red, Nonschool Days are in Light Blue"
    )


}


stu.cal(df = nmcusd.dates ,
        "nmcusd",
        stu.id = 27762,
        end_date = "2023-02-28")


stu.cal(df = scesd.dates ,
        "scesd",
        stu.id = 1202001916,
        end_date = "2023-03-15")


stu.cal(df = soledad.dates ,
        "soledad",
        stu.id = 15562,
        end_date = "2023-03-15")


### Old NMCUSD version ------
# 
# nm.stu.cal <- function(stu.id) {
#     
#     
#     nmcusd.dates.out <- nmcusd.dates %>%
#         filter(`Full Day Amount` == 1,
#                `Student ID` %in% stu.id) %>%
#         mutate(att.date = as.Date(Date)) %>%
#         group_by(att.date) %>%
#         count() %>%
#         ungroup() %>%
#         mutate(n = "Absent") %>%
#         bind_rows(nm.holiday)
#     
#     
#     
#     dat_pr <- nmcusd.dates.out %>% 
#         complete(att.date = seq.Date(ymd("2022-08-01"), 
#                                      ymd("2023-02-28"),  
#                                      "day")) # %>%
#     #    mutate(n = replace_na(n,0)) # %>%
#     
#     # mutate(weekday = wday(att.date, label = T, week_start = 1), 
#     #        month = month(att.date, label = T, abbr = F),
#     #        week = isoweek(att.date),
#     #        day = day(att.date),
#     #        text_col = "black",
#     #        perc = n/1000
#     #        )
#     
#     
#     calendR(start_date = "2022-08-01", # Custom start date
#             end_date = "2023-02-28" , # "2023-02-28",
#             special.days = dat_pr$n[1:length(dat_pr$n)], # Vector of the same length as the number of days of the year
#             # gradient = TRUE,      # Set gradient = TRUE to create the heatmap
#             special.col = c("red","lightblue"),
#             #  special.col = "red", # Color of the gradient for the highest value
#             low.col = "white",
#             title = paste0("Absences for ",stu.id),
#             subtitle = "Missed days are in Red, Nonschool Days are in Light Blue"
#     )
#     
#   #  dat_pr$n[1:length(dat_pr$n)]
#     
# }
# 
# 
# nm.stu.cal(27742)
# 
# ggsave(here("output","nmcusd",paste0("Student 27742.png")), width = 9, height = 6)
# 
# nm.stu.cal(28856)
# 
# ggsave(here("output","nmcusd",paste0("Student 28856.png")), width = 9, height = 6)
# 
# nm.stu.cal(7000021717)
# 
# ggsave(here("output","nmcusd",paste0("Student 7000021717.png")), width = 9, height = 6)
# 
# 
# ###
# 
# nm.stud.count <- nmcusd.dates %>%
#     group_by(`Student ID`) %>%
#     summarise(sum(`Full Day Amount`))
# 
