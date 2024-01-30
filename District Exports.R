
#### Libraries -------

library(tidyverse)
library(googlesheets4)
library(janitor)
library(readxl)
library(ggtext)
library(here)
library(MCOE)
library(scales)

#### Imports --------
calendars.23.24 <- read_sheet("https://docs.google.com/spreadsheets/d/16K_pA937FH7MxOqyuaVPuSUY3n8xCyIvi7jwGphIebU/edit#gid=0")


calendars.23.24v2 <- calendars.23.24 %>% 
        mutate(woy = (Date)%>% week()) %>%
        group_by(woy) %>%
        mutate(week.start = min((Date))) %>%
    pivot_longer(cols = c(alisal,nmcusd,scesd,soledad), names_to = "District") %>%
    group_by(woy, District) %>%
    transmute(school.days = sum(value)) %>%
    distinct()
    


##### Salinas City --------

## 2023-24 -------



scesd.month.students.23 <- read_xlsx(here("data","scesd","SCESD Chronic Abs Data File1_Abs Days 01.22.24.xlsx"),
                                   # sheet = "2023"
                                    ) %>%
    clean_names(case = "snake") %>%
    mutate(year = "2023",
           date = mdy(date)
           )

# Virtual Academy
scva.month.students.23 <- read_xlsx(here("data","scesd","SCVA Chronic Abs Data File1_Abs Days 01.19.24.xlsx"),
                                     # sheet = "2023"
) %>%
    clean_names(case = "snake") %>%
    mutate(year = "2023",
           date = mdy(date)
    )



scesd.month.students.22 <- read_xlsx(here("data","scesd","SCESD Chronic Abs Data File1_Abs Days 2022-23.xlsx"),
                                     # sheet = "2023"
) %>%
    clean_names(case = "snake") %>%
    mutate(year = "2022",
           date = mdy(date)
    )



scva.month.school.enr.23 <- read_xlsx(here("data","scesd","SCVA Chronic Abs Data File3_Demographics 01.19.24.xlsx"),
                                       # sheet = "2023"
)

scesd.month.school.enr.23 <- read_xlsx(here("data","scesd","SCESD Chronic Abs Data File3_Demographics 12.12.23.xlsx"),
                                       # sheet = "2023"
                                       ) %>%
    bind_rows(scva.month.school.enr.23) %>%
    clean_names(case = "snake") %>%
 #   group_by(school) %>%
    transmute(enrollment = n()) %>%
    distinct()

scesd.month.students.demo.23 <- read_xlsx(here("data","scesd","SCESD Chronic Abs Data File3_Demographics 12.12.23.xlsx"),
          # sheet = "2023"
) %>%
    clean_names(case = "snake")

scesd.month.students.demo.22 <- read_xlsx(here("data","scesd","SCESD Chronic Abs Data File3_Demographics.xlsx"),
                                          # sheet = "2023"
) %>%
    clean_names(case = "snake")



## 2022-23 -------

scesd.chr.22 <- read_xlsx(here("data", "scesd", "SCESD Chronic Abs Data File2_Att Calc.xlsx"),
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










##### Alisal ---------

## 2023-24 -------
ausd.month.students.demo.23 <- read_xlsx(here("data","alisal","monthly","StudentAbsenceSummaryDec.xlsx"),
                                    sheet = "2023",
                                    skip = 2 ) %>%
    clean_names(case = "snake") %>%
    mutate(year = "2023",
           student_id = ssid) 

ausd.month.students.22 <- read_xlsx(here("data","alisal","monthly","StudentAbsenceSummaryDec.xlsx"),
                                    sheet = "2022",
                                    skip = 2) %>%
    mutate(year = "2022") %>%
    clean_names(case = "snake")

ausd.month.schools <- read_xlsx(here("data","alisal","monthly","October 2023 & October 2022 Attendance.xlsx"),
                                sheet = "long") 



ausd.month.student.abs.23 <- read_csv(here("data","alisal","monthly","AlisalStudentAbsenceDates.csv"),
                                       skip = 1,
                                    #   col_types = "text"
                                      ) %>%
    clean_names(case = "snake") %>%
    pivot_longer(cols = (starts_with("abs")), values_to = "date") %>%
    na.omit() %>%
    select(-name) %>%
    mutate(date = mdy(date),
           student_id = ssid) %>%
    filter(date < ymd("2025-1-1"))


# ausd.ADA.schools <- read_xlsx(here("data","alisal","monthly","ADA_ADM by Date.xlsx"),
# #                                sheet = "long"
#                                 range = "A5:E65") 

ausd.ADA.schools.step <- tibble(sheet = excel_sheets(here("data","alisal","monthly","ADA_ADM by Date.xlsx"))) %>%
    mutate(
        data = sheet %>% map2(sheet, ~read_xlsx(here("data","alisal","monthly","ADA_ADM by Date.xlsx"),
                                                     sheet = .x,
                                                     range = "A5:E85") %>% mutate(school = .x) ) #Update range to cover all dates
    ) %>%
    pull(data) %>%
    bind_rows() %>%
    clean_names(case = "snake")

ausd.ADA.schools <- ausd.ADA.schools.step %>%
    select(-number, -day) %>% 
    mutate(woy = mdy(date)%>% week(),
           membership = membership %>% str_trim() %>% as.numeric()
           ) %>%
    group_by(woy, school) %>%
    transmute(week.start = min(mdy(date)),
           weekly.mem = sum(membership),
           weekly.att = sum(attendance),
           weekly.ADA = weekly.att/weekly.mem
           ) %>%
    distinct()
    




ausd.month.school.enr.23 <- ausd.month.student.abs.23 %>%
    clean_names(case = "snake") %>%
    group_by(school ) %>%
    transmute(enrollment = n()) %>%
    distinct()





##### North Monterey ---------

## 2023-24 -------

nmcusd.month.students.23 <- read_xlsx(here("data","nmcusd","NMCUSD Student Attendance_Chronic Absenteesim Project.xlsx"),
                                    sheet = "Student Daily Absence Details") %>%
    clean_names(case = "snake") %>%
    rename(school = school_name,
           student_id = perm_id) %>%
    filter(school_year == "2023-2024") %>%
    filter(!str_detect(reason_1,"Early Out|Tardy|Home|Late" )) %>%
    mutate(date = ymd(date))
    


nmcusd.month.students.22 <- read_xlsx(here("data","nmcusd","NMCUSD Student Attendance_Chronic Absenteesim Project.xlsx"),
                                      sheet = "Student Daily Absence Details") %>%
    clean_names(case = "snake") %>%
    rename(school = school_name,
           student_id = perm_id) %>%
    filter(school_year == "2022-2023") %>%
    filter(!str_detect(reason_1,"Early Out|Tardy|Home|Late" )) %>%
    mutate(date = ymd(date))




tabyl(nmcusd.month.students.23$grade)

nmcusd.month.students.demo.23 <- read_xlsx(here("data","nmcusd","NMCUSD Student Attendance_Chronic Absenteesim Project.xlsx"),
                                      sheet = "Student Demographics") %>%
    clean_names(case = "snake") %>%
    rename(student_id = perm_id) 
    

nmcusd.month.students.demo.22.step <- read_xlsx(here("data","nmcusd","NMCUSD Student Chronic Absenteeism File_2022-2023.xlsx"),
                                           sheet = "Student Attendance Summary") %>%
    clean_names(case = "snake") %>%
    select(school, student_id) %>%
    mutate(student_id = as.character(student_id)) %>%
    distinct()

nmcusd.month.students.demo.22 <- read_xlsx(here("data","nmcusd","NMCUSD Student Chronic Absenteeism File_2022-2023.xlsx"),
                                           sheet = "Student Demographics") %>%
    clean_names(case = "snake") %>%
    filter(grade %in% c("TK","K","01","02","03")) %>%
    left_join(nmcusd.month.students.demo.22.step)




nmcusd.ADA.schools <- read_xlsx((here("data","nmcusd","02b) NMCUSD Weekly Student Attendance_Chronic Absenteeism Project_MCOE File.xlsx"))) %>%
    clean_names(case = "snake") %>%
    mutate(week.start = str_sub(dates,1,10), 
               woy = mdy(week.start)%>% week(),
    ) # %>%
    # group_by(woy, school) %>%
    # transmute(week.start = min(mdy(date)),
    #           weekly.mem = sum(membership),
    #           weekly.att = sum(attendance),
    #           weekly.ADA = weekly.att/weekly.mem
    # ) %>%
    # distinct()






nmcusd.month.school.enr.23 <- read_xlsx(here("data","nmcusd","NMCUSD Student Attendance_Chronic Absenteesim Project.xlsx"),
                                        sheet = "Student Attendance Summary") %>%
    clean_names(case = "snake") %>%
    filter(school_year == "2023-2024") %>%
    group_by(school = school_name) %>%
    transmute(enrollment = n()) %>%
    distinct()







##### Soledad ---------

## 2023-24 -------


sol.sch.chr.23 <- read_sheet("https://docs.google.com/spreadsheets/d/1SXB3EZa54WrUJ0DoTwB2YLdEzQ_SOHX892w2hmsMgYY/edit#gid=0")



soledad.month.students.23 <- read_xlsx(here("data","soledad","SUSD ChronicAbsenteeism11.1.xlsx"),
                                 #     sheet = "Student Daily Absence Details"
                                      ) %>%
    clean_names(case = "snake")




#

sol.month.students.demo.23 <- read_xlsx(here("data","soledad","monthly","PrintQueryToExcel_20231106_101837_949452b.xlsx"),
                                         # sheet = "2023",
                                         # skip = 2 
                                         ) %>%
    clean_names(case = "snake") %>%
    mutate(year = "2023") %>%
    mutate(school = as.character(school)) %>%
    mutate(
    school_name = recode(school,
                         "1" = "San Vicente Elementary",
                         "2" = "Gabilan Elementary",
                         "3" = "Main Street Middle",
                         "4" = "Soledad High",
                         "5" = "Rose Ferrero Elementary",
                         "6" = "Pinnacles High",
                         "9" = "Frank Ledesma Elementary",
                         "10" = "Jack Franscioni Elementary",
                         "11" = "Preschool",
                         "13" = "Indepenent Study 9-12",
                         "14" = "Virtual Academy",
                         "15" = "Virtual Academy",
                         "17" = "SPED Preschool"
    )
    ) %>%
    mutate(school = coalesce(school_name, school))



sol.month.students.demo.22 <- read_xlsx(here("data","soledad","DEMOGRAPHICS.xlsx"),
                                        # sheet = "2023",
                                        # skip = 2 
) %>%
    clean_names(case = "snake") %>%
    mutate(year = "2022") %>%
    mutate(
        school_name = recode(school,
                             "1" = "San Vicente Elementary",
                             "2" = "Gabilan Elementary",
                             "3" = "Main Street Middle",
                             "4" = "Soledad High",
                             "5" = "Rose Ferrero Elementary",
                             "6" = "Pinnacles High",
                             "9" = "Frank Ledesma Elementary",
                             "10" = "Jack Franscioni Elementary",
                             "11" = "Preschool",
                             "13" = "Indepenent Study 9-12",
                             "14" = "Virtual Academy",
                             "15" = "Virtual Academy",
                             "17" = "SPED Preschool"
        )
    )




sol.month.student.abs.23 <- read_xlsx(here("data","soledad","monthly",
                                           "Student Absences Nov Sol.xlsx"
                                      #     "PrintQueryToExcel_20231106_101931_532978a.xlsx"
                                           ),
                                     # skip = 1,
                                      #   col_types = "text"
) %>%
    clean_names(case = "snake") %>%
    mutate(date = mdy(date)
           ) %>%
    filter(date < ymd("2023-10-29"))%>%
    mutate(school = as.character(school)) %>%
    mutate(
        school_name = recode(school,
                             "1" = "San Vicente Elementary",
                             "2" = "Gabilan Elementary",
                             "3" = "Main Street Middle",
                             "4" = "Soledad High",
                             "5" = "Rose Ferrero Elementary",
                             "6" = "Pinnacles High",
                             "9" = "Frank Ledesma Elementary",
                             "10" = "Jack Franscioni Elementary",
                             "11" = "Preschool",
                             "13" = "Indepenent Study 9-12",
                             "14" = "Virtual Academy",
                             "15" = "Virtual Academy",
                             "17" = "SPED Preschool"
                             )
    ) %>%
    mutate(school = coalesce(school_name, school))





sol.month.student.abs.22.step <- read_xlsx(here("data","soledad","SHS AND MSMS ATT DATA.xlsx"),
                                      # skip = 1,
                                      #   col_types = "text"
) %>%
    clean_names(case = "snake") %>%
    mutate(date = mdy(date)
    ) 

sol.month.student.abs.22 <- read_xlsx(here("data","soledad","Elementary ATT data.xlsx"),
                                           # skip = 1,
                                           #   col_types = "text"
) %>%
    clean_names(case = "snake") %>%
    mutate(date = mdy(date)
    ) %>% 
    bind_rows(sol.month.student.abs.22.step)%>%
    mutate(
        school_name = recode(school,
                             "1" = "San Vicente Elementary",
                             "2" = "Gabilan Elementary",
                             "3" = "Main Street Middle",
                             "4" = "Soledad High",
                             "5" = "Rose Ferrero Elementary",
                             "6" = "Pinnacles High",
                             "9" = "Frank Ledesma Elementary",
                             "10" = "Jack Franscioni Elementary",
                             "11" = "Preschool",
                             "13" = "Indepenent Study 9-12",
                             "14" = "Virtual Academy",
                             "15" = "Virtual Academy",
                             "17" = "SPED Preschool"
        )
    ) %>%
    filter(!is.na(all_day))




sol.month.school.enr.23 <- sol.month.students.demo.23 %>%
    group_by(school) %>%
    transmute(enrollment = n()) %>%
    distinct() %>%
    mutate(school = as.character(school))








#### Grouping -------

school.enr.23 <- scesd.month.school.enr.23 %>%
    bind_rows(nmcusd.month.school.enr.23) %>%
    bind_rows(ausd.month.school.enr.23) %>%
    bind_rows(sol.month.school.enr.23) %>%
    mutate(year = "2023")

#### Functions -------



# temp <- scesd.month.students.23 %>% 
#     mutate(woy = mdy(Date)%>% week()) %>%
#     group_by(woy, School) %>%
#     transmute(week.start = min(mdy(Date)),
#               days.absent = n()) %>%
#     distinct() %>%
#     ungroup() %>%
#     filter(str_detect(School, "Boronda Meadows")) 
#     
# med.line <- temp %>%
#     filter(woy != min(woy),
#            woy != max(woy),
#            ) %>%
#     select(days.absent) %>%
#     transmute(med.line = median(days.absent)) %>%
#     distinct() %>%
#     unlist()
#     
#     temp %>%
# ggplot( aes(x = week.start, y = days.absent) ) +
# geom_point() +
#         geom_hline(aes(yintercept = med.line)) +
#         scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%d-%Y"
#                      ) +
#         theme_minimal()
    


# Number of Days Absent by Week 
    
graph.run.df <- function(df, date.column, school.column, school.name) {
    
    df.new <- df %>% 
        mutate(woy = {{date.column}}%>% week()) %>%
        group_by(woy, {{school.column}}) %>%
        transmute(week.start = min({{date.column}}),
                  days.absent = n()) %>%
        distinct() %>%
        ungroup() %>%
        filter(str_detect({{school.column}}, school.name)) 
    
    med.line <- df.new %>%
        filter(woy != min(woy),
               woy != max(woy),
        ) %>%
        select(days.absent) %>%
        transmute(med.line = median(days.absent)) %>%
        distinct() %>%
        unlist()
    
    df.new %>%
        ggplot( aes(x = week.start, y = days.absent) ) +
        geom_point() +
        geom_hline(aes(yintercept = med.line)) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%d-%Y"
        ) +
        theme_minimal() +
        labs(title = paste0(school.name, " Number of Days Absent by Week"),
             subtitle = "Horizonal line represents Median without first and last week",
             y = "Total Days Missed",
             x = "Day Starting School Week")
 }   


graph.run.df(scesd.month.students.23, date, school, "Laurel")

nmcusd.month.students.23 %>%
graph.run.df( date, school, "Castroville")


ausd.month.student.abs.23 %>%
    graph.run.df( date, school, "Creekside")



# Looks at how many school days are in a week and divides the missed days by that.  
    
graph.run.dfv2 <- function(df, dist ,date.column, school.column, school.name) {
        
        df.new <- df %>% 
            mutate(woy = {{date.column}}%>% week()) %>%
            group_by(woy, {{school.column}}) %>%
            transmute(week.start = min({{date.column}}),
                      days.absent = n()) %>%
            distinct() %>%
            ungroup() %>%
            filter(str_detect({{school.column}}, school.name)) %>% 
            left_join(
                calendars.23.24v2 %>%
                    filter(str_detect(District,dist))
            ) %>%
            mutate(weekly.absences.daily.average = days.absent/school.days)
        
        med.line <- df.new %>%
            filter(woy != min(woy),
                   woy != max(woy),
            ) %>%
            select(weekly.absences.daily.average) %>%
            transmute(med.line = median(weekly.absences.daily.average)) %>%
            distinct() %>%
            unlist()
        
        df.new %>%
            ggplot( aes(x = week.start, y = weekly.absences.daily.average) ) +
            geom_point() +
            geom_hline(aes(yintercept = med.line)) +
            scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%d-%Y"
            ) +
            theme_minimal() +
            labs(title = paste0(school.name, " Average Days Absent in a Week"),
                 subtitle = "Horizonal line represents Median without first and last week",
                 y = "Average Count of Students Missing Days per Week",
                 x = "Day Starting School Week")
    }   
    



graph.run.dfv2(scesd.month.students.23, "scesd" ,date, school, "Laurel")

nmcusd.month.students.23 %>%
    graph.run.dfv2("nmcusd" , date, school, "Echo Valley")


ausd.month.student.abs.23 %>%
    graph.run.dfv2("alisal" , date, school, "Creekside")




# Looks at total students enrolled 





graph.run.dfv3 <- function(df, dist ,date.column, school.column, school.name, note = "") {
    
    df.new <- df %>% 
        mutate(woy = {{date.column}}%>% week()) %>%
        group_by(woy, {{school.column}}) %>%
        transmute(week.start = min({{date.column}}),
                  days.absent = n()) %>%
        distinct() %>%
        ungroup() %>%
        filter(str_detect({{school.column}}, school.name)) %>% 
        left_join(
            calendars.23.24v2 %>%
                filter(str_detect(District,dist))
        ) %>%
        mutate(weekly.absences.daily.average = days.absent/school.days)%>%
        left_join(school.enr.23) %>%
        mutate(weekly.ADA = 1- weekly.absences.daily.average/enrollment)
    
    med.line <- df.new %>%
        filter(woy != min(woy),
               woy != max(woy),
        ) %>%
        select(weekly.ADA) %>%
        transmute(med.line = median(weekly.ADA)) %>%
        distinct() %>%
        unlist()
    
    df.new %>%
        ggplot( aes(x = week.start, y = weekly.ADA) ) +
        geom_hline(aes(yintercept = med.line), color = "#DDA63A", linewidth = 1.5) +
        geom_point(size = 3, 
                   color = "#6C9BA6") +
        geom_richtext(fill = NA, label.color = NA, label = "<span style = 'color:#DDA63A;'>Median</span>", 
                      aes(x = mdy("8/6/2023"), y = med.line, hjust = 0, vjust = 0)) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%d-%Y"
        ) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        labs(title = paste0(school.name, note, " Average Daily Attendance in a Week"),
             subtitle = "<span style = 'color:#DDA63A;'>Horizonal line represents Median without first and last week</span>",
             y = "Average Percentage of Enrolled Students Attending Daily",
             x = "Day Starting School Week") +
        theme(plot.subtitle = element_markdown() )
}   






graph.run.dfv3(scesd.month.students.23, "scesd" ,date, school, "Laurel")

nmcusd.month.students.23 %>%
    graph.run.dfv3("nmcusd" , date, school, "Echo Valley", " TK-3")



ausd.month.student.abs.23 %>%
    graph.run.dfv3("alisal" , date, school, "Creekside")


sol.month.student.abs.23 %>%
    graph.run.dfv3("soledad" , date, school, "Main")


scesd.month.students.23 <- scesd.month.students.23 %>%
    filter(date < mdy("1-22-2024"))

for (i in unique(scesd.month.students.23$school)) {
    
    graph.run.dfv3(scesd.month.students.23, "scesd" ,date, school, i)
    
    ggsave(here("output","ADA","SCESD" ,"Nov 2023" ,paste0("Salinas City - ",i," ADA by Week", today(),".png")),
           width = 8,
           height = 4.5)
    
}

# Salinas City Virtual Academy


for (i in unique(scva.month.students.23$school)) {
    
    graph.run.dfv3(scva.month.students.23, "scesd" ,date, school, i)
    
    ggsave(here("output","ADA","SCESD" ,"Nov 2023" ,paste0("Salinas City - ",i," ADA by Week", today(),".png")),
           width = 8,
           height = 4.5)
    
}


temp <- scesd.month.students.23 %>% 
    mutate(woy = date%>% week()) %>%
    group_by(woy, school) %>%
    transmute(week.start = min(date),
              days.absent = n()) %>%
    distinct() %>%
    ungroup() %>%
    # filter(str_detect({{school.column}}, school.name)) %>% 
    left_join(
        calendars.23.24v2  %>%
            filter(str_detect(District,"scesd"))
    ) %>%
    mutate(weekly.absences.daily.average = days.absent/school.days)%>%
    left_join(school.enr.23) %>%
    mutate(weekly.ADA = 1- weekly.absences.daily.average/enrollment)

temp %>% 
    mutate(weekly.ADA = weekly.ADA * 100) %>%
    pivot_wider(names_from = school, values_from = weekly.ADA, id_cols = week.start) %>%
    arrange(week.start)

clipr::write_last_clip()


####


for (i in unique(nmcusd.month.students.23$school)) {
    
    graph.run.dfv3(nmcusd.month.students.23, "nmcusd" ,date, school, i, " TK-3")
    
    ggsave(here("output","ADA",paste0("North Monterey County TK-3 - ",i," ADA by Week", today(),".png")),
           width = 8,
           height = 4.5)
    
}


for (i in unique(sol.month.student.abs.23$school)) {
    
    graph.run.dfv3(sol.month.student.abs.23, "soledad" ,date, school, i, "")
    
    ggsave(here("output","ADA", "Soledad", "Nov 2023", paste0("Soledad - ",i," ADA by Week", today(),".png")),
           width = 8,
           height = 4.5)
    
}


 



##f# Alternative if given the weekly average ---


# ausd.ADA.schools %>%
#     filter(str_detect(school, "Creekside"))
#     
#     med.line <- ausd.ADA.schools %>%
#         filter(str_detect(school, "Creekside")) %>%
#         ungroup() %>%
#     select(weekly.ADA) %>%
#     transmute(med.line = median(weekly.ADA)) %>%
#     distinct() %>%
#     unlist()
# 
#     ausd.ADA.schools %>%
#         filter(str_detect(school, "Creekside")) %>%
#     ggplot( aes(x = week.start, y = weekly.ADA) ) +
#     geom_point() +
#     geom_hline(aes(yintercept = med.line)) +
#     scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%d-%Y"
#     ) +
#         scale_y_continuous(labels = scales::percent) +
#     theme_minimal() +
#     labs(title = paste0(# school.name, 
#                         " Average Daily Attendance in a Week"),
#          subtitle = "Horizonal line represents Median",
#          y = "Average Percentage of Enrolled Students Attending Daily",
#          x = "Day Starting School Week")


    
ADA.graph <- function(df, school.name) {
    

    
    med.line <- df %>%
        filter(str_detect(school, school.name)) %>%
        ungroup() %>%
        select(weekly.ADA) %>%
        transmute(med.line = median(weekly.ADA)) %>%
        distinct() %>%
        unlist()
    
    df %>%
        filter(str_detect(school, school.name)) %>%
        ggplot( aes(x = week.start, y = weekly.ADA) ) +
        geom_hline(aes(yintercept = med.line), color = "#DDA63A", linewidth = 1.5) +
        geom_point(size = 3, 
                   color = "#6C9BA6") +
        geom_richtext(fill = NA, label.color = NA, label = "<span style = 'color:#DDA63A;'>Median</span>", 
                      aes(x = mdy("8/6/2023"), y = med.line, hjust = 0, vjust = 0)) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%d-%Y"
        ) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        labs(title = paste0( school.name, 
            " Average Daily Attendance by Week"),
     #       subtitle = "<span style = 'color:#DDA63A;'>Horizonal line represents Median</span>",
            y = "Average Percentage of Enrolled Students Attending Daily",
            x = "Day Starting School Week") +
        theme(plot.subtitle = element_markdown() )
}    
   

ADA.graph(ausd.ADA.schools, "Creekside")
 
for (i in unique(ausd.ADA.schools$school)) {
    
    ADA.graph(ausd.ADA.schools, i)
    
    ggsave(here("output","ADA","Alisal" ,"2023 Dec" ,paste0("Alisal - ",i," ADA by Week", today(),".png")),
                width = 8,
                height = 4.5)
    
}
    
ausd.ADA.schools %>%
    filter(school == "Loya") %>%
    mutate(weekly.ADA = if_else(woy >= 41, weekly.ADA + .015, weekly.ADA),
           school = "Make-Believe") %>%
    ADA.graph( "Make-Believe")

ggsave(here("output","ADA",paste0("Make-Believe ADA by Week", today(),".png")),
       width = 8,
       height = 4.5)


### ADA based on Cronic for student group


graph.run.dfv4 <- function(df, dist, enrollment, title.name) {
    
    df.new <- df %>% 
        mutate(woy = date %>% week()) %>%
        group_by(woy) %>%
        transmute(week.start = min(date),
                  days.absent = n()) %>%
        distinct() %>%
        ungroup() %>%
        left_join(
            calendars.23.24v2 %>%
                filter(str_detect(District,dist))
        ) %>%
        mutate(weekly.absences.daily.average = days.absent/school.days,
               enrollment = enrollment) %>%
        mutate(weekly.ADA = 1- weekly.absences.daily.average/enrollment)
    
    
    med.line <- df.new %>%
        filter(woy != min(woy),
               woy != max(woy),
        ) %>%
        select(weekly.ADA) %>%
        transmute(med.line = median(weekly.ADA)) %>%
        distinct() %>%
        unlist()
    
    df.new %>%
        ggplot( aes(x = week.start, y = weekly.ADA) ) +
        geom_hline(aes(yintercept = med.line), color = "#DDA63A", linewidth = 1.5) +
        geom_point(size = 3, 
                   color = "#6C9BA6") +
        geom_richtext(fill = NA, label.color = NA, label = "<span style = 'color:#DDA63A;'>Median</span>", 
                      aes(x = mdy("8/6/2023"), y = med.line, hjust = 0, vjust = 0)) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%d-%Y"
        ) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
    labs(title = paste0(title.name, " Average Daily Attendance in a Week"),
         subtitle = "<span style = 'color:#DDA63A;'>Horizonal line represents Median without first and last week</span>",
         y = "Average Percentage of Enrolled Students Attending Daily",
         x = "Day Starting School Week") +
        theme(plot.subtitle = element_markdown() )
    
}   


ausd.month.student.abs.23 %>%
    graph.run.dfv4("alisal" , 500, "Creekside Students with Disabilities")

## Example for calculation for Subgroup

# SCESD
# SWD are focus
stud.list <- scesd.month.students.demo.23 %>% 
    filter(special_ed == "Yes") %>%
    pull(student_id)


scesd.month.students.23 %>%
    filter(student_id %in% stud.list ) %>%
    graph.run.dfv4("scesd" , length(stud.list), "Salinas City Students with Disabilities")


# AUSD 
stud.list <- ausd.month.students.demo.23 %>% 
    filter(swds == "Y",
           str_detect(school_name,"Creekside")
           ) %>%
    pull(student_id)


ausd.month.student.abs.23 %>%
    filter(ssid %in% stud.list ) %>%
    graph.run.dfv4("alisal" , length(stud.list), "Creekside for SWD")






# NMCUSD
# TK-3, which is all have data for
stud.list <- nmcusd.month.students.demo.23 %>% 
    filter(swd == "Yes") %>%
    pull(student_id)

nmcusd.month.students.23 %>%
    filter(student_id %in% stud.list ) %>%
    graph.run.dfv4("nmcusd" , length(stud.list), "North Monterey Students with Disabilities")





#### Graphs --------


# Soledad using the data they provided 

sol.sch.chr.23 %>%
    ggplot( aes(x = factor(School), y = `Percent Chronic`, group = Group, fill = Group, label = `Percent Chronic`) )+
    geom_col(position = "dodge") +
    geom_text(position = position_dodge(width = 0.9), ) +
    mcoe_theme + 
    labs(title = "Soledad Schools Percent Chronically Absent",
         subtitle = "As of November 1, 2023")

ggsave("Soledad Nov 1.png", width = 8, height = 4.5)







##  Comparison ADA  by School in a District 2022-23 v 2023-24


scesd.month.students.22 %>%
    filter(date <= mdy("10/28/2022"))

nmcusd.month.students.22 %>%
    filter(date <= mdy("10/28/2022"))

scesd.month.students.23 %>%
    filter(date <= mdy("10/27/2023"))

nmcusd.month.students.23 %>%
    filter(date <= mdy("10/27/2023"))



calendars.oct <- tribble(~district, ~year, ~school.days,
                         "alisal", "2022", 59,
                         "alisal", "2023", 56,
                         "nmcusd","2022", 56,
                         "nmcusd","2023", 55,
                         "scesd","2022", 57,
                         "scesd","2023", 56,
                         "soledad", "2022", 55,
                         "soledad", "2023", 55
                         
)




two.year.compare <- function(df.22, df.23, dist) {
    
df.22 <- df.22 %>%
        filter(date <= mdy("10/28/2022")) %>%
    mutate(year = "2022")
    
    df.23 %>%
        mutate(year = "2023") %>%
        filter(date <= mdy("10/27/2023")) %>%
        bind_rows(df.22) %>% 
        mutate(district = dist) %>%
          group_by(district, school, year)  %>%
          transmute( days.absent = n()) %>%
          distinct()  %>%
          ungroup() %>%
 
     left_join(
         calendars.oct) # %>%
#             filter(str_detect(school,"Laurel"))
#     ) %>%
#     mutate(weekly.absences.daily.average = days.absent/school.days,
#            enrollment = enrollment) %>%
#     mutate(weekly.ADA = 1- weekly.absences.daily.average/enrollment)

}

two.year.compare(nmcusd.month.students.22, nmcusd.month.students.23, "nmcusd")


stud.list <- scesd.month.students.demo.23 %>% 
    filter(special_ed == "Yes") %>%
    pull(student_id)


scesd.month.students.23 %>%
    filter(student_id %in% stud.list ) %>%
    group_by(student_id) %>%
    summarise(missed.count = n()) %>%
    filter(missed.count >= 6) %>%
    pull(student_id) %>%
    length()/length(stud.list)




#. Must update in future,  currently has threshold of 6 absences 
chronic.rate <- function(df.demo, df.absence, ssid.col) {
    
    stud.list <- df.demo %>% 
 #       filter(special_ed == "Yes") %>%
        pull({{ssid.col}})
    
    
chronic.list <- df.absence %>%
        filter({{ssid.col}} %in% stud.list ) %>%
        group_by({{ssid.col}}) %>%
        summarise(missed.count = n()) %>%
        filter(missed.count >= 6) %>%. #Note Hardcoded for 6 absences for through OCtober. 
        pull({{ssid.col}}) 

 tribble(~count, ~chronic, ~rate,
        length( stud.list),length(chronic.list), length(chronic.list)/length(stud.list)  )
    
}

chronic.rate(scesd.month.students.demo.23 %>% 
                        filter( str_detect(school, "Laurel"),
                            special_ed == "Yes"), 
             scesd.month.students.23,
             student_id)


unique(scesd.month.students.demo.23$school)

# Salinas City Chronic Rates 

for (i in unique(scesd.month.students.demo.23$school)) {
    
print(i)

chr.table <- chr.table %>%
    bind_rows(chronic.rate(scesd.month.students.demo.23 %>% 
                 filter( str_detect(school,i),
                         special_ed == "Yes"
                         ), 
             scesd.month.students.23,
             student_id) %>%
                 mutate(school = i)
)


}


for (i in unique(scesd.month.students.demo.22$school)) {
    
    print(i)
    
    chr.table <- chr.table %>%
        bind_rows(chronic.rate(scesd.month.students.demo.22 %>% 
                                   
                                   filter( str_detect(school,i),
                          #                 special_ed_value == "Yes"
                                   ), 
                               
                               scesd.month.students.22 %>%
                                   filter(date <= mdy("10/28/2022"))
                               ,
                               student_id) %>%
                      mutate(school = i)
        )
    
    
}


clipr::write_clip(chr.table)




# NMCUSD Chronic Rates 


chr.table <-chronic.rate(scesd.month.students.demo.23 %>% 
                             filter( str_detect(school, "Laurel"),
                                     special_ed == "Yes"), 
                         scesd.month.students.23,
                         student_id) %>%
    mutate(school = i) %>%
    filter(count == 0)

for (i in unique(nmcusd.month.students.demo.23$school)) {
    
    print(i)
    
    chr.table <- chr.table %>%
        bind_rows(chronic.rate(nmcusd.month.students.demo.23 %>% 
                                   filter( str_detect(school,i),
                #                           swd == "Yes"
                                   ), 
                               nmcusd.month.students.23,
                               student_id)  %>%
                      mutate(school = i)
        )
    
}


clipr::write_clip(chr.table)




for (i in unique(nmcusd.month.students.demo.22$school)) {
    
    print(i)
    
    chr.table <- chr.table %>%
        bind_rows(chronic.rate(nmcusd.month.students.demo.22 %>% 
                                   filter( str_detect(school,i),
                                                                      swd == "Y"
                                   ), 
                               nmcusd.month.students.22 %>%
                                   filter(date <= mdy("10/28/2022")),
                               student_id)  %>%
                      mutate(school = i)
        )
    
}


# Alisal Chronic Rates 

# These two are looking at October only, and comparing percent chronically absent in this month. 

chr.table <- ausd.month.students.demo.23 %>% 
#     filter(swds == "Y") %>%
    group_by(school_name) %>%
    summarise(total = n(),
              chronics = sum(chronically_absent == "Y", na.rm = TRUE),
              rate = chronics/total)


clipr::write_clip(chr.table)

chr.table <- ausd.month.students.22 %>% 
    filter(swds == "Y") %>%
    group_by(school_name) %>%
    summarise(total = n(),
              chronics = sum(chronically_absent == "Y", na.rm = TRUE),
              rate = chronics/total)


clipr::write_clip(chr.table)

# Standard using the days missed from the start of the school year

chr.table <-chronic.rate(scesd.month.students.demo.23 %>% 
                             filter( str_detect(school, "Laurel"),
                                     special_ed == "Yes"), 
                         scesd.month.students.23,
                         student_id) %>%
    mutate(school = i) %>%
    filter(count == 0)

for (i in unique(ausd.month.students.demo.23$school_name)) {
    
    print(i)
    
    chr.table <- chr.table %>%
        bind_rows(chronic.rate(ausd.month.students.demo.23 %>% 
                     filter( str_detect(school_name,i),
                               swds == "Y"
                     ), 
                 ausd.month.student.abs.23,
                 ssid)  %>%
        mutate(school = i)
    )
    
}


clipr::write_clip(chr.table)





# Soledad Chronic Rates 

chr.table <-chronic.rate(scesd.month.students.demo.23 %>% 
                             filter( str_detect(school, "Laurel"),
                                     special_ed == "Yes"), 
                         scesd.month.students.23,
                         student_id) %>%
    mutate(school = i) %>%
    filter(count == 0)

for (i in unique(sol.month.students.demo.23$school_name)) {
    
    print(i)
    
    chr.table <- chr.table %>%
        bind_rows(chronic.rate(sol.month.students.demo.23 %>% 
                                   filter( str_detect(school_name,i),
  #                                         specialed_value == "Yes"
                                   ), 
                               sol.month.student.abs.23,
                               student_id)  %>%
                      mutate(school = i)
        )
    
}


clipr::write_clip(chr.table)




chr.table <-chronic.rate(scesd.month.students.demo.23 %>% 
                             filter( str_detect(school, "Laurel"),
                                     special_ed == "Yes"), 
                         scesd.month.students.23,
                         student_id) %>%
    mutate(school = i) %>%
    filter(count == 0)

for (i in unique(sol.month.students.demo.22$school_name)) {
    
    print(i)
    
    chr.table <- chr.table %>%
        bind_rows(chronic.rate(sol.month.students.demo.22 %>% 
                                   filter( str_detect(school_name,i),
                  #                         specialed_value == "Yes"
                                   ), 
                               sol.month.student.abs.22 %>%
                                   filter(date <= mdy("10/28/2022")),
                               student_id)  %>%
                      mutate(school = i)
        )
    
}


clipr::write_clip(chr.table)












### Graphs for Chronic Rates


chronic.rate.graph <- function(dist, yr, dater, layers =1) {
    
    
read_sheet("https://docs.google.com/spreadsheets/d/1SXB3EZa54WrUJ0DoTwB2YLdEzQ_SOHX892w2hmsMgYY/edit#gid=0",
           sheet = str_to_lower(dist)) %>%
        filter( year == yr) %>%
    ggplot( aes(x = factor(school), y = rate, group = group, fill = group, label = percent(rate,0.1)) )+
    geom_col(position = "dodge") +
    geom_text(position = position_dodge(width = 0.9), ) +
    mcoe_theme + 
        scale_y_continuous(labels = scales::percent) +
        scale_x_discrete(guide = guide_axis(n.dodge = layers)) + #Fixes the overlapping axis labels to make them alternate if lots of columns
 #       {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        
    labs(title = paste0(dist, " Schools Percent Chronically Absent"),
         subtitle = paste0("As of ", dater )
    )+
        theme(plot.margin = margin(r = 10))

ggsave(here("output","ADA",paste0(dist," " ,yr, " Chronic Absenteeism Rates.png") ) , width = 8, height = 4.5)

}





chronic.rate.graph.sub <- function(dist, yr, dater, layers =1, sub) {
    
    
    read_sheet("https://docs.google.com/spreadsheets/d/1SXB3EZa54WrUJ0DoTwB2YLdEzQ_SOHX892w2hmsMgYY/edit#gid=0",
               sheet = str_to_lower(dist)) %>%
        filter(group == sub,
               year == yr) %>%
        ggplot( aes(x = factor(school), y = rate, label = percent(rate,0.1)) )+
        geom_col(position = "dodge", fill = "#DDA63A") +
        geom_text(position = position_dodge(width = 0.9), ) +
        mcoe_theme + 
        scale_y_continuous(labels = scales::percent) +
        scale_x_discrete(guide = guide_axis(n.dodge = layers)) + #Fixes the overlapping axis labels to make them alternate if lots of columns
        #       {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        
        labs(title = paste0(dist," ",sub,  " Students Percent Chronically Absent"),
             subtitle = paste0("As of ", dater )
        ) +
        theme(plot.margin = margin(t = 5,l = 5 , r = 25))
    
    ggsave(here("output","ADA",paste0(dist," ", yr ," ",sub, " Chronic Absenteeism Rates",dater,".png") ) , width = 8, height = 4.5)
    
}



# chronic.rate.graph("Soledad", "November 1, 2023")
    

chronic.rate.graph("NMCUSD TK-3",2023 , "October 27, 2023", 2)

chronic.rate.graph("NMCUSD TK-3",2022 , "October 28, 2022", 2)




# chronic.rate.graph("Alisal", "October 27, 2023",3)


chronic.rate.graph.sub("Alisal", 2023 ,"October 27, 2023",3, "All")

chronic.rate.graph.sub("Alisal", 2023 ,"October 27, 2023",3, "SWD")


chronic.rate.graph.sub("Alisal", 2022 ,"October 28, 2022",3, "All")

chronic.rate.graph.sub("Alisal", 2022 ,"October 28, 2022",3, "SWD")



# chronic.rate.graph("SCESD", "October 27, 2023", 3)


chronic.rate.graph.sub("SCESD", 2023, "October 27, 2023",3, "All")

chronic.rate.graph.sub("SCESD", 2023, "October 27, 2023",3, "SWD")


chronic.rate.graph.sub("SCESD", 2022, "October 28, 2022",3, "All")

chronic.rate.graph.sub("SCESD", 2022, "October 28, 2022",3, "SWD")



# soledad


chronic.rate.graph.sub("Soledad", 2023, "October 27, 2023",2, "All")

chronic.rate.graph.sub("Soledad", 2023, "October 27, 2023",2, "SWD")

chronic.rate.graph.sub("Soledad", 2022, "October 28, 2022",2, "All")

chronic.rate.graph.sub("Soledad", 2022, "October 28, 2022",2, "SWD")
