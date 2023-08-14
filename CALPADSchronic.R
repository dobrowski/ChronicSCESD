
# Uses the CALPADS EOY3 files to calculate Chronic Absenteeism rates for student groups

### Load libraries -------

library(tidyverse)
library(readxl)
library(here)
library(googlesheets4)


sheet <- "https://docs.google.com/spreadsheets/d/1E7x2W-bWkZGenZTPVmlyGSl0LQPfrHc2s8ZLILKUOGw/edit#gid=0"




ausd.calpads <- read_xlsx(here("data", "alisal" , "14.2_StudentAbsencesStudentList.xlsx"))
ausd.calpads.demo <- read_xlsx(here("data", "alisal" , "8.1_StudentProfileList(EOY3).xlsx"))



calpads.join <- function(df, df.demo) {
    
    
    df.calpads2 <- df %>%
        group_by(SSID, StudentName, Gender, Grade ) %>% #, Ethnicity, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        summarise(across(.cols =   c(DaysExpectedA:DaysAbsentCEFG),
                         ~ sum(.x, na.rm = TRUE)
        )
        ) %>%
        mutate(AbsenceRate2 = 100*DaysAbsentCEFG/DaysExpectedA,
               chronic = if_else(AbsenceRate2 >= 10, TRUE, FALSE),
               dupes = duplicated(SSID))
    

    df.calpads.demo2 <- df.demo %>%
        select(SSID, EthnicityRace, Homeless, StudentswithDisabilities, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        distinct() %>%
        group_by(SSID) %>%
        mutate(Homeless = if_else(any(Homeless == "Y"), "Y", "N" ),
               StudentswithDisabilities = if_else(any(StudentswithDisabilities == "Y"), "Y", "N" ),
               EnglishLearner = if_else(any(EnglishLearner == "Y"), "Y", "N" ),
               SocioEconomicallyDisadvantaged = if_else(any(SocioEconomicallyDisadvantaged == "Y"), "Y", "N" ),
        ) %>%
        distinct() # %>%
    #   mutate(dupes = duplicated(SSID)) 

    df.calpads2 %>%
        left_join(df.calpads.demo2)
    
    
}

chronic.group.rate <- function(df, studentgroup) {

    ddff <-     deparse(substitute(df)) 
    studentsss <-     deparse(substitute(studentgroup))
    
    holder <- df %>%
        group_by({{studentgroup}}) %>%
        transmute(count = n(),
                  perc.chronic = 100*mean(chronic)) %>%
        distinct()%>%
        mutate(district = ddff,
               students = studentsss
        )
    
    sheet_append(ss = sheet,
                 sheet = "Chronic Group",
                 data = holder )
    holder

    
}



ausd.calpads.joint <- calpads.join(ausd.calpads, ausd.calpads.demo)

chronic.group.rate(ausd.calpads.joint, EthnicityRace)
chronic.group.rate(ausd.calpads.joint, Homeless)
chronic.group.rate(ausd.calpads.joint, StudentswithDisabilities)
chronic.group.rate(ausd.calpads.joint, EnglishLearner)
chronic.group.rate(ausd.calpads.joint, SocioEconomicallyDisadvantaged)

