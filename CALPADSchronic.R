
# Uses the CALPADS EOY3 files to calculate Chronic Absenteeism rates for student groups

### Load libraries -------

library(tidyverse)
library(readxl)
library(here)
library(googlesheets4)
library(MCOE)


sheet <- "https://docs.google.com/spreadsheets/d/1E7x2W-bWkZGenZTPVmlyGSl0LQPfrHc2s8ZLILKUOGw/edit#gid=0"


### Load files -------

ausd.calpads <- read_xlsx(here("data", "alisal" , "14.2_StudentAbsencesStudentList.xlsx"))
ausd.calpads.demo <- read_xlsx(here("data", "alisal" , "8.1_StudentProfileList(EOY3).xlsx"))


scesd.calpads <- read_csv(here("data", "scesd" , "14.2_StudentAbsencesStudentList.csv"))
scesd.calpads.demo <- read_csv(here("data", "scesd" , "8.1_StudentProfileList(EOY3).csv"))



### Functions ----

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



scesd.calpads.joint <- calpads.join(scesd.calpads, scesd.calpads.demo)

chronic.group.rate(scesd.calpads.joint, EthnicityRace)
chronic.group.rate(scesd.calpads.joint, Homeless)
chronic.group.rate(scesd.calpads.joint, StudentswithDisabilities)
chronic.group.rate(scesd.calpads.joint, EnglishLearner)
chronic.group.rate(scesd.calpads.joint, SocioEconomicallyDisadvantaged)










# This file is to generate a bar graph to display distance from standard for groups. 



working <- read_sheet(ss = sheet,
                      sheet = "Chronic Group") %>%
    filter(StudentGroup != "N",
           StudentGroup != "Missing",
           NumberStudents >= 30) %>%
    mutate(Group = case_match(StudentGroupCategory,
                              "Homeless" ~ "Homeless",
                              "StudentswithDisabilities" ~ "Students with \nDisabilities",
                              "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                              "EnglishLearner" ~ "English Learner",
                              .default = StudentGroup
    ))


chronic.dash.graph <- function(dist, dist.name ) {
    
    
    working %>%
        filter(District == dist) %>%
 #       mutate(DFS = as.numeric(DFS)) %>%
        ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent, )) +
        geom_col(aes(fill = EstimatedColor, 
                     color = "black")) +
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Chronically Absent",
             title = paste0(dist.name," - Chronically Absent Student Group Results 2023"))
    
    
 #   ggsave(here("output",paste0(dist.name, " - Chronically Absent Student Group Results 2023 ", Sys.Date(),".png")), width = 8, height = 5)    
    
}


chronic.dash.graph(dist = "ausd.calpads.joint",
          dist.name = "Alisal")


chronic.dash.graph(dist = "scesd.calpads.joint",
                   dist.name = "Salinas City")

# Haven't updated below here yet ###



### Comparison to prior year ----


con <- mcoe_sql_con()


dash <- tbl(con,"DASH_ALL_2022") %>%
    filter(countyname == "Monterey",
           rtype == "D",
           indicator == "chronic" 
           ) %>%
    collect()  %>%
    mutate(Group = case_match(studentgroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HI" ~ "Hispanic",
                              "EL" ~ "English Learner",
                              "AS" ~ "Asian",
                              "FI" ~ "Filipino",
                              "WH" ~ "White",
                              "MR" ~ "Multiple",
                              "AA" ~ "Black/African Am",
                              "AI" ~ "Am Indian/Alskn Nat",
                              .default = studentgroup
    ))


chronic.dash.comp <- function(dist, assessment = "ELA", dist.name ) {
    
    
    work.group <-   working %>%
        filter(District == dist
               ) %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    dash2 <- dash %>%
        filter(str_detect(districtname, dist.name),
               Group %in% work.group
        ) %>%
        select(districtname, indicator, currstatus, Group) %>%
        mutate(EstimatedColor = "Light Gray") %>%
        rename(PercentChronicAbsent = currstatus)
    
    
    
df <-    working %>%
        filter(District == dist
               ) %>%
        mutate(PercentChronicAbsent = as.numeric(PercentChronicAbsent)) %>%
        bind_rows(dash2) %>%
        mutate(EstimatedColor = as_factor(EstimatedColor)) 
        
# Sorts only by the current year
 leveler <- df %>% 
     filter(EstimatedColor != "Light Gray") %>%
     arrange(PercentChronicAbsent) 

 levelss <- leveler$Group %>% union(working$Group %>% unique())


df %>%   
    mutate(Group = factor(Group, levels = levelss), # Sorts only by the current year
           EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ). # Puts gray to the left of color
           ) %>%
    ggplot(aes(x = Group, y = PercentChronicAbsent)) +
#    ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Chronically Absent",
             title = paste0(dist.name," - Chronically Absent Student Group Results 2023"),
             subtitle = "Gray is 2022 results and Colored bars are 2023 with the estimated Dashboard color")
    
    
#    ggsave(here("output",paste0(dist.name, " - ",assessment," CAASPP Student Group Results 2022 and 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)    
    
}



chronic.dash.comp(dist = "ausd.calpads.joint",
         dist.name = "Alisal")



chronic.dash.comp(dist = "scesd.calpads.joint",
                   dist.name = "Salinas City")
