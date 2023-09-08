
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

nmcusd.calpads <- read_csv(here("data", "nmcusd" , "14.2_StudentAbsencesStudentList.csv"))
nmcusd.calpads.demo <- read_csv(here("data", "nmcusd" , "8.1_StudentProfileList(EOY3).csv"))

soledad.calpads <- read_csv(here("data", "soledad" , "14.2_StudentAbsencesStudentList.csv"))
soledad.calpads.demo <- read_csv(here("data", "soledad" , "8.1_StudentProfileList(EOY3).csv"))

spreckels.calpads <- read_xlsx(here("data", "spreckels" , "14.2_StudentAbsencesStudentList.xlsx"),
                               range = "G9:AE950") %>%
    janitor::clean_names("upper_camel") %>%
    rename(SSID = Ssid) %>%
    mutate(AdaGeneratingIndependentStudyDays = as.numeric(AdaGeneratingIndependentStudyDays))
spreckels.calpads.demo <- read_xlsx(here("data", "spreckels" , "8.1_StudentProfileList(EOY3).xlsx"),
                                    range = "G10:AT968")%>%
    janitor::clean_names("upper_camel") %>%
    rename(SSID = Ssid) %>%
    mutate(StudentswithDisabilities = StudentsWithDisabilities)


### Functions ----

calpads.join <- function(df, df.demo) {
    
    
    df.calpads2 <- df %>%
        filter(DaysExpectedA >= 1,
               Grade %in% c("KN",1,2,3,4,5,6,7,8,  "01","02","03","04","05","06","07","08"   )
               ) %>%
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



nmcusd.calpads.joint <- calpads.join(nmcusd.calpads, nmcusd.calpads.demo)

chronic.group.rate(nmcusd.calpads.joint, EthnicityRace)
chronic.group.rate(nmcusd.calpads.joint, Homeless)
chronic.group.rate(nmcusd.calpads.joint, StudentswithDisabilities)
chronic.group.rate(nmcusd.calpads.joint, EnglishLearner)
chronic.group.rate(nmcusd.calpads.joint, SocioEconomicallyDisadvantaged)


nmcusd.calpads.joint %>%
    mutate(All = "Y") %>%
    chronic.group.rate(All)


temp <- nmcusd.calpads %>%
    mutate(AbsenceRate2 = 100*DaysAbsentCEFG/DaysExpectedA,
           chronic = if_else(AbsenceRate2 >= 10, TRUE, FALSE),
           dupes = duplicated(SSID))

mean(temp$chronic, na.rm = TRUE)


soledad.calpads.joint <- calpads.join(soledad.calpads, soledad.calpads.demo)

chronic.group.rate(soledad.calpads.joint, EthnicityRace)
chronic.group.rate(soledad.calpads.joint, Homeless)
chronic.group.rate(soledad.calpads.joint, StudentswithDisabilities)
chronic.group.rate(soledad.calpads.joint, EnglishLearner)
chronic.group.rate(soledad.calpads.joint, SocioEconomicallyDisadvantaged)


spreckels.calpads.joint <- calpads.join(spreckels.calpads, spreckels.calpads.demo)

chronic.group.rate(spreckels.calpads.joint, EthnicityRace)
chronic.group.rate(spreckels.calpads.joint, Homeless)
chronic.group.rate(spreckels.calpads.joint, StudentswithDisabilities)
chronic.group.rate(spreckels.calpads.joint, EnglishLearner)
chronic.group.rate(spreckels.calpads.joint, SocioEconomicallyDisadvantaged)


### Graphing Single Year -----

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
    
    
    ggsave(here("output",paste0(dist.name, " - Chronically Absent Student Group Results 2023 ", Sys.Date(),".png")), width = 8, height = 5)    
    
}


chronic.dash.graph(dist = "ausd.calpads.joint",
          dist.name = "Alisal")

chronic.dash.graph(dist = "scesd.calpads.joint",
                   dist.name = "Salinas City")

chronic.dash.graph(dist = "nmcusd.calpads.joint",
                   dist.name = "North Monterey County")

chronic.dash.graph(dist = "soledad.calpads.joint",
                   dist.name = "Soledad")


chronic.dash.graph(dist = "spreckels.calpads.joint",
                   dist.name = "Spreckels")


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
           EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) # Puts gray to the left of color
           ) %>%
    ggplot(aes(x = Group, y = PercentChronicAbsent)) +
#    ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        mcoe_theme +
    {if(length(unique(df$Group)) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Chronically Absent",
             title = paste0(dist.name," - Chronically Absent Student Group Results 2023"),
             subtitle = "Gray is 2022 results and Colored bars are 2023 with the estimated Dashboard color")
    
    
    ggsave(here("output",paste0(dist.name," - Chronically Absent Student Group Results 2022 and 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)    
    
}



chronic.dash.comp(dist = "ausd.calpads.joint",
         dist.name = "Alisal")

chronic.dash.comp(dist = "scesd.calpads.joint",
                   dist.name = "Salinas City")

chronic.dash.comp(dist = "nmcusd.calpads.joint",
                  dist.name = "North Monterey County")

chronic.dash.comp(dist = "soledad.calpads.joint",
                   dist.name = "Soledad")


chronic.dash.comp(dist = "spreckels.calpads.joint",
    dist.name = "Spreckels")
