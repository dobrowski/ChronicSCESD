
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
   #        rtype == "D",
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




### School Graphs ---


joint.school <- function(df, df.demo, dist.name) {
    

    df.calpads2 <- df %>%
        filter(DaysExpectedA >= 1,
               Grade %in% c("KN",1,2,3,4,5,6,7,8,  "01","02","03","04","05","06","07","08"   )
        ) %>%
        group_by(SSID, StudentName, SchoolName, SchoolCode) %>% #, Ethnicity, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        summarise(across(.cols =   c(DaysExpectedA:DaysAbsentCEFG),
                         ~ sum(.x, na.rm = TRUE)
        )
        ) %>%
        filter(DaysExpectedA >= 31) %>%
        mutate(AbsenceRate2 = 100*DaysAbsentCEFG/DaysExpectedA,
               chronic = if_else(AbsenceRate2 >= 10, TRUE, FALSE),
               dupes = duplicated(SSID))
    
    
    df.calpads.demo2 <- df.demo %>%
        select(SSID, EthnicityRace, Homeless, StudentswithDisabilities, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        distinct() %>%
        group_by(SSID) %>%
        mutate(Homeless = if_else(any(Homeless == "Y"), "Yes", "N" ),
               StudentswithDisabilities = if_else(any(StudentswithDisabilities == "Y"), "Yes", "N" ),
               EnglishLearner = if_else(any(EnglishLearner == "Y"), "Yes", "N" ),
               SocioEconomicallyDisadvantaged = if_else(any(SocioEconomicallyDisadvantaged == "Y"), "Yes", "N" ),
        ) %>%
        distinct()  %>%
        mutate(All = "Yes") %>%
        pivot_wider(names_from = EthnicityRace, values_from = All) %>%
        mutate(All = "Yes")
    
joint <- df.calpads2 %>%
        left_join(df.calpads.demo2)
    
joint


}


nmcusd.calpads.school.joint <- joint.school(nmcusd.calpads, nmcusd.calpads.demo)


car.school <- function(df,students) {
    
    ddff <-     deparse(substitute(df)) 
    studentsss <-     deparse(substitute(students))
    
    holder <-  df %>% 
        ungroup() %>%
        filter({{students}} == "Yes")  %>%
        
        mutate( # dist.standard = ScaleScore - MeetStandard,
            chronic.rate = 100*mean(chronic),
            count = n())  %>%
        select(chronic.rate, count) %>%
        distinct() %>%
        mutate(district = ddff,
               students = studentsss
        )
    
    # sheet_append(ss = sheet,
    #              sheet = "Distance from Standard Group",
    #              data = holder )
    holder
    
}


add.school.car <- function(df) {
    
    namer <- unique(df$SchoolName)
    coder <- unique(df$SchoolCode)
    
    waiting.room <- car.school(df,All) %>%
        bind_rows(  car.school(df,White) ) %>%
        bind_rows(  car.school(df,EnglishLearner) ) %>%
        bind_rows( car.school(df,Asian) )  %>%
        bind_rows( car.school(df,Filipino) )  %>%
        bind_rows( car.school(df,Multiple) )  %>%
        bind_rows( car.school(df,`Black/African Am`) )  %>%
        bind_rows( car.school(df,`Am Indian/Alskn Nat`) )  %>%
        bind_rows( car.school(df,`Nat Hwiin/Othr Pac Islndr`) )  %>%
        bind_rows( car.school(df,Hispanic) )  %>%
        bind_rows( car.school(df,StudentswithDisabilities) )  %>%
        bind_rows( car.school(df,SocioEconomicallyDisadvantaged) )  %>%
        bind_rows( car.school(df,Homeless) ) %>%
        mutate(SchoolName = namer,
               SchoolCode = coder
        )
    
    waiting.room
    
    
}

nmcusd.calpads.school.joint %>% 
    filter(str_detect(SchoolName,"Echo Valley")) %>%
    add.school.car()


holder <- ausd.calpads.school.joint %>%
    # filter(str_detect(DistrictName,dist.name)) %>%
    split(.$SchoolName) %>%
    map_df(~add.school.car(.))  %>%
     mutate(Group = case_match(students,
                               "StudentswithDisabilities" ~ "Students with \nDisabilities",
                               "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                               "Hispanic" ~ "Latino",
                               "EnglishLearner" ~ "English Learner",
                               .default = students
     ))


dash.school.chr <- function(cdsCode) {
    
    tbl(con,"DASH_ALL_2022") %>%
        filter(countyname == "Monterey",
               cds == cdsCode,
               rtype == "S",
               indicator == "chronic") %>%
        collect()  %>%
        mutate(Group = case_match(studentgroup,
                                  "HOM" ~ "Homeless",
                                  "SWD" ~ "Students with \nDisabilities",
                                  "SED" ~ "Socio-Economically \nDisadvantaged",
                                  "HI" ~ "Latino",
                                  "EL" ~ "English Learner",
                                  "AS" ~ "Asian",
                                  "FI" ~ "Filipino",
                                  "WH" ~ "White",
                                  "ALL" ~ "All",
                                  .default = studentgroup
        ))
    
}

chron.comp.school <- function(df, dist.code, school.code, limit.case.count = TRUE ) {
    
    cds <- paste0("27",dist.code, str_pad(school.code, 7, side="left", pad="0"))

    # ent <- tbl(con,"SCHOOLS") %>%
    #     filter(
    #         CDSCode == cds
    #     ) %>%
    #     collect()  %>%
    #     select(EILName) %>%
    #     simplify()
    # 
    # print(ent)
    # 
    # 
    
    work.group <-   df %>%
        filter(SchoolCode == school.code #| SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
        ungroup() %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    print(work.group)

    dash2 <- dash.school.chr( cds ) %>%
        filter(# str_detect(districtname, dist.name),
            Group %in% work.group
        ) %>%
        select(districtname, schoolname ,indicator, currstatus, Group) %>%
        mutate(EstimatedColor = "Light Gray") %>%
        rename(chronic.rate = currstatus)
    
    print(dash2)
    
    
    df %>%
        filter(SchoolCode == school.code # | SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
  #      mutate(DFS = as.numeric(DFS)) %>%
        left_join(dash2, by = c("Group")) %>%
        mutate(change = chronic.rate.x - chronic.rate.y,
               EstimatedColor = case_when(
                   count < 30 ~ "White",
                   
                   # All Schools
                   chronic.rate.x >=20 & change > -0.5 ~ "Red",
                   chronic.rate.x >=20 & change <= -3.0 ~ "Yellow",
                   chronic.rate.x >=20 & change <= -0.5 ~ "Orange",
                   
                   chronic.rate.x >=10 & change >= 3.0 ~ "Red",
                   chronic.rate.x >=10 & change <= -0.5 ~ "Yellow", 
                   chronic.rate.x >=10 & change < 3.0 ~ "Orange", 
                   
                   chronic.rate.x >=5 & change > 0.5 ~ "Orange",
                   chronic.rate.x >=5 & change <= -0.5 ~ "Green",
                   chronic.rate.x >=5 & change < 0.5 ~ "Yellow",

                   chronic.rate.x >=2.5 & change >= 3.0 ~ "Orange",
                   chronic.rate.x >=2.5 & change <= -3.0 ~ "Blue",    
                   chronic.rate.x >=2.5 & change <= 0.5 ~ "Green",    
                   chronic.rate.x >=2.5 & change < 3.0 ~ "Yellow",    
                   
                   chronic.rate.x < 2.5 & change >= 3.0 ~ "Yellow",
                   chronic.rate.x < 2.5 & change <= 0.5 ~ "Blue",
                   chronic.rate.x < 2.5 & change  < 3.0 ~ "Green",
                   

                   
    #               !is.na(DFS.x) & is.na(DFS.y) ~ "Black",
                   
                   TRUE ~ EstimatedColor
                   
               ),
    chronic.rate = chronic.rate.x
        ) %>%
        
        
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
               EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) )
    
    
    
    
}



test <- chron.comp.school(holder, dist.code = 65961, school.code = 127456)



chron.comp.school.graph <- function(df) {
    
    skul <- df$schoolname[1]

    df %>%
        ggplot(aes(x = Group, y = chronic.rate)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Chronic Absenteeism Rate",
             title = paste0(skul, " Chronic Absenteeism Student Group Results 2023"),
             subtitle = "Gray is 2022 results and Colored bars are 2023 with the estimated Dashboard color")
    
    
    #    ggsave(here("output",paste0(dist.name, " - ",assessment," CAASPP Student Group Results 2022 and 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

chron.comp.school(holder, dist.code = 65961, school.code = 127456, limit.case.count = TRUE) %>%
    chron.comp.school.graph()




chron.all.schools <- function(df, dist.cd) {
    

holder <- df %>%
    # filter(str_detect(DistrictName,dist.name)) %>%
    split(.$SchoolName) %>%
    map_df(~add.school.car(.))  %>%
    mutate(Group = case_match(students,
                              "StudentswithDisabilities" ~ "Students with \nDisabilities",
                              "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                              "Hispanic" ~ "Latino",
                              "EnglishLearner" ~ "English Learner",
                              .default = students
    ))





school.list <- holder$SchoolCode %>% unique()


for (i in 1:length(school.list)) {
    
    chron.comp.school(df = holder, dist.code = dist.cd, school.code = school.list[i], limit.case.count = TRUE) %>% 
        chron.comp.school.graph()
    
    ggsave(here("output",paste0(school.list[i], " - ","Chronic Absenteeism Student Group Results 2022 and 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
}


}


nmcusd.calpads.school.joint <- joint.school(nmcusd.calpads, nmcusd.calpads.demo)
chron.all.schools(nmcusd.calpads.school.joint , dist.cd = 73825)

ausd.calpads.school.joint <- joint.school(ausd.calpads, ausd.calpads.demo)
chron.all.schools(ausd.calpads.school.joint , dist.cd = 65961)

soledad.calpads.school.joint <- joint.school(soledad.calpads, soledad.calpads.demo)
chron.all.schools(soledad.calpads.school.joint , dist.cd = 75440)

scesd.calpads.school.joint <- joint.school(scesd.calpads, scesd.calpads.demo)
chron.all.schools(scesd.calpads.school.joint , dist.cd = 66142)
