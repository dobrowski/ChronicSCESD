
# Looks at CALPADS Suspension data to give dashboard estimates 
# Need CALPADS 7.12 and 8.1 reports


sol.susp <- read_excel(here("data","soledad","7.12_IncidentResultsStudentList (1).xlsx"),
                             range = "C9:AD802")



susp.df <- function(suspenion.incidences, demographics) {
    
sol.susp.sum <- suspenion.incidences %>%
    group_by(SSID, `Student Name`) %>%
    summarise(days = sum(`Duration Days`)) %>%
    filter(days >= 0.5) 


sol.susp.demo <- demographics %>%
    mutate(SSID = as.character(SSID)) %>%
    select(SSID, StudentName, EthnicityRace, Homeless:SocioEconomicallyDisadvantaged) %>%
    distinct() %>%
    mutate(Yes = "Y") %>%
    pivot_wider(names_from = EthnicityRace, values_from = Yes, values_fill = "N")


sol.susp.final <- sol.susp.demo %>%
    left_join(sol.susp.sum) %>%
    mutate(susp = if_else(is.na(days), FALSE, TRUE))

} 


susp.group.rate <- function(df, studentgroup) {
    
    ddff <-     deparse(substitute(df)) 
    studentsss <-     deparse(substitute(studentgroup))
    
    holder <- df %>%
        group_by({{studentgroup}}) %>%
        transmute(count = n(),
                  perc.susp = 100*mean(susp)) %>%
        distinct()%>%
        mutate(district = ddff,
               students = studentsss
        )
    
    holder
}


sol.susp.final <- susp.df(sol.susp,soledad.calpads.demo )

susp.group.rate(sol.susp.final, Homeless)
susp.group.rate(sol.susp.final, Asian)
susp.group.rate(sol.susp.final, SocioEconomicallyDisadvantaged)
susp.group.rate(sol.susp.final, StudentswithDisabilities)
susp.group.rate(sol.susp.final, White)
susp.group.rate(sol.susp.final, EnglishLearner)
susp.group.rate(sol.susp.final, Filipino)
susp.group.rate(sol.susp.final, Hispanic)




