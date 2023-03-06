


library(tidyverse)
library(janitor)
library(here)
library(scales)
library(ggthemes)
library(MCOE)
library(ggrepel)


con <- mcoe_sql_con()

### Collect Data ----

chronic.all <- tbl(con,"CHRONIC") %>%
     filter(academic_year == max(academic_year) ,
            county_code == "27",
            charter_yn == "No",
#            district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate,
           definition = paste0(definition," (", chronic_absenteeism_eligible_cumulative_enrollment , ")"))


write_rds(chronic.all, here("Chronic", "chronic-all.rds") )


absent.all <- tbl(con,"ABSENT") %>%
    filter(academic_year == max(academic_year) ,
           county_code == "27",
           charter_school == "No",
           dass == "No",
    #       district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("ABSENT","reporting_category") %>%
    mutate(definition = paste0(definition," (", eligible_cumulative_enrollment , ")"))



write_rds(absent.all, here("Chronic", "absent-all.rds") )

chronic_comp <- tbl(con,"CHRONIC") %>%
    filter(academic_year == max(academic_year) ,
           county_code == "27",
           charter_yn == "No",
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate)



df.by.lea <- function(dist) {

    # Note the double arrow head changes the global dataframe not just in the function
    
chronic <<- chronic.all %>%
    filter(str_detect(district_name, dist))

absent <<- absent.all %>%
    filter(str_detect(district_name, dist))

}

df.by.lea("Alisal")




chronic_graph <- function(df, indi = rate  , xxx, tit, subtit) {
    
        df  %>%
        
        ggplot( aes( y = {{indi}}, x =fct_reorder({{xxx}}, {{indi}}) ,  label = round2( {{indi}}, 2) )) +
        geom_segment( aes(x=fct_reorder({{xxx}}, {{indi}}), xend=fct_reorder({{xxx}}, {{indi}}), y=0, yend={{indi}}),
                      color="orange",
                      size =2 ) +
        geom_point( color="orange", size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black") +
        theme_hc() +
        mcoe_theme +
        labs(x = "",
             y = "",
             color ="",
             title = tit, 
             subtitle = subtit,
             caption = "Source: https://www.cde.ca.gov/ds/ad/filesabd.asp")
    
}


chronic_graph_percent <- function(df, indi = rate  , xxx, tit, subtit) {
    
    df  %>%
        
        ggplot( aes( y = {{indi}}, x =fct_reorder({{xxx}}, {{indi}}) ,  label = percent( {{indi}}, accuracy = .1) )) +
        geom_segment( aes(x=fct_reorder({{xxx}}, {{indi}}), xend=fct_reorder({{xxx}}, {{indi}}), y=0, yend={{indi}}),
                      color="orange",
                      size =2 ) +
        geom_point( color="orange", size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black") +
        theme_hc() +
        mcoe_theme +
        labs(x = "",
             y = "",
             color ="",
             title = tit, 
             subtitle = subtit,
             caption = "Source: https://www.cde.ca.gov/ds/ad/filesabd.asp") +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        )
    
}

### Chronic Absenteeism Graphs ----


chronic.student.group <- function(df, dist) {
    

df %>%
    filter(aggregate_level == "D",
           !is.na(rate)) %>%
    chronic_graph_percent(definition, 
                  indi = rate/100 ,
                  tit = "Percent Chronically Absent by Student Group",
                  subtit = paste0(dist, " - 2021-22")) 


ggsave(here("output", paste0(dist, " - District Level - Chronic by Student Group.png") ) )

}


chronic.student.group(chronic, "SCESD")


chronic.by.school <- function(df, dist) {
    
    df %>%
    filter(reporting_category == "TA",
           !is.na(rate)) %>%
    mutate(school_name = if_else(is.na(school_name), "District Overall", school_name)) %>%
        chronic_graph_percent(school_name,
                  indi = rate/100 ,
                  tit = "Percent Chronically Absent by School",
                  subtit = paste0(dist, " - 2021-22"))


    ggsave(here("output", paste0(dist, " - District Level - Chronic by School.png") ) )

}    


chronic.by.school(chronic, "SCESD")


### Using Average Days Absent from Absent REason table -----





absent.student.group <- function(df, dist) {
    
    df %>%
    filter(!is.na(average_days_absent),
           aggregate_level == "D") %>%
    chronic_graph(definition,
                  indi = average_days_absent,
                  tit = "Average Days Absent",
                  subtit = paste0(dist, " - 2021-22"))
    
    ggsave(here("output", paste0(dist, " - District Level - Absent by Student Group.png") ) )
    
}


absent.student.group(absent, "SCESD")



absent.by.school <- function(df, dist) {
    
    df %>%
    filter(!is.na(average_days_absent),
           aggregate_level == "S",
           reporting_category == "TA") %>%
    chronic_graph(school_name,
                  indi = average_days_absent,
                  tit = "Average Days Absent",
                  subtit = paste0(dist, " - 2021-22"))
    
    ggsave(here("output", paste0(dist, " - District Level - Absent by School.png") ) )
    
}


absent.by.school(absent, "SCESD")




### Reason by school ----

absent %>%
    filter(aggregate_level == "D") %>%
    pivot_longer(ends_with("percent")) %>%
    ggplot(aes(x = definition, y = value, fill = name)) +
    geom_col() +
    mcoe_theme +
    coord_flip()
    


absence.reason.group <- function(df, dist) {
    
    
    df %>%
        filter(aggregate_level == "D") %>%
        mutate(sorting = unexcused_absences_percent) %>%
        pivot_longer(ends_with("percent")) %>%
        mutate(name = recode(name,
                             "unexcused_absences_percent" = "Unexcused Absences",
                             "excused_absences_percent" = "Excused Absences",
                             "incomplete_independent_study_absences_percent" = "Incomplete Independent Study",
                             "out_of_school_suspension_absences_percent" = "Out of School Suspensions"
        )) %>%
        filter(value > 0) %>%
        ggplot(aes(x = fct_reorder(definition, sorting), y = value/100, fill = name, label = percent(value/100, accuracy = 0.1))) +
        geom_col() +
        geom_label_repel(aes(fill = name),
                   position=position_stack(vjust=0.5),
                   show.legend = FALSE,
                   direction = "x"
        ) +
        mcoe_theme +
        coord_flip() + 
        labs(title = "Reasons for Absences by Student Group",
             subtitle = paste0(dist, " - 2021-22") ) +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        ) +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
    
    
    ggsave(here("output", paste0(dist, " - District Level - Absence Reason by Group.png") ), width = 15, height = 10 )
    
}  



absence.reason.group(absent, "North Monterey")



absence.reason.school <- function(df, dist) {
    

df %>%
    filter(aggregate_level == "S",
           reporting_category == "TA") %>%
        mutate(sorting = unexcused_absences_percent) %>%
    pivot_longer(ends_with("percent")) %>%
    mutate(name = recode(name,
                         "unexcused_absences_percent" = "Unexcused Absences",
                         "excused_absences_percent" = "Excused Absences",
                         "incomplete_independent_study_absences_percent" = "Incomplete Independent Study",
                         "out_of_school_suspension_absences_percent" = "Out of School Suspensions"
    )) %>%
        filter(value > 0) %>%
    ggplot(aes(x = fct_reorder(school_name, sorting), y = value/100, fill = name, label = percent(value/100))) +
    geom_col() +
    geom_label_repel(aes(fill = name),
                     position=position_stack(vjust=0.5),
                     show.legend = FALSE,
                     direction = "x"
                     ) +
    mcoe_theme +
    coord_flip() + 
    labs(title = "Reasons for Absences by School",
         subtitle = paste0(dist, " - 2021-22") ) +
    scale_y_continuous(
        labels = label_percent(),
        expand = expansion(c(0.1, 0.1))
    ) +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))

    
    ggsave(here("output", paste0(dist, " - District Level - Absence Reason by School.png") ), width = 10, height = 6 )
    
}   

absence.reason.school(absent, "North Monterey")



### Compare to Monterey County ----



county.compare <- function(df, dist) {
    df %>%
    mutate(reporting_group = case_when( str_sub(reporting_category,1,2) == "GR" ~ "Grade Span",
                                        str_sub(reporting_category,1,1) == "R" ~ "Race/Ethnicity",
                                        str_sub(reporting_category,1,1) == "G" ~ "Gender",
                                        str_sub(reporting_category,1,1) == "S" ~ "Student Group",
                                        TRUE ~ "All Students"
        )) %>%
    filter(aggregate_level %in% c("D","C"),
           !is.na(rate),
           definition %notin% c(NA, "Not Reported", "Grades 9–12" , "Grades 7–8")) %>%
    mutate(district_name = ifelse(is.na(district_name), "Monterey County" ,district_name )) %>%
    filter( str_detect(district_name, dist) | aggregate_level == "C")  %>%
   
    ggplot( aes( y = rate/100,
                 x =fct_reorder(definition, rate) ,
                 fill = district_name,
                 label = percent( rate/100, accuracy = 0.1) )) +
    geom_col(position = "dodge") +
    #   geom_point( aes(color = {{geo}}), size=5, alpha=0.6) +
    coord_flip() +
    geom_text(size = 3, color = "black", position=position_dodge(width=0.9)) +
    theme_hc() +
    mcoe_theme +
    labs(x = "",
         y = "",
         color ="",
         title = "Percent Chronically Absent by Student Group Compared with Monterey County", 
         subtitle = paste0(dist, " - 2021-22"),
         caption = "Source: https://www.cde.ca.gov/ds/ad/filesabd.asp") +
    facet_wrap(~ reporting_group, scales = "free") +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        )


ggsave(here("output", paste0(dist, " - County Compare.png") ), width = 15, height = 10 )

}


county.compare(chronic_comp, "Salinas City")




### All the graphs ----

all.graphs <- function(dist) {
   
df.by.lea(dist)

chronic.student.group(chronic, dist)

chronic.by.school(chronic, dist)

absent.student.group(absent, dist)

absent.by.school(absent, dist)

absence.reason.school(absent, dist)

absence.reason.group(absent, dist)

county.compare(chronic_comp, dist)

}

all.graphs("Alisal Union")

all.graphs("Salinas City")

all.graphs("North Monterey")

all.graphs("Soledad")


### End -----
