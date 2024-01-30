


library(tidyverse)
library(janitor)
library(here)
library(scales)
library(ggthemes)
library(MCOE)
library(ggrepel)
library(ggtext)


con <- mcoe_sql_con()



chronic <- tbl(con,"CHRONIC") %>%
     filter(academic_year == max(academic_year) ,
      #      county_code == "27",
            aggregate_level == "D",
      reporting_category == "TA",
      charter_school == "No",
      dass == "All"
      
        #    charter_yn == "No",
       #     district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate)


chronic_graph <- function(df, indi = rate  , xxx, tit, subtit = "Salinas City ESD - 2021-22") {
    
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


### Chronic Absenteeism Graphs ----

chronic %>%
    filter(aggregate_level == "D",
           !is.na(rate)) %>%
    chronic_graph(definition, 
                  indi = rate ,
                  "Percent Chronically Absentee by Student Group")

ggsave("SCESD - District Level.png")



chronic %>%
    filter(reporting_category == "TA",
           !is.na(rate)) %>%
    mutate(school_name = if_else(is.na(school_name), "District Overall", school_name)) %>%
    chronic_graph(school_name,
                  indi = rate ,
                  "Percent Chronically Absentee by School")


ggsave("SCESD By School.png")


### Using Average Days Absent from Absent REason table -----



absent <- tbl(con,"ABSENT") %>%
    filter(academic_year == max(academic_year) ,
           county_code == "27",
           charter_school == "No",
           dass == "No",
           district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("ABSENT","reporting_category") 


absent %>%
    filter(!is.na(average_days_absent),
           aggregate_level == "D") %>%
    chronic_graph(definition,
                  indi = average_days_absent,
                  "Average Days Absent")

ggsave("SCESD - Average Days Absent - District Level.png")



absent %>%
    filter(!is.na(average_days_absent),
           aggregate_level == "S",
           reporting_category == "TA") %>%
    chronic_graph(school_name,
                  indi = average_days_absent,
                  "Average Days Absent")

ggsave("SCESD - Average Days Absent - by School.png")




### Compare to Monterey County ----

chronic_graph_compare <- function(df, indi = rate  , geo, xxx, tit, subtit = "Salinas City ESD - 2021-22") {
    
    df  %>%
        
        ggplot( aes( y = {{indi}},
                     x =fct_reorder({{xxx}}, {{indi}}) ,
                     fill = {{geo}},
                     label = round2( {{indi}}, 2) )) +
        geom_col(position = "dodge") +
     #   geom_point( aes(color = {{geo}}), size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black", position=position_dodge(width=0.9)) +
        theme_hc() +
        mcoe_theme +
        labs(x = "",
             y = "",
             color ="",
             title = tit, 
             subtitle = subtit,
             caption = "Source: https://www.cde.ca.gov/ds/ad/filesabd.asp")
    
}




chronic_comp <- tbl(con,"CHRONIC") %>%
    filter(academic_year == max(academic_year) ,
           county_code == "27",
           charter_yn == "No",
      #     district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate)



chronic_comp %>%
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
    filter(district_name %in% c("Salinas City Elementary", "Monterey County")) %>%
        chronic_graph_compare(definition, 
                  indi = rate ,
                  geo = district_name,
                  "Percent Chronically Absentee by Student Group Compared with Monterey County") +
    facet_wrap(~ reporting_group, scales = "free")



ggsave("SCESD compare with County Chronic.png", width = 12, height = 6)






### End -----
