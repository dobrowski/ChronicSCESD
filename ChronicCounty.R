



chronic.all <- tbl(con,"CHRONIC") %>%
    filter(academic_year == max(academic_year) ,
           county_code == "27",
     #      charter_school == "All",
           #            district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate)



absent.all <- tbl(con,"ABSENT") %>%
    filter(academic_year == max(academic_year) ,
           county_code == "27",
 #          charter_school == "All",
 #          dass == "No",
           #       district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("ABSENT","reporting_category") 


    # Note the double arrow head changes the global dataframe not just in the function
    
    chronic <<- chronic.all %>%
        filter(is.na(district_name),
               !is.na(definition))
    
    absent <<- absent.all %>%
        filter(is.na(district_name),
               !is.na(definition))
    




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


chronic.student.group <- function(df, dist, yr = "2022-23") {
    
    
    df %>%
        filter(
#            aggregate_level == "D",
               !is.na(rate)) %>%
        mutate(subgroup.n = paste0(definition," (",chronic_absenteeism_eligible_cumulative_enrollment,")" )) %>%
        
        chronic_graph_percent(subgroup.n, 
                              indi = rate/100 ,
                              tit = "Percent Chronically Absent by Student Group",
                              subtit = paste0(dist, " - ", yr)) 
    
    
    ggsave(here("output", paste0(dist, " - District Level - Chronic by Student Group.png") ), width = 8, height = 4.5 )
    
}



### Using Average Days Absent from Absent Reason table -----





absent.student.group <- function(df, dist, yr = "2022-23") {
    
    df %>%
        filter(!is.na(average_days_absent),
 #              aggregate_level == "D"
               ) %>%
        mutate(subgroup.n = paste0(definition," (",eligible_cumulative_enrollment,")" )) %>%
        
        chronic_graph(subgroup.n,
                      indi = average_days_absent,
                      tit = "Average Days Absent by Student Group for the School Year",
                      subtit = paste0(dist, " - ", yr))
    
    ggsave(here("output", paste0(dist, " - District Level - Absent by Student Group.png") ), width = 8, height = 4.5  )
    
}



chronic.all %>%
    filter(aggregate_level == "C",
           dass == "All",
           charter_school == "All",
           !str_detect(definition,"eport")
           ) %>%
    chronic.student.group("Monterey County")

absent.all %>%
    filter(aggregate_level == "C",
           dass == "All",
           charter_school == "All",
           !str_detect(definition,"eport")
           
    ) %>%
    absent.student.group("Monterey County")


### For all Districts Graph creation -------

districts <- chronic.all$district_name %>% unique()

for (i in districts) {
    
    
    
    chronic.all %>%
        filter(
            str_detect(district_name,i) ,
                aggregate_level == "D",
            charter_school == "No",
               dass == "All",
               !str_detect(definition,"eport")
        ) %>%
        chronic.student.group(i)
    
    absent.all %>%
        filter(
            str_detect(district_name,i) ,
            aggregate_level == "D",
            charter_school == "No",
            dass == "All",
            !str_detect(definition,"eport")
               
        ) %>%
        absent.student.group(i)
    
}




### For all Charter Graph creation -------

charters <- chronic.all %>%
    filter(charter_school == "Yes") %>%
    select(school_name) %>%
    unique() %>% 
    unlist()

for (i in charters) {
    
    
    
    chronic.all %>%
        filter(
            str_detect(school_name,i) ,
            aggregate_level == "S",
            charter_school == "Yes",
            # dass == "All",
            # !str_detect(definition,"eport")
        ) %>%
        chronic.student.group(i)
    
    absent.all %>%
        filter(
            str_detect(school_name,i) ,
            aggregate_level == "S",
            charter_school == "Yes",
            # dass == "All",
            # !str_detect(definition,"eport")
            
        ) %>%
        absent.student.group(i)
    
}



### Monterey among CA Counties ----


chronic.counties <- tbl(con,"CHRONIC") %>%
    filter(academic_year >= "2021-22" ,
           aggregate_level %in% c( "C", "T"),
           charter_school == "All",
           dass == "All",
           reporting_category == "TA"
           #            district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate)


chronic.counties <- chronic.counties %>%
    mutate(county_name = if_else(str_detect(county_name,"State"), "California", county_name ),
        kular = if_else(str_detect(county_name,"Monterey"), "#DDA63A", "#6C9BA6"),
county_kular = paste0("<span style=\"color: ", kular, "\">", county_name, "</span>")
) 


chronic.counties %>%
    filter(academic_year == max(academic_year)) %>%
ggplot(       aes( y = rate/100, x =fct_reorder(county_kular, rate/100) ,  label = percent( rate/100, accuracy = .1) )) +
    geom_segment( aes(x=fct_reorder(county_kular,rate/100), xend=fct_reorder(county_kular, rate/100), y=0, yend=rate/100,
                  color=kular),
                  size =1 ) +
    geom_point( aes(color=kular), size=3, alpha=0.6) +
    coord_flip() +
    geom_text(size = 1.5, color = "black") +
    scale_color_identity()+
    theme_hc() +
    mcoe_theme +
    theme(axis.text.y = element_markdown(size = 5)
    ) +
    labs(x = "",
         y = "",
         color ="",
         title = "Monterey County has the 15th lowest Chronic Absenteeism Rate for 2022-23", 
         subtitle = "Based on all California Counties including charter and alternative schools",
         caption = "Source: https://www.cde.ca.gov/ds/ad/filesabd.asp") +
    scale_y_continuous(
        labels = label_percent(),
        expand = expansion(c(0.1, 0.1))
    )


ggsave(here("output",paste0("Monterey Among the Counties  " ,Sys.Date(),".png")), width = 8, height = 4.5)  



chronic.counties.change <- chronic.counties %>%
    select(county_kular, kular ,academic_year, rate) %>%
    pivot_wider(names_from = academic_year,
                values_from = rate) %>%
    mutate(change = `2021-22` - `2022-23`)   
    
    

ggplot2::ggplot(chronic.counties.change, aes( y = change,
                                     x =forcats::fct_reorder(county_kular,change) ,
                                     label = round2(change,1))
) +
    geom_segment( aes(x=forcats::fct_reorder(county_kular, change),
                      xend=forcats::fct_reorder(county_kular, change),
                      y=0,
                      yend=change,
                      color=kular),
                  size =2 ) +
    geom_point( aes(color=kular), size=5, alpha=0.6) +
    coord_flip() +
    geom_text(size = 3, color = "black") +
    scale_color_identity()+
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
    theme_hc() +
    mcoe_theme +
    theme(axis.text.y = element_markdown()
    ) +
    labs(title = "RPP Districts among the most improved on Chronic Absenteeism",
         subtitle = "Number of percentage points decreased from 2021-22 to 2022-23",
         source = "Source: DataQuest Research Files, https://www.cde.ca.gov/ds/ad/filesabd.asp, excluding charters")



ggsave(here("output",paste0("RPP Change  " ,Sys.Date(),".png")), width = 8, height = 4.5)    




### Reason by school ----



absence.reason.group <- function(df, dist, yr) {
    
    
df %>%
  #      filter(aggregate_level == "D") %>%
        mutate(sorting = unexcused_absences_percent) %>%
        pivot_longer(ends_with("percent")) %>%
        mutate(name = recode(name,
                             "unexcused_absences_percent" = "Unexcused Absences",
                             "excused_absences_percent" = "Excused Absences",
                             "incomplete_independent_study_absences_percent" = "Incomplete Independent Study",
                             "out_of_school_suspension_absences_percent" = "Out of School Suspensions"
        )) %>%
        mutate(nudger = case_when(name == "Unexcused Absences" ~ 0,
                                  name == "Excused Absences" ~ 0,
                                  name == "Incomplete Independent Study" ~ 5,
                                  name == "Out of School Suspensions" ~ -5)) %>%
        filter(value > 0) %>%
        ggplot(aes(x = fct_reorder(definition, sorting), y = value/100, fill = name, label = percent(value/100, accuracy = 0.1))) +
        geom_col() +
        geom_label_repel(aes(fill = name),
                         position=position_stack(vjust=0.5),
                         show.legend = FALSE,
                         direction = "x",
              #           nudge_x = nudger
                         # force = 0.01,
                         # force_pull = 10
        ) +
        mcoe_theme +
        coord_flip() + 
        labs(title = "Reasons for Absences by Student Group",
             subtitle = paste0(dist, " - ",yr) ,
             caption = "Source: https://www.cde.ca.gov/ds/ad/filesabr.asp") +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        ) +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
    
    
    ggsave(here("output", paste0(dist, " - District Level - Absence Reason by Group.png") ), width = 9, height = 5  )
    
}  


absent %>% 
    filter(!str_detect(definition,"Grade|Kind|Did|Islander")) %>%
absence.reason.group( "Monterey County")



### Countywide by District ----


chronic.all %>%
    filter(
                    aggregate_level == "D",
                    reporting_category == "TA",
                    dass == "All",
                    charter_school == "No",
                    !str_detect(definition,"eport")
                    
  #      !is.na(rate)
        ) %>%
    chronic_graph_percent(district_name, 
                          indi = rate/100 ,
                          tit = "Percent Chronically Absent by District",
                          subtit = paste0("Monterey County", " - 2022-23")) 


ggsave(here("output", paste0("Monterey County - Chronic by District.png") ), width = 8, height = 4.5 )




absent.all %>%
    filter(
        aggregate_level == "D",
        reporting_category == "TA",
        dass == "All",
        charter_school == "No",
        !str_detect(definition,"eport")
        
 #       !is.na(average_days_absent)
        ) %>%
    chronic_graph(district_name,
                  indi = average_days_absent,
                  tit = "Average Days Absent by District for the School Year",
                  subtit = paste0("Monterey County", " - 2022-23"))
 


ggsave(here("output", paste0("Monterey County - Absent by District.png") ), width = 8, height = 4.5 )



### All Schools in a District -----


schools.in.dist <- function(dist, yr = "2022-23") {
    

chronic.all %>%
    filter(
        str_detect(district_name, dist),
        aggregate_level == "S",
        reporting_category == "TA",
        charter_school == "No",
  #      dass == "All",
   #     !str_detect(definition,"eport")
        
              !is.na(rate)
    ) %>%
    chronic_graph_percent(school_name, 
                          indi = rate/100 ,
                          tit = "Percent Chronically Absent by School",
                          subtit = paste0(dist, " - ",yr)) 


ggsave(here("output", paste0(dist, " - Chronic by School.png") ), width = 8, height = 4.5 )


absent.all %>%
    filter(
        str_detect(district_name, dist),
        aggregate_level == "S",
        
        reporting_category == "TA",
        charter_school == "No",
   #     dass == "All",
   #     !str_detect(definition,"eport")
        
               !is.na(average_days_absent)
    ) %>%
    chronic_graph(school_name,
                  indi = average_days_absent,
                  tit = "Average Days Absent by School for the School Year",
                  subtit = paste0(dist, " - ", yr))



ggsave(here("output", paste0(dist," - Absent by School.png") ), width = 8, height = 4.5 )

}


schools.in.dist("Carmel")


for (i in districts) {
    
    schools.in.dist(i)
}



### YTD with select grades ------

# Using the invidual records from 


graph2023.tk %>%
    filter(district_name == "Soledad",
           school_name ==  "District Office" 
           ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = definition,
                          tit = paste0("Soledad" ," Chronic Absenteeism"), 
                          subtit = paste0("TK - K" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")



ggsave(here("output", paste0("Soledad - TK-K - YTD.png") ), width = 9, height = 6 )

graph2023.tk %>%
    filter(district_name == "North Monterey",
           school_name ==  "District Office" 
    ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = definition,
                          tit = paste0("North Monterey" ," Chronic Absenteeism"), 
                          subtit = paste0("TK - 2" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")


ggsave(here("output", paste0("North Monterey - TK-2 - YTD.png") ), width = 9, height = 6 )



graph2023.tk %>%
    filter(district_name == "Alisal Union",
           school_name ==  "District Office" 
    ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = definition,
                          tit = paste0("Alisal" ," Chronic Absenteeism"), 
                          subtit = paste0("TK - K" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")


ggsave(here("output", paste0("Alisal - TK-K - YTD.png") ), width = 9, height = 6 )


graph2023.tk %>%
    filter(district_name == "Salinas City",
           school_name ==  "District Office" 
    ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = definition,
                          tit = paste0("Salinas City" ," Chronic Absenteeism"), 
                          subtit = paste0("TK - K" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")


ggsave(here("output", paste0("Salinas City - TK-K - YTD.png") ), width = 9, height = 6 )


soledad.2023 %>%
    filter(#district_name == "Salinas City",
           school_name ==  "District Office" 
    ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = definition,
                          tit = paste0("Soledad" ," Chronic Absenteeism"), 
                          subtit = paste0("7th-8th Grade" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")


ggsave(here("output", paste0("Soledad - 7th-8th grade - YTD.png") ), width = 9, height = 6 )

### SWD by SChool -------



graph2023.swd %>%
    filter(district_name == "Alisal Union",
            definition == "SWD"
    ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = school_name,
                          tit = paste0("Alisal" ," Chronic Absenteeism"), 
                          subtit = paste0("SWD for All Grades" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")


ggsave(here("output", paste0("Alisal - SWD for All Grades - YTD.png") ), width = 9, height = 6 )



graph2023.swd %>%
    filter(district_name == "Soledad",
           definition == "SPED"
    ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = school_name,
                          tit = paste0("Soledad" ," Chronic Absenteeism"), 
                          subtit = paste0("SPED for TK, K, 7th, and 9th Grades" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")


ggsave(here("output", paste0("Soledad - SPED for TK, K, 7th, and 9th grades - YTD.png") ), width = 9, height = 6 )



graph2023.swd %>%
    filter(district_name == "Salinas City",
           definition == "SPED"
    ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = school_name,
                          tit = paste0("Salinas City" ," Chronic Absenteeism"), 
                          subtit = paste0("SPED for All Grades" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")


ggsave(here("output", paste0("Salinas City - SPED for all grades - YTD.png") ), width = 9, height = 6 )



graph2023.swd %>%
    filter(district_name == "North Monterey",
           definition == "SWD"
    ) %>%
    chronic_graph_percent(indi = chronic.rate  ,
                          xxx = school_name,
                          tit = paste0("North Monterey" ," Chronic Absenteeism"), 
                          subtit = paste0("SWD for TK through 2nd Grade" ," 2022-23 ")) +
    labs( caption = "Source: Data exported from SIS")


ggsave(here("output", paste0("North Monterey - SWD for TK-2 - YTD.png") ), width = 9, height = 6 )
