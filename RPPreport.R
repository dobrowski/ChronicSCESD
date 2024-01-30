
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(here)
library(ggrepel)
library(MCOE)
library(ggtext)

chronic.dq <- vroom::vroom(here("data","chronicabsenteeism23.txt"), .name_repair = ~ janitor::make_clean_names(., case = "snake")) %>%
    mutate(district_code = as.character( district_code),
           chronic_absenteeism_count = as.numeric(chronic_absenteeism_count),
           chronic_absenteeism_rate = as.numeric(chronic_absenteeism_rate),
           chronic_absenteeism_eligible_cumulative_enrollment = as.numeric(chronic_absenteeism_eligible_cumulative_enrollment)
       )

chronic.22.23 <- chronic.dq %>%
    filter(str_detect( district_name, "North Monterey|Alisal|Soledad|Salinas City"),
           reporting_category == "TA",
           aggregate_level == "D",
           charter_school == "No",
           dass == "All")


chronic.all <- tbl(con,"CHRONIC") %>%
    filter(academic_year == max(academic_year) ,
           county_code == "27",
           charter_yn == "No",
           #            district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate)

chronic.21.22 <- chronic.all %>%
    filter(str_detect( district_name, "North Monterey|Alisal|Soledad|Salinas City"),
           reporting_category == "TA",
           aggregate_level == "D",
          charter_school == "No")


rpp.report.dq <- bind_rows( chronic.21.22, chronic.22.23)



rpp.report <- tribble(~geo,~rate,~year, ~colored,
                       "California",12.0 , 2019, "light gray",
                       "Monterey County",11.2 ,2019, "light gray",
                      #  "Salinas City",9.5 , 2019,"light gray",
         #              "North Monterey County",14.4 , 2019,"light gray",
                #       "Soledad",12.9 , 2019,"light gray",
                      "Alisal",8.2 , 2019,"light gray",
                       "California",30.8 , 2022, "#6C9BA6",
                       "Monterey County", 28.9 ,2022,"#6C9BA6",
                       # "Salinas City",37.9 , 2022, "#6C9BA6", # 37.9 dash #37.7 dq
                       # "North Monterey County",43.4 , 2022, "#6C9BA6", # 43.4 dash #42.3 dq
                       # "Soledad",35.4 , 2022, "#6C9BA6", # 35.4 dash # 32.4 dq
                      "Alisal",32.8 , 2022, "#6C9BA6", # 33.1 dash # 32.8 dq
        # "Salinas City",27.1 , 2023, "#DDA63A",
        # "North Monterey County", 28.0 , 2023, "#DDA63A",
        # "Soledad",28.1 , 2023, "#DDA63A",
      #  "Alisal",22.8 , 2023, "#DDA63A"
                      ) %>%
    mutate( colored = fct_relevel(colored,"light gray" ),
            geo = fct_relevel(geo,"Alisal","Monterey County" )
            )

rpp.report %>%
ggplot(aes(x = geo, y = rate, fill = colored)) +
    #    ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent)) +
    geom_col(
                 color = "black",
             
             position = "dodge2")   +
    geom_text(aes(label = scales::percent(rate/100)),
              position=position_dodge(width=1), size = 4,vjust=-0.5) +
    list(ggthemes::theme_hc(),
     #    ggthemes::scale_fill_few() ,
         ggplot2::theme(plot.title.position = "plot"),
         ggplot2::labs(x = "",
                       y = "",
                       fill ="") ) +
    scale_fill_identity() +
    labs(y = "Percent Chronically Absent",
         title = paste0("Chronic Absenteeism in 2019 and 2022"),
         subtitle = "Gray bars are 2019 and Blue bars are 2022")

ggsave(here("output",paste0("Alisal RPP Graph 2019 and 2022 Comparison ", Sys.Date(),".png")), width = 6, height = 5)    


### Waffle graph -----

library(ggwaffle)
library(ggtext)
library(emojifont)  

# waffle_data <- tribble(~y,~x,~Response, ~colr,
#                        1,1,"Yes", "#6C9BA6",
#                        1,2,"Yes","#6C9BA6",
#                        1,3,"No", "#DDA63A",
#                        2,1,"No","#DDA63A",
#                        2,2,"No","#DDA63A",
#                        2,3,"No","#DDA63A")

# Alisal 2/6
# Salinas City N/A
# North Monterey 2/7
# Soledad  N/A


waffle_data <- tribble(~Response, ~colr,
                       "No", "#DDA63A",
                       "No","#DDA63A",
                       "No","#DDA63A",
                       "No","#DDA63A",
                       "No","#DDA63A",
                       "Yes", "#6C9BA6",
                       "Yes","#6C9BA6"
                       
                       )

waffle_data2 <- waffle_iron(waffle_data, aes_d(group = Response), rows = 2) 


# waffle_data <- waffle_iron(iris, aes_d(group = Species)) 
waffle_data2$label = fontawesome('fa-user')

ggplot(waffle_data2, aes(x, y, colour = group)) + 
    geom_text(aes(label=label), family='fontawesome-webfont', size=20, show.legend = FALSE) +
    coord_equal() + 
    scale_color_manual(values = c("#DDA63A","#6C9BA6")) +
    theme_waffle()  +
    ylim(.5, 2.5) +
    xlim(.5, 4.5) +
    labs(x = "",
         y = "",
         title = "Do you understand the attendance policy?",
         subtitle = "Parent responses: <span style='color: #DDA63A'>No<span> <span style='color: #6C9BA6 '>Yes<span>") + 
    theme(plot.subtitle = element_markdown(size = 17))

ggsave(here("output",paste0("NMCUSD RPP Parent Understanding ", Sys.Date(),".png")), width = 3, height = 2)    



### Four district slope graph -----


rpp.report %>%
    mutate(lablr = paste0(geo,", ",rate)) %>%
    ggplot(aes(x = year, y = rate, group = geo)) +
    geom_line(aes(color = geo, alpha = 1), size = 1) +
    # geom_text_repel(data = caaspp.long %>% filter(Test_Year == "2022"), 
    #                 aes(label = District_Name) , 
    #                 hjust = "left", 
    #                 segment.size = .2,
    #                 segment.color = "grey",
    #                 size = 3, 
    #                 nudge_x = -.4, 
    #                 direction = "y") +
    # geom_text_repel(data = caaspp.long %>% filter(Test_Year == yr.curr), 
    #                 aes(label = District_Name) , 
    #                 hjust = "right", 
    #                 segment.size = .2,
    #                 segment.color = "grey",
    #                 fontface = "bold", 
    #                 size = 3, 
    #                 nudge_x = .4, 
    #                 direction = "y") +
    geom_label_repel(aes(label = lablr), 
  #             size = 4, 
               label.padding = unit(0.05, "lines"), 
               label.size = 0.0) +
    theme_hc() +  # Remove the legend
    # theme(axis.text.y      = element_blank()) +
    # theme(panel.grid.major.y = element_blank()) +
    # theme(panel.grid.minor.y = element_blank()) +
     theme(axis.ticks       = element_blank()) +
    scale_x_discrete(position = "top") +
        ylim(20,45) +
    theme(legend.position = "none") +
    labs(title = "Chronic Absenteeism Decreased from 21-22 to 22-23",
         y = "Percent of Students Chronically Absent",
         x = "",
         caption = "2022 Data from Dashboard; 2023 estimated from CALPADS extracts") +
         theme(
        plot.title = element_markdown(size =18),
        axis.text.y = element_text(size =13)
    )


ggsave(here("output",paste0("All four slope ", Sys.Date(),".png")), width = 8, height = 6)    

    
### Overlapping bars -----

#Dashboard version-
rpp.report %>%
ggplot(mapping = aes(x = fct_rev(as.character(geo)),
                     y = rate/100,
       label = scales::percent(rate/100, accuracy = .1))) +
    geom_col(data =  rpp.report[rpp.report$year == 2022,], 
             fill = "#DDA63A",
             width = 0.75,
             ) +
    geom_col(data = rpp.report[rpp.report$year == 2023,], 
             #         position = "dodge" ,
             width = 0.5,
             fill = "#6C9BA6") + 
    geom_text(nudge_y = -.02 ) +
    coord_flip() + 
    mcoe_theme + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    labs(title = "Chronic Absenteeism Decreased in All Participating Districts<br><span style='color: #DDA63A;'>Yellow is 2022</span> and <span style='color: #6C9BA6;'>Blue is 2023</span>",
         y = "",
         x = "",
         caption = "2022 Data from Dashboard; 2023 estimated from CALPADS extracts"
    ) +
    theme(
        plot.title = element_markdown(size =18),
        axis.text.y = element_text(size =13)
        )
    
ggsave(here("output",paste0("All four overlap ", Sys.Date(),".png")), width = 7, height = 5)    



#Dataquest version-
rpp.report.dq %>%
    ggplot(mapping = aes(x = fct_rev(as.character(district_name)),
                         y = chronic_absenteeism_rate/100,
                         label = scales::percent(chronic_absenteeism_rate/100, accuracy = .1))) +
    geom_col(data =  rpp.report.dq[rpp.report.dq$academic_year == "2021-22",], 
             fill = "#DDA63A",
             width = 0.75,
    ) +
    geom_col(data = rpp.report.dq[rpp.report.dq$academic_year == "2022-23",], 
             #         position = "dodge" ,
             width = 0.5,
             fill = "#6C9BA6") + 
    geom_text(nudge_y = -.02 ) +
    coord_flip() + 
    mcoe_theme + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    labs(title = "Chronic Absenteeism Decreased in All Participating Districts<br><span style='color: #6C9BA6;'>Blue is 2022-23</span> and <span style='color: #DDA63A;'>Yellow is 2021-22</span>",
         y = "",
         x = "",
         caption = "Source: DataQuest Research Files"
    ) +
    theme(
        plot.title = element_markdown(size =18),
        axis.text.y = element_text(size =13)
    )

ggsave(here("output",paste0("All four overlap ", Sys.Date(),".png")), width = 7, height = 5)    


### Dataquest student groups ---

temp <- chronic.dq %>%
    filter(str_detect( district_name, "North Monterey|Alisal|Soledad|Salinas City"),
       #    reporting_category == "TA",
        #   aggregate_level == "D",
           charter_school == "No",
           dass == "All")




### State and County Comparisons
chronic.19 <- tbl(con,"CHRONIC") %>%
    filter(academic_year %in% c("2018-19") ,
           county_code %in% c("00","27"),
           aggregate_level %in% c("T","C","D"),
           #    charter_yn == "No",
           #            district_name == "Salinas City Elementary"
    ) %>%
    collect() 



chronic.dq <- tbl(con,"CHRONIC") %>%
    filter(academic_year %in% c("2018-19","2020-21", "2021-22","2022-23") ,
           county_code %in% c("00","27"),
           aggregate_level %in% c("T","C","D"),
       #    charter_yn == "No",
           #            district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate)


mry.state <- chronic.dq %>%
    filter(charter_yn == "All",
           dass == "All"|is.na(dass),
           aggregate_level %in% c("T","C"),
    ) %>%
    mutate(def.geo = paste0(definition," - ",county_name))


facet.state.comp <- function(subby, namer) {
    


mry.state %>%
    mutate(definition = fct_relevel(definition, "Kindergarten", after = 0 )) %>%
    filter(str_starts(reporting_category, subby)) %>%
    ggplot(aes(x = academic_year, y = chronic_absenteeism_rate, group = county_name ,color = county_name)) +
    geom_line() +
    geom_line(size = 1.5) +
    geom_point(size = 3) + 
    facet_wrap(~definition) +
    mcoe_theme +
    scale_color_few() +
    ylim(0,30) +    
    guides(color = guide_legend(""))


ggsave(here("output",paste0("Monterey and State ", namer," " ,Sys.Date(),".png")), width = 8, height = 4.5)    

}

facet.state.comp("GR", "Grade")

facet.state.comp("R", "Race")

facet.state.comp("S", "Group")

facet.state.comp("T", "Overall")





mry.state %>%
  #  mutate(definition = fct_relevel(definition, "Kindergarten", after = 0 )) %>%
    filter(str_starts(reporting_category, "T")) %>%
    ggplot(aes(x = academic_year, y = chronic_absenteeism_rate/100, group = county_name ,color = county_name,
               label = scales::percent(chronic_absenteeism_rate/100, digits = 1))) +
    geom_line(size = 1.5) +
    geom_point(size = 3 ) + 
    geom_text_repel(color = "gray40") +
    # facet_wrap(~definition) +
    mcoe_theme +
    scale_color_few() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.35)) +
 #   ylim(0,35) +    
    guides(color = guide_legend("")) +
    labs(title = "Monterey County and California Chronic Absenteeism Rates")


ggsave(here("output",paste0("Monterey and State ", "All Students"," " ,Sys.Date(),".png")), width = 8, height = 4.5)    


#### Biggest changes in Monterey County -----
library(ggtext)

chronic.change <- chronic.dq %>% 
    filter(aggregate_level %in% c("D","C"), 
           reporting_category == "TA",
           dass == "All"|is.na(dass),
           charter_school == "No"
           ) %>%
    select(district_name, academic_year, rate) %>%
    pivot_wider(names_from = academic_year,
                values_from = rate) %>%
    mutate(change = `2021-22` - `2022-23`,
           district_name = if_else(is.na(district_name),"Monterey County", district_name),
           kular = case_when(
               str_detect(district_name,"Alisal|North|Soledad|Salinas City") ~ "#DDA63A",
               district_name == "Monterey County" ~ "gray30",
               TRUE ~ "#6C9BA6"),
           district_kular = paste0("<span style=\"color: ", kular, "\">", district_name, "</span>")
)


ggplot2::ggplot(chronic.change, aes( y = change,
                         x =forcats::fct_reorder(district_kular,change) ,
                         label = round2(change,1))
                ) +
    geom_segment( aes(x=forcats::fct_reorder(district_kular, change),
                      xend=forcats::fct_reorder(district_kular, change),
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



ggplot2::ggplot(chronic.change, aes( y = change,
                                     x =forcats::fct_reorder(district_name,change) ,
                                     label = round2(change,1))
) +
    geom_segment( aes(x=forcats::fct_reorder(district_name, change),
                      xend=forcats::fct_reorder(district_name, change),
                      y=0,
                      yend=change,
                   #   color=kular
                      ),
                  color = "grey70",
                  size =2 ) +
    geom_point( # aes(color=kular),
        color = "grey70",
        
                size=5, alpha=0.6) +
    coord_flip() +
    geom_text(size = 3, color = "black") +
#    scale_color_identity()+
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
    theme_hc() +
    mcoe_theme +
    theme(axis.text.y = element_markdown()
    ) +
    labs(title = "Change in Chronic Absenteeism Rates",
         subtitle = "Number of percentage points decreased from 2021-22 to 2022-23",
         source = "Source: DataQuest Research Files, https://www.cde.ca.gov/ds/ad/filesabd.asp, excluding charters")



ggsave(here("output",paste0("RPP Change Single Color " ,Sys.Date(),".png")), width = 8, height = 4.5)    



#### Dataquest Comparison Tables ----


chronic.all <- tbl(con,"CHRONIC") %>%
    filter(# academic_year == max(academic_year) ,
           county_code == "27",
           charter_school == "No",
           #            district_name == "Salinas City Elementary"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","reporting_category") %>%
    mutate(rate = chronic_absenteeism_rate)

rpp.data.table <- chronic.all %>%
    filter(str_detect( district_name, "North Monterey|Alisal|Soledad|Salinas City"),
           academic_year %in% c("2021-22","2022-23"),
#           reporting_category == "TA",
           aggregate_level == "D",
           charter_school == "No",
            dass == "All"
) %>%
    select(district_name, academic_year, definition, rate) %>%
    pivot_wider(names_from = academic_year, values_from = rate) %>%
    na.omit()


clipr::write_clip(rpp.data.table)
