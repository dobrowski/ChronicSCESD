
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(here)
library(ggrepel)
library(MCOE)
library(ggtext)

rpp.report <- tribble(~geo,~rate,~year, ~colored,
                      # "California",12.0 , 2019, "light gray",
                      # "Monterey County",11.2 ,2019, "light gray",
                      #  "Salinas City",9.5 , 2019,"light gray",
         #              "North Monterey County",14.4 , 2019,"light gray",
                #       "Soledad",12.9 , 2019,"light gray",
                  #    "Alisal",8.2 , 2019,"light gray",
                      # "California",30.8 , 2022, "#6C9BA6",
                      # "Monterey County", 28.9 ,2022,"#6C9BA6",
                       "Salinas City",37.9 , 2022, "#6C9BA6", # 37.9 dash #37.7 dq
                       "North Monterey County",43.4 , 2022, "#6C9BA6", # 43.4 dash #42.3 dq
                       "Soledad",35.4 , 2022, "#6C9BA6", # 35.4 dash # 32.4 dq
                      "Alisal",33.1 , 2022, "#6C9BA6", # 33.1 dash # 32.8 dq
        "Salinas City",27.1 , 2023, "#DDA63A",
        "North Monterey County", 28.0 , 2023, "#DDA63A",
        "Soledad",28.1 , 2023, "#DDA63A",
        "Alisal",22.8 , 2023, "#DDA63A"
                      ) %>%
    mutate( colored = fct_relevel(colored,"light gray" ),
            geo = fct_relevel(geo,"Salinas City","Monterey County" )
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

ggsave(here("output",paste0("Salinas City RPP Graph 2019 and 2022 Comparison ", Sys.Date(),".png")), width = 6, height = 5)    


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

    
### Overlapping bars

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


22.8/33.1
28/43.4
27.1/37.9
28.1/35.4
