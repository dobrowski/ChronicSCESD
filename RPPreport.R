library(ggthemes)
library(ggplot2)
library(tidyverse)
library(here)

rpp.report <- tribble(~geo,~rate,~year, ~colored,
                      "California",12.0 , 2019, "light gray",
                      "Monterey County",11.2 ,2019, "light gray",
                       "Salinas City",9.5 , 2019,"light gray",
         #              "North Monterey County",14.4 , 2019,"light gray",
                #       "Soledad",12.9 , 2019,"light gray",
                  #    "Alisal",8.2 , 2019,"light gray",
                      "California",30.8 , 2022, "#6C9BA6",
                      "Monterey County", 28.9 ,2022,"#6C9BA6",
                       "Salinas City",37.7 , 2022, "#6C9BA6"
        #               "North Monterey County",42.3 , 2022, "#6C9BA6"
              #         "Soledad",32.4 , 2022, "#6C9BA6"
                 #     "Alisal",32.8 , 2022, "#6C9BA6"
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
                       "Yes", "#6C9BA6",
                       "Yes","#6C9BA6",
                       "No", "#DDA63A",
                       "No","#DDA63A",
                       "No","#DDA63A",
                       "No","#DDA63A")

waffle_data2 <- waffle_iron(waffle_data, aes_d(group = Response), rows = 2) 


# waffle_data <- waffle_iron(iris, aes_d(group = Species)) 
waffle_data2$label = fontawesome('fa-user')

ggplot(waffle_data2, aes(x, y, colour = group)) + 
    geom_text(aes(label=label), family='fontawesome-webfont', size=20, show.legend = FALSE) +
    coord_equal() + 
    scale_color_manual(values = c("#DDA63A","#6C9BA6")) +
    theme_waffle()  +
    ylim(.5, 2.5) +
    xlim(.5, 3.5) +
    labs(x = "",
         y = "",
         title = "Do you understand the attendance policy?",
         subtitle = "Parent responses: <span style='color: #DDA63A'>No<span> <span style='color: #6C9BA6 '>Yes<span>") + 
    theme(plot.subtitle = element_markdown(size = 17))

ggsave(here("output",paste0("Alisal RPP Parent Understanding ", Sys.Date(),".png")), width = 3, height = 2)    
