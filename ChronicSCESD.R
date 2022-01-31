


library(tidyverse)
library(janitor)
library(here)
library(scales)
library(ggthemes)
library(MCOE)
library(ggrepel)


con <- mcoe_sql_con()



chronic <- tbl(con,"CHRONIC") %>%
     filter(AcademicYear == "2020-21" ,
            CountyCode == "27",
            CharterYN == "No",
            DistrictName == "Salinas City Elementary"
          #   (cds == "27660680000000"),
    #         id == "ccidownload2021.txt"
    ) %>%
    collect()  %>%
    left_join_codebook("CHRONIC","ReportingCategory") %>%
    mutate(rate = ChronicAbsenteeismRate)


chronic_graph <- function(df, xxx, tit) {
    
        df  %>%
        
        ggplot( aes( y = rate, x =fct_reorder({{xxx}}, rate) ,  label = round2( rate, 2) )) +
        geom_segment( aes(x=fct_reorder({{xxx}}, rate), xend=fct_reorder({{xxx}}, rate), y=0, yend=rate),
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
             subtitle = "Salinas City ESD - 2021",
             caption = "Source: https://www.cde.ca.gov/ds/ad/filesabd.asp")
    
}


chronic %>%
    filter(AggregateLevel == "D",
           !is.na(rate)) %>%
    chronic_graph(definition,"Percent Chronically Absentee by Student Group")

ggsave("District Level.png")



chronic %>%
    filter(ReportingCategory == "TA",
           !is.na(rate)) %>%
    mutate(SchoolName = if_else(is.na(SchoolName), "District Overall", SchoolName)) %>%
    chronic_graph(SchoolName,"Percent Chronically Absentee by School")


ggsave("By School.png")
