#
# This is Shiny app graphs absenteeism data for the Research Pratice Partnerships in Monterey County
#

library(shiny)
library(tidyverse)
library(here)
library(scales)
library(ggthemes)
library(ggrepel)


### Functions -------


round2 = function(x, digits=2) {
    posneg = sign(x)
    z = abs(x)*10^digits
    z = z + 0.5
    z = trunc(z)
    z = z/10^digits
    z*posneg
}


absent.all <- read_rds("absent-all.rds")

chronic.all <- read_rds("chronic-all.rds")

graph2023 <- read_rds("graph2023.rds")


chronic_graph <- function(df, indi = rate  , xxx, tit, subtit) {
    
    df  %>%
        
        ggplot( aes( y = {{indi}}, x =fct_reorder({{xxx}}, {{indi}}) ,  label = round2( {{indi}}, 2) )) +
        geom_segment( aes(x=fct_reorder({{xxx}}, {{indi}}), xend=fct_reorder({{xxx}}, {{indi}}), y=0, yend={{indi}}),
                      color="orange",
                      size =2 ) +
        geom_point( color="orange", size=7, alpha=0.6) +
        coord_flip() +
        geom_text(size = 5, color = "black") +
        theme_hc() +
        list(
             ggthemes::scale_fill_few() ,
             ggplot2::theme(plot.title.position = "plot")) +
        labs(x = "",
             y = "",
             color ="",
             fill = "",
             title = tit, 
             subtitle = subtit,
             caption = "Source: https://www.cde.ca.gov/ds/ad/filesabd.asp") +
        theme(text = element_text(size = 20)) +
        scale_y_continuous(
            expand = expansion(c(0.1, 0.1))
        )
    
}


chronic_graph_percent <- function(df, indi = rate  , xxx, tit, subtit) {
    
    df  %>%
        
        ggplot( aes( y = {{indi}}, x =fct_reorder({{xxx}}, {{indi}}) ,  label = percent( {{indi}}, accuracy = .1) )) +
        geom_segment( aes(x=fct_reorder({{xxx}}, {{indi}}), xend=fct_reorder({{xxx}}, {{indi}}), y=0, yend={{indi}}),
                      color="orange",
                      size =2 ) +
        geom_point( color="orange", size=7, alpha=0.6) +
        coord_flip() +
        geom_text(size = 5, color = "black") +
        theme_hc() +
        list(
            ggthemes::scale_fill_few() ,
            ggplot2::theme(plot.title.position = "plot")) +
        labs(x = "",
             y = "",
             color ="",
             title = tit, 
             subtitle = subtit,
             caption = "Source: https://www.cde.ca.gov/ds/ad/filesabd.asp") +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        )+
        theme(text = element_text(size = 20))
    
}

##

absent.student.group <- function(df, dist, skul) {
    
    df %>%
        filter(!is.na(average_days_absent),
            #   aggregate_level == "D"
               ) %>%
        chronic_graph(definition,
                      indi = average_days_absent,
                      tit = "Average Days Absent by Student Group for the School Year",
                      subtit = paste0(dist, " - ", skul ," - 2021-22")) 
    
 #   ggsave(here("output", paste0(dist, " - District Level - Absent by Student Group.png") ) )
    
}


absent.by.school <- function(df, dist) {
    
    df %>%
        filter(!is.na(average_days_absent),
               aggregate_level == "S",
               reporting_category == "TA") %>%
        chronic_graph(school_name,
                      indi = average_days_absent,
                      tit = "Average Days Absent by School for the School Year",
                      subtit = paste0(dist, " - 2021-22"))
    
 #   ggsave(here("output", paste0(dist, " - District Level - Absent by School.png") ) )
    
}




chronic.student.group <- function(df, dist, skul) {
    
    
    df %>%
        filter(
        #    aggregate_level == "D",
               !is.na(rate)) %>%
        chronic_graph_percent(definition, 
                              indi = rate/100 ,
                              tit = "Percent Chronically Absent by Student Group",
                              subtit = paste0(dist, " - ", skul , " - 2021-22")) 
    
    
#    ggsave(here("output", paste0(dist, " - District Level - Chronic by Student Group.png") ) )
    
}


chronic.by.school <- function(df, dist) {
    
    df %>%
        filter(reporting_category == "TA",
               !is.na(rate)) %>%
        mutate(school_name = if_else(is.na(school_name), "District Overall", school_name)) %>%
        chronic_graph_percent(school_name,
                              indi = rate/100 ,
                              tit = "Percent Chronically Absent by School",
                              subtit = paste0(dist, " - 2021-22"))
    
    
 #   ggsave(here("output", paste0(dist, " - District Level - Chronic by School.png") ) )
    
}    




absence.reason.group <- function(df, dist, skul) {
    
    
    df %>%
 #       filter(aggregate_level == "D") %>%
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
        list(ggthemes::theme_hc(),
             ggthemes::scale_fill_few() ,
             ggplot2::theme(plot.title.position = "plot"),
             ggplot2::labs(x = "",
                           y = "",
                           fill ="") ) +
        coord_flip() + 
        labs(title = "Reasons for Absences by Student Group",
             subtitle = paste0(dist, " - ", skul , " - 2021-22") ) +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        ) +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))+
        theme(text = element_text(size = 20))
    
    
#    ggsave(here("output", paste0(dist, " - District Level - Absence Reason by Group.png") ), width = 15, height = 10 )
    
}  


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
        list(ggthemes::theme_hc(),
             ggthemes::scale_fill_few() ,
             ggplot2::theme(plot.title.position = "plot"),
             ggplot2::labs(x = "",
                           y = "",
                           fill ="") ) +
        coord_flip() + 
        labs(title = "Reasons for Absences by School",
             subtitle = paste0(dist, " - 2021-22") ) +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        ) +
        guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
        theme(text = element_text(size = 20))
    
    
#    ggsave(here("output", paste0(dist, " - District Level - Absence Reason by School.png") ), width = 10, height = 6 )
    
}   



#### UI -------



dist.list <- c("Alisal Union", "North Monterey", "Salinas City", "Soledad")
sch.list <- c("District Office")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Research Practice Partnership on Chronic Absenteeism"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(p("This application looks at Absenteeism data from the 2021-22 school year."),
                     br(),
            selectInput("district.choice",
                        "Select a district:",
                        dist.list),
            selectInput("school.choice",
                        "Select a school:",
                        choices = sch.list),
            br(),
            img(src = "logo.png", height="30%", width="30%", align = "left")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Average Days Absent",  
                         plotOutput("adaPlot", height = "600px") ,
                         plotOutput("adaSchPlot", height = "600px") 
                ),
                tabPanel("Percent Chronically Absent",  
                         plotOutput("chronicPlot", height = "600px") ,
                         plotOutput("chronicSchPlot", height = "600px") 
                ),
                tabPanel("Reason for Absence",  
                         plotOutput("reasonPlot", height = "600px") ,
                         plotOutput("reasonSchPlot", height = "600px") 
                ),
                tabPanel("2022-23 Year to Date",  
                         plotOutput("currentPlot", height = "600px") ,
                         plotOutput("currentSchPlot", height = "600px") 

                )
            )
        )
    )
)


### Server ---------

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    ## Updating dataframes
    
    absent <- reactive({
        absent.all %>%
            filter(str_detect(district_name, input$district.choice),
                   str_detect(school_name, input$school.choice)
                   )
    })
    
    
    chronic <- reactive({
        chronic.all %>%
            filter(str_detect(district_name, input$district.choice),
                   str_detect(school_name, input$school.choice)
            )
    })
    
    
    ## Getting the list of schools based on Dsitrict Choice
    
    school.list <- reactive({
        absent.all %>%
            filter(str_detect(district_name, input$district.choice)) %>%
            pull(school_name) %>%
            unique()
    })
    
    observe({
        updateSelectizeInput(session, "school.choice", choices = school.list())
    })
    

    ## My PLots    
    
    output$adaPlot <- renderPlot({

        absent() %>%
        # absent.all %>%
        #     filter(str_detect(district_name, input$district.choice)) %>%
        absent.student.group(input$district.choice, input$school.choice)
    })
    
    output$adaSchPlot <- renderPlot({
        absent.all %>%
            filter(str_detect(district_name, input$district.choice)) %>%
            absent.by.school(input$district.choice)
    })
    
    output$chronicPlot <- renderPlot({
        chronic() %>%
            chronic.student.group(input$district.choice, input$school.choice)
    })
    
    output$chronicSchPlot <- renderPlot({
        chronic.all %>%
            filter(str_detect(district_name, input$district.choice)) %>%
            chronic.by.school(input$district.choice)
    })
    
    output$reasonPlot <- renderPlot({
        absent() %>%
            absence.reason.group(input$district.choice, input$school.choice)
    })
    
    
    output$reasonSchPlot <- renderPlot({
        absent.all %>%
            filter(str_detect(district_name, input$district.choice)) %>%
            absence.reason.school(input$district.choice)
    })
    
    
    
    output$currentPlot <- renderPlot({
        
        timeframe <- case_when( input$district.choice == "Salinas City" ~ "March 15",
                                input$district.choice == "Alisal Union" ~ "March 16",
                                input$district.choice == "Soledad" ~ "March 14",
                                input$district.choice == "North Monterey" ~ "February 28",
                                )
        
        graph2023 %>%
            filter(district_name == input$district.choice,
                   school_name ==  input$school.choice ) %>%
            chronic_graph_percent(indi = chronic.rate  , xxx = definition, tit = paste0(input$district.choice ," Chronic Absenteeism"), 
                                  subtit = paste0(input$school.choice, " 2022-23 through ", timeframe)) +
                                 labs( caption = "Source: Data exported from SIS")
    })
    
    output$currentSchPlot <- renderPlot({
        timeframe <- case_when( input$district.choice == "Salinas City" ~ "March 15",
                                input$district.choice == "Alisal Union" ~ "Second Trimester",
                                input$district.choice == "Soledad" ~ "March 14",
                                input$district.choice == "North Monterey" ~ "February 28",
        )
        
        
        graph2023 %>%
            filter(district_name == input$district.choice,
                   definition == "Over all" ) %>%
            chronic_graph_percent(indi = chronic.rate  , xxx = school_name, 
                                  tit = paste0(input$district.choice," Chronic Absenteeism"), 
                                  subtit = paste0("2022-23 through ", timeframe)) +
            labs( caption = "Source: Data exported from SIS")
        
    })
    
    
    
    
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
