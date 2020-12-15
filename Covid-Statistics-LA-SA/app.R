#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rstanarm)
library(ggthemes)
library(gt)
library(gtsummary)
library(broom.mixed)

# Read in rds files
confirmed_cases <- read_rds("Confirmed_Cases.rds")
confirmed_cases_la <- read_rds("Confirmed_Cases_La.rds")
confirmed_cases_sa <- read_rds("Confirmed_Cases_Sa.rds")
confirmed_cases_model <- read_rds("Confirmed_Cases_Model.rds")

race_ethnicity <- read_rds("race_ethnicity.rds")
pre_post_covid_mh <- read_rds("pre_post_covid_mh.rds")

# Define UI for application that draws a histogram

# First panel ofr impact on minorities and mental health disorders

ui <- fluidPage(
    navbarPage(
        "Covid-19: Domestic Impact and International Pandemic Management",
        tabPanel("USA",
                 fluidPage(
                     titlePanel("Covid-19 Impact on Minorites and Mental Health Disorders"),
                         mainPanel(
                             plotOutput("race_ethnicity"),
                             br()
                         )
                     ,
                         mainPanel(
                             plotOutput("pre_post_covid_mh"),
                             br()
                         )
                     
                 )
        ),
        
        # Second panel: geom_smooth() for case counts, embedded 2 graphs for comparison
        
        tabPanel("Latin America",
                 fluidPage(
                     titlePanel("Latin America Covid-19 Case Count"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("Country_1_la",
                                         "Select a Country:",
                                         choices = unique(confirmed_cases_la$country_region))
                             ),
                         mainPanel(
                             plotOutput("country1Plot_la"),
                             br()
                             )
                         ),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("Country_2_la",
                                         "Select a 2nd Country:",
                                         choices = unique(confirmed_cases_la$country_region))
                         ),
                         mainPanel(
                             plotOutput("country2Plot_la"),
                             br()
                         )
                     )
                     )
                 ),
        
        # Similar to previous panel, adjusted for South Asian countries specifically
        
        tabPanel("South Asia",
                 fluidPage(
                     titlePanel("South Asia Covid-19 Case Count"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("Country_1_sa",
                                         "Select a Country:",
                                         choices = unique(confirmed_cases_sa$country_region))
                         ),
                         mainPanel(
                             plotOutput("country1Plot_sa"),
                         )
                     ),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("Country_2_sa",
                                         "Select a 2nd Country:",
                                         choices = unique(confirmed_cases_sa$country_region))
                         ),
                         mainPanel(
                             plotOutput("country2Plot_sa"),
                         )
                     )
                 )),
        
        # Model panel: implementation coded in Data.Rmd, embedded in the Shiny app with
        # gt and broom.mixed packages
        
        tabPanel("Model",
                 titlePanel("Linear Regression Model"),
                 h1("Mortality as a Function of Region"),
                 br(),
                 gt_output(outputId = "table"),
                 br(),
                 h2("Interpretation"),
                 br(),
                 p("For this model, I wanted to examine the relationship between Covid-related
                   mortality and the number of cases in each country, with an additional variable
                   to measure whether or not mortality is affected by the region of the world that an
                   individual inhabits. Interestingly enough, my model shows that mortality from
                   Covid-19 is more likely in Latin America than in South Asia -- although there are a
                   number of potential explanations for this, additional examination of the healthcare
                   infrastructure in both of these regions is necessary to understand the specific factors
                   that have increased mortality in Latin America."),
            
                 
        ),
        
        # Final page -- project description, data sources, Github link
        
        tabPanel("About",
                 h1("Background"),
                 p("For my final project, I wanted to anaylze how Covid-19 has impacted racial and ethnic minorities
                   in the United States. Institutional shortcomings in the healthcare provided to minorities has emerged
                   as leading public health challenge in our country, and I was curious to see how this issue manifested
                   within the current pandemic that we are facing."),
                 p("Additionally, I wanted to assess how Covid-19 has impacted mental health disorders in the United States.
                   Multiple studies conducted over the past year have confirmed that the isolation, loneliness, job insecurity,
                   and general uncertainty of the pandemic have caused mental health illnesses to skyrocket. I wanted to 
                   analyze this on a state-by-state basis, looking for areas and regions that have been particularly vulnerable
                   to these challenges."),
                 p("Lastly, I wanted to see how Covid-19 is being managed abroad, particularly in the regions of Latin America
                   and South Asia. Given that gaps in healthcare are rampant in these regions, I wanted to provide country-by-country
                   comparisons of case counts to see whether or not governmental policy impacts the overall trajectory of the virus
                   and understand which policies were the most effective."),
                 br(),
                 h2("Data"),
                 p("I gathered my data on the number of case counts and confirmed Covid-related deaths from the Humanitarian Exchange,
                   which provided daily measurements of these variables."),
                 p("My project code can be found on my",
                   a("GitHub",
                     href = 
                         "https://github.com/neloyshome/Covid-Statistics-LA-SA",)),
                 br(),
                 h3("About Me"),
                 p("My name is Neloy Shome and I am a senior at Harvard College studying Human and Evolutionary Biology with a
                   secondary in Global Health and Health Policy. I hope to use data to identify critical public health issues
                   and analyze the factors that determine an individual's health outcomes. You can reach me at neloyshome@college.harvard.edu.")
                 )
        
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Graphics for USA
    
    output$pre_post_covid_mh <- renderPlot({
        pre_post_covid_mh %>%
            ggplot(aes(x = Pre_or_Post, y = Percentage, fill = Pre_or_Post)) +
            geom_col() +
            facet_wrap(~ state, nrow = 5) +
            labs(title = "Increase in Mental Health Disorders since Covid-19 Pandemic",
                 subtitle = "Higher Rates of Anxiety/Depression Across the Board",
                 x = "Current vs 2018",
                 y = "Percentage with Mental Health Condition") + 
            scale_x_discrete(labels = c(" ", " ")) +
            scale_y_continuous(labels = scales::percent_format()) +
            theme(axis.text = element_text(size = 5), strip.text = element_text(size = 7), 
                  panel.grid = element_blank(), panel.spacing.x = unit(3, "mm")) +
            scale_fill_manual(name = "Current vs 2018",
                              values = c("red", "blue"),
                              labels = c("Current", "2018"))
    })
    
    output$race_ethnicity <- renderPlot({
        race_ethnicity %>%
            ggplot(aes(x = Race, y = Proportion, fill = Race)) +
            geom_col() +
            facet_wrap(~ state, nrow = 5) +
            labs(title = "Proportion of Covid-19 Cases in US States by Race/Ethnicity",
                 subtitle = "Black/Hispanic Minorities Affected at Higher Rates",
                 x = "Race",
                 y = "Proportional Measurement of Cases") +
            scale_x_discrete(labels = c(" ", " ", " ", " ")) +
            theme(axis.text = element_text(size = 5), strip.text = element_text(size = 7),
                  panel.grid = element_blank(), panel.spacing.x = unit(3, "mm")) +
            scale_fill_manual(name = "Race",
                              values = c("red", "blue", "green", "yellow"),
                              labels = c("White", "Black", "Hispanic", "Asian"))
    })

    # Graphics for Latin America
    
    output$country1Plot_la <- renderPlot({
        confirmed_cases %>%
            filter(country_region == input$Country_1_la) %>%
            mutate(month = str_sub(date, 7, 7)) %>%
            filter(month %in% c("3", "4", "5", "6", "7", "8", "9")) %>%
            mutate(day = str_sub(date, 10, 10)) %>%
            mutate(first_day = str_sub(date, 9, 9)) %>%
            filter(day == "1") %>%
            filter(first_day == "0") %>%
            ggplot(aes(x = date, y = number_of_cases, group = country_region)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "auto", se = FALSE, color = "red") +
            labs(title = "Number of Covid 19 Cases Since Start of Pandemic",
                 x = "Month",
                 y = "Cases") +
            theme_economist()
    })
    
    output$country2Plot_la <- renderPlot({
        confirmed_cases %>%
            filter(country_region == input$Country_2_la) %>%
            mutate(month = str_sub(date, 7, 7)) %>%
            filter(month %in% c("3", "4", "5", "6", "7", "8", "9")) %>%
            mutate(day = str_sub(date, 10, 10)) %>%
            mutate(first_day = str_sub(date, 9, 9)) %>%
            filter(day == "1") %>%
            filter(first_day == "0") %>%
            ggplot(aes(x = date, y = number_of_cases, group = country_region)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "auto", se = FALSE, color = "red") +
            labs(title = "Number of Covid 19 Cases Since Start of Pandemic",
                 x = "Month",
                 y = "Cases") +
            theme_economist()
    })
    
    # Graphics for SA
    
    output$country1Plot_sa <- renderPlot({
        confirmed_cases %>%
            filter(country_region == input$Country_1_sa) %>%
            mutate(month = str_sub(date, 7, 7)) %>%
            filter(month %in% c("3", "4", "5", "6", "7", "8", "9")) %>%
            mutate(day = str_sub(date, 10, 10)) %>%
            mutate(first_day = str_sub(date, 9, 9)) %>%
            filter(day == "1") %>%
            filter(first_day == "0") %>%
            ggplot(aes(x = date, y = number_of_cases, group = country_region)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "auto", se = FALSE, color = "red") +
            labs(title = "Number of Covid 19 Cases Since Start of Pandemic",
                 x = "Month",
                 y = "Cases") +
            theme_economist()
    })
    
    output$country2Plot_sa <- renderPlot({
        confirmed_cases %>%
            filter(country_region == input$Country_2_sa) %>%
            mutate(month = str_sub(date, 7, 7)) %>%
            filter(month %in% c("3", "4", "5", "6", "7", "8", "9")) %>%
            mutate(day = str_sub(date, 10, 10)) %>%
            mutate(first_day = str_sub(date, 9, 9)) %>%
            filter(day == "1") %>%
            filter(first_day == "0") %>%
            ggplot(aes(x = date, y = number_of_cases, group = country_region)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "auto", se = FALSE, color = "red") +
            labs(title = "Number of Covid 19 Cases Since Start of Pandemic",
                 x = "Month",
                 y = "Cases") +
            theme_economist()
    })
    
    output$table <- render_gt({
        set.seed(10)
        fit_1 <- stan_glm(data = confirmed_cases_model,
                          formula = deaths ~ number_of_cases + region,
                          refresh = 0)
        
        tbl_regression(fit_1, intercept = TRUE) %>%
            as_gt() %>%
            tab_header(title = md("Regression of Case Count and Region on Covid-19 Deaths"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
