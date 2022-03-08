
library(dplyr)
library(rgeos)
library(ggplot2)
library(tidyverse)
library(viridis)
library(plotly)
library(shiny)
library(ggthemes)
library(viridisLite)
library(shinydashboard)


# 2019 Boston Marathon Finish Times by Country

boston_data <- read.csv("DatasetBoston2019.csv")
most_boston <- boston_data %>% count(Country, sort=TRUE)
most_boston <- most_boston[1:30,]
# Create vector with names of top 30 participating countries

boston <- filter(boston_data, Country %in% most_boston[,1])
most_boston <- most_boston[order(most_boston$Country),]

head(most_boston)
head(boston)

ui <- dashboardPage(
  
  #format
  skin="purple",
  
  dashboardHeader(
    title="2019 Boston Marathon Finish Times"
  ),
  #sidebar
  dashboardSidebar(
    #menu
    sidebarMenu(
      selectInput("in_country","Choose a country", c(most_boston[,1])),
      menuItem("Nationality", tabName = "byNation"),
      menuItem("Finish Times", tabName = "fin_distr")
    )
  ),
  
  dashboardBody(
    tabItems(
      #first Page
      tabItem("byNation",
              h2("Finishers from ", textOutput("in_country1", inline = TRUE)),
              h3("Finish Times by Age"),
              box(plotlyOutput("plot_by_nat"), width = 500)),
      #second Page
      tabItem("fin_distr",
              h2("Finish Times Distribution for Athletes from", textOutput("in_country3", inline = TRUE)),
              box(plotlyOutput("finish_times_hist",width = 500)))
    )
  )
)

#--------------------------
# time by age and country
#--------------------------

server <- function(input, output) { 
  
  output$in_country1 <- renderText({
    input$in_country
  })
  
  
  output$in_country3 <- renderText({
    input$in_country
  })
  
  output$plot_by_nat <- renderPlotly({
    in_country <-  input$in_country
    plot_by_nat <- ggplot(boston %>% filter(Country == in_country),
                          aes(x=Age,
                              y = (Result_sec/3600),
                              color = Gender)) +
      geom_point(alpha = 0.2) +
      theme_tufte(base_size = 10, base_family = "sans") +
      ylab("Finish Time (hours)") +
      scale_y_reverse() +
      xlab("Age (Years)") +
      theme_minimal() +
      stat_smooth(alpha = 0.15, method = "loess") +
      scale_x_continuous(limits = c(min(boston$Age),max(boston$Age)))
    
    ggplotly(plot_by_nat) %>%
      layout(hovermode = "x") %>%
      rangeslider(start = min(boston$Age),max(boston$Age))
  })
  
  
  output$finish_times_hist <- renderPlotly({
    in_country <-  input$in_country
    finish_times_hist <- ggplot(boston %>% filter(Country==in_country),
                                aes(x=Result_sec/3600, fill= Gender)) +
      geom_histogram(color = "#e9ecef", alpha=0.35, position = "identity",binwidth = .25)+
      theme_minimal() +
      #   labs(fill="") +
      ylab("Finishers") +
      xlab("Finish Time (hours)") 
    
    ggplotly(finish_times_hist) 
  })
}

shinyApp(ui, server)

