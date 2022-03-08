
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
      menuItem("Finish Times", tabName = "fin_distr"),
      menuItem("Discussion", tabName = "discuss")
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
              box(plotlyOutput("finish_times_hist",width = 500))),
      tabItem("discuss",
              h2("Discussion"),
              p("I downloaded the 2019 Boston Marathon finishing times by country from Kaggle and explored finishing times by age and country. There were 109 countries represented and over 26,000 athletes, so I used only the 30 countries with the most finishers in my analysis."), 
              p("I wanted to create a dashboard to visualize the finish time trends as athletes age as well as differences between finish times by gender. I thought it would be interesting to see if males and females peaked at comparable ages and whether or not this depended on nationality."), 
              p("Lastly, I wanted to see how finish times were distrbuted. With so many athletes, this data set is large enough to yield interesting visualizations even when subset by nationality."),
              p("Through the plots, I wanted to communicate the relationship between age and endurance performance as well as how similar these trends were across countries. Since a smaller value for finish time relates to a faster performance, I inverted the y axis orientation, so the values would peak with faster performance rather than slower performance. Since most athletes have qualifying times based on age standards, one would expect these trends to be similar, but some countries did seem different than the others. Spain, for example, seems to skew disproportionately faster even with older athletes, while most other countries seem to show a peak performance in the athletes' later twenties."),
              p("Another aspect that seems to be revealed throught the plots is that for most countries' athletes, males finish faster than females, but the difference narrows with age."), 
              p("One unanticipated results from the histogram view was just how many more male participants there were than female."),
              p("I elected to allow the viewer to select a country from a drop down list of the top 30 participating countries. I alphabetized the list. One of the reasons for this was so the United States wouldn't always be first, since it is the slowest plot to load due to the number of participants."), 
              p("I also wanted the viewer to be able to select between viewing a plot of finish time trends by age or a histogram of finish time distribution. Both of these are displayed for individual countries."))
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

