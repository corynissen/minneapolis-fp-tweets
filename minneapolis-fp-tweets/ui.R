
library(shiny)
library(ggplot2)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Minneapolis Area Food Poisoning Tweets"),
   
  sidebarPanel(
    selectInput(inputId = "category",
      label = "Select category label",
      choices = c("All", "Good", "Junk"),
      selected = "All"),
    checkboxInput("rt", "Show Retweets", FALSE),
    br(),
    uiOutput("day.slider"),
    br(),
    actionButton("refresh", "Click to Update Data (takes about a minute)")
  ),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plot"),    
    tableOutput("tweet.table")
  )
))
