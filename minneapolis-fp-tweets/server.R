
library(shiny)
library(lubridate)
library(reshape2)

shinyServer(function(input, output) {
  #source("update_data.R")  # df is all the data
  load("minneapolis_fp.Rdata")
  df <- dcast(df, status.id ~ variable, value.var="value")
  df$is.rt <- as.logical(df$is.rt)
  df$epoch <- as.numeric(df$epoch)
  df$created.at3 <- as.Date(df$created.at2)

  data <- reactive({    
    if(input$refresh > 0){
      isolate({
        source("update_data.R")
        load("minneapolis_fp.Rdata")
        df <- dcast(df, status.id ~ variable, value.var="value")
      })
    }
    df <- df[order(df$epoch, decreasing=TRUE),]
    if(!input$rt){
      df <- subset(df, !is.rt)
    }

    if(input$category != "All"){
      df <- subset(df, classification==input$category)
    }
    
    df
  })

  # had to subset the data after the slider was created
  subset.data <- reactive({
    df <- data()
    max.date <- max(df$created.at3)
    df <- subset(df, created.at3 >= max.date-days(input$day.slider.reactive))
    df
  })

  # create the slider here because I need input from the df dataframe
  output$day.slider <- renderUI({
    df <- data()
    min.date <- min(df$created.at3)
    max.date <- max(df$created.at3)
    max.value <- ceiling(as.numeric((max.date - min.date)))
    #browser()
    return(sliderInput("day.slider.reactive", "Date range (back from present)",
                       min=1, max=max.value, value=7))
  })  

  output$caption <- renderText({
    "Minneapolis Area Food Poisoning Tweets by Time"
  })

  output$plot <- renderPlot({
    df <- subset.data()
    df$created.at3 <- substring(df$created.at2, 1,
                                regexpr(" ", df$created.at2)-1)
    tmp <- as.data.frame(table(df$created.at3, df$classification))
    tmp$Var1 <- as.character(tmp$Var1)
    tmp$Var2 <- as.character(tmp$Var2)
    overall.len <- length(unique(tmp$Var1))
    ticks <- c(seq(1, overall.len, ceiling(overall.len / 10)), overall.len)
    
    p <- ggplot(tmp) + geom_point(aes(x=Var1, y=Freq, colour=Var2)) +
         geom_line(aes(x=Var1, y=Freq, colour=Var2, group=Var2)) +
         xlab("Date") + ylab("Count of Tweets") +
         scale_colour_discrete(name="Tweet Label") +
         scale_x_discrete(breaks=tmp[ticks, "Var1"])
    print(p)
  })

  output$tweet.table <- renderTable({
      df <- subset.data()
      tab <- subset(df, select=c("text", "classification", "created.at",
                            "status.link"))
      
  },include.rownames=FALSE, sanitize.text.function = function(x) x)
      
})
