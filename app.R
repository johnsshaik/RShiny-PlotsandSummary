library(shiny)
library(datasets)
library(tidyverse)
library(viridis)
library(ggplot2)
library(haven)
library(wordcloud2)
library(car)
library(psych)

ui <- shinyUI(fluidPage(
  titlePanel("Data Analysis & Visualization"),
  tabsetPanel(
    tabPanel(
      "Upload File",
      titlePanel("Uploading Files"),
      sidebarLayout(
        sidebarPanel(
          fileInput(
            'file1',
            'Choose SAS Datasets',
            accept = c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')
          ),
          
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',
                       c(
                         Comma = ',',
                         Semicolon = ';',
                         Tab = '\t'
                       ),
                       ','),
          radioButtons(
            'quote',
            'Quote',
            c(
              None = '',
              'Double Quote' = '"',
              'Single Quote' = "'"
            ),
            '"'
          )
          
        ),
        mainPanel(DT::dataTableOutput("contents"))
      )
    ),
    tabPanel(
      "Generate Plots",
      pageWithSidebar(
        headerPanel(''),
        sidebarPanel(
          selectInput('anl1', 'X axis Variable', ""),
          selectInput('anl2', 'Y axis Variable', "", selected = ""),
          
          selectInput(
            "plot",
            "Plot Type:",
            choices =
              c(
                "Boxplot" = "box" ,
                "Scatter Plot" = "scatter",
                "Connected Scatter Plot" = "conscatter",
                "Bar Plot" = "bar"
                
              )
          ),
          
        ),
        mainPanel(plotOutput('MyPlot'))
      )
    ),
    
    
    tabPanel(
      "Input file Summary",
      pageWithSidebar(
        headerPanel(''),
        sidebarPanel(),
        mainPanel(DT::dataTableOutput("summa"))
      )
    )
    
  )
))

server <- shinyServer(function(input, output, session) {
  data <- reactive({
    req(input$file1)
    
    inFile <- input$file1
    
    df <-
      read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    #  df <- read_sas(inFile$datapath)
    
    
    
    updateSelectInput(
      session,
      inputId = 'anl1',
      label = 'X axis Variable',
      choices = names(df),
      selected = names(df)
    )
    updateSelectInput(
      session,
      inputId = 'anl2',
      label = 'Y axis Variable',
      choices = names(sapply(df, is.numeric)),
      selected = names(sapply(df, is.numeric))
    )
    
    return(df)
  })
  
  output$contents <- DT::renderDataTable({
    data()
  })
  
  
  output$summa <- DT::renderDataTable({
    describe(data())
  })
  
  
  plot1 <- reactive({
    ggplot(data(),
           aes_string(
             x = input$anl1,
             y = input$anl2,
             fill = input$anl1
           )) + geom_boxplot(
             # custom boxes
             
             
             # custom outliers
             outlier.colour = "red",
             outlier.fill = "red",
             outlier.size = 3
           ) +
      scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
      geom_jitter(color = "blue",
                  size = 0.4,
                  alpha = 0.9) +
      theme(legend.position = "top",
            plot.title = element_text(size = 14)) +
      ggtitle("A boxplot with jitter")
    
  })
  
  plot2 <- reactive({
    # scatter plot
    p <-
      ggplot(data(), aes_string(x = input$anl1, y = input$anl2)) + geom_point(
        shape = 21,
        color = "black",
        fill = "#69b3a2",
        size = 3
      )
    print(p)
  })
  
  plot3 <- reactive({
    #connected scatter plot
    p <- ggplot(data(), aes_string(x = input$anl1, y = input$anl2)) +
      geom_line(color = "grey") +
      geom_point(
        shape = 21,
        color = "black",
        fill = "#69b3a2",
        size = 3
      )
    print(p)
    
  })
  
  plot4 <- reactive({
    #Bar plot
    ggplot(data = data(),
           aes_string(
             x = input$anl1,
             y = input$anl2,
             fill = input$anl1
           ))  +
      stat_summary(
        fun = sum,
        geom = "bar",
        colour = "#56B4E9",
        fill = "#56B4E9"
      ) +
      geom_bar(stat = "identity") +
      
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
  # Return the requested graph
  graphInput <- reactive({
    switch(
      input$plot,
      "box" = plot1(),
      "scatter" = plot2(),
      "conscatter" = plot3(),
      "bar" = plot4(),
      "pie" = plot6()
      
      
    )
  })
  
  output$MyPlot <- renderPlot({
    graphInput()
  })
  
  
})


shinyApp(ui, server)