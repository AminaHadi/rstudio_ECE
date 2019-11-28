
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)


data("airquality")

shinyUI<-(pageWithSidebar(
    
    headerPanel("Air Quality Dataset Manipulation"),
    
    sidebarPanel(
        selectInput("YVar", "Dependant Variable", choices=c("Ozone","Solar.R", "Wind", "Temp"), selected=c("Ozone")),
        selectInput("XVar", "Plot X Variable", choices=c("Ozone", "Solar.R", "Wind", "Temp"), selected = c("Temp")),
        selectInput("Col", "Plot Colored By", choices=c("Ozone", "Solar.R", "Wind", "Temp"), selected = c("Solar.R")),
        selectInput("Predictors", "Predictor(s)", multiple=TRUE, choices=c("Ozone", "Solar.R", "Wind", "Temp"), selected = c("Temp"))
    ),
    mainPanel(
        plotOutput("Plot"),
        verbatimTextOutput("ModelSummary"),
        verbatimTextOutput("ModelOutput")
        )
    )
    
)     

                        
                      
                 


shinyServer<-(function(input,output) {
    YColumn <- reactive({
        grep(input$YVar, names(airquality))
    })
    XColumn <- reactive({
        grep(input$XVar, names(airquality))
    })
    ColorColumn <- reactive({
        grep(input$Col, names(airquality))
    })
    output$Plot <- renderPlot(
        ggplot2::qplot(x = airquality[,XColumn()], y = airquality[,YColumn()], color = airquality[,ColorColumn()], geom="point", size=I(5)) + ggplot2::xlab(input$XVar) + ggplot2::ylab(input$YVar) + ggplot2::scale_colour_continuous(name = input$Col))
    
    df_pred <- reactive({
        df <- data.frame(airquality[,YColumn()])
        names(df)[1] <- input$YVar
        if ("Ozone" %in% input$Predictors) df$Ozone = airquality$Ozone
        if ("Solar.R" %in% input$Predictors) df$Solar.R = airquality$Solar.R
        if ("Wind" %in% input$Predictors) df$Wind = airquality$Wind
        if ("Temp" %in% input$Predictors) df$Temp = airquality$Temp
        df
    })
    
    model <- reactive({
        if (input$YVar == "Ozone") retmodel = train(Ozone ~ ., data=df_pred(), method="lm")
        if (input$YVar == "Solar.R") retmodel = train(Solar.R ~ ., data=df_pred(), method="lm")
        if (input$YVar == "Wind") retmodel =  train(Wind ~ ., data=df_pred(), method="lm")
        if (input$YVar == "Temp") retmodel = train(Temp ~ ., data=df_pred(), method="lm")
        retmodel
    })
    
    output$ModelSummary <- renderPrint(summary(model()))
    
    output$ModelOutput <- renderPrint(model())
    
})

# Create Shiny object
shinyApp(shinyUI,shinyServer)                      