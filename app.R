#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Yrkestilpasningskalkulator:"),

    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6,
                       sliderInput("Im",
                                   "Extraversion:",
                                   min = 20,
                                   max = 80,
                                   value = 50),      
                       sliderInput("IIm",
                                   "Agreeablness:",
                                   min = 20,
                                   max = 80,
                                   value = 50),      
                       sliderInput("IIIm",
                                   "Conscientiousness:",
                                   min = 20,
                                   max = 80,
                                   value = 50),      
                       sliderInput("IVm",
                                   "Emotional stability:",
                                   min = 20,
                                   max = 80,
                                   value = 50),      
                       sliderInput("V",
                                   "Openness to Experience:",
                                   min = 20,
                                   max = 80,
                                   value = 50)),
                column(6,
                       sliderInput("If",
                                   "Extraversion:",
                                   min = 20,
                                   max = 80,
                                   value = 50),      
                       sliderInput("IIf",
                                   "Agreeablness:",
                                   min = 20,
                                   max = 80,
                                   value = 50),      
                       sliderInput("IIIf",
                                   "Conscientiousness:",
                                   min = 20,
                                   max = 80,
                                   value = 50),      
                       sliderInput("IVf",
                                   "Emotional stability:",
                                   min = 20,
                                   max = 80,
                                   value = 50),      
                       sliderInput("Vf",
                                   "Openness to Experience:",
                                   min = 20,
                                   max = 80,
                                   value = 50)))),
        mainPanel(
            textOutput("result")
        )
    )
)

server <- function(input, output) {

    library(readr)
    library(readxl)
    library(psych)
    library(DT)

    output$result <- renderText ({
        
        If <- input$If
        IIf <- input$IIf
        IIIf <- input$IIIf
        IVf <- input$IVf
        Vf <- input$Vf

        Im <- input$Im
        IIm <- input$IIm
        IIIm <- input$IIIm
        IVm <- input$IVm
        Vm <- input$Vf

        round(100-proxy::dist(rbind(c(If,IIf,IIIf,IVf,Vf),c(Im,IIm,IIIm,IVm,Vm)),
                    method="Euclidean")/120*100,0)
    })
}

                                        # Run the application 
shinyApp(ui = ui, server = server)
