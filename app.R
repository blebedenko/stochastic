#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("functions.R")
library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # tags$div(HTML("
    #               MathJax.Hub.Config({
    #               tex2jax"))
    header =     dashboardHeader(),
    body = dashboardBody(
        withMathJax(""),
        titlePanel("Stochastic Models shiny app"),
        h5("You can download all the functions by pressing the button"),
        downloadButton("download_script","Download script"),
        fluidRow(
            box(
                h4("Input for Benny the fisherman"),
                inputPanel(
                    
                    numericInput(inputId = "l1_n",
                                 label = "number of experiments",
                                 min = 10,
                                 max = 1e7,
                                 step = 10,
                                 value = 5000),
                    sliderInput(inputId = "l1_p",
                                label = "Probability of keeping fish",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0.5),
                    sliderInput(inputId = "l1_lambda",
                                label = "\\(\\lambda \\)",
                                min = 1,
                                max = 1000,
                                step = 0.1,
                                value = 5),
                    sliderInput(inputId = "l1_mu",
                                label = "\\(\\mu \\)",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 5),
                    sliderInput(inputId = "l1_price",
                                label = "Price of fish",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 15)
                    
                ),
                
                plotOutput("l1_plot1"),
                h4("Fish shortage distribution"),
                plotOutput("l1_plot2"),
                tableOutput("l1_table1"),
                title = "Lesson 1 - simulation",
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE)
        ),
        fluidRow(
            
        )
        
    ),
    sidebar = dashboardSidebar(disable = TRUE),
    title = "The stochastic models (2320) app"
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$l1_plot1 <- renderPlot(
        
        PlotFish(n = input$l1_n,
                 lambda = input$l1_lambda,
                 p = input$l1_p)
    )
    output$l1_plot2 <- renderPlot({
        fish_kept <- KeepFish(CatchFish(input$l1_n,input$l1_lambda),input$l1_p)
        shortage <- FishShortage(fish_kept=fish_kept,mu = input$l1_mu)
        prob <- mean(shortage>0)
        values <- as.numeric(names(table(shortage)))
        cost <- input$l1_price * sum(values*prop.table(table(shortage)))
        tab <- data.frame(probability=prob,cost=cost)
        par(mfrow=c(1,2))
        observe(table(shortage))
        p1 <- ggplot(as.data.frame(table(shortage)))+aes(x=shortage,y=Freq) +geom_bar(stat = "identity")
        p2 <- tableGrob(tab,theme =ttheme_default(base_size = 20))
        grid.arrange(p1,p2)
    }
    )
    output$download_script <- downloadHandler(
        filename = "functions.R",
        content = function(file){
            renderV("functions.R")
        }
    )
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
