library(dplyr)
library(shiny)
library(plotly)

setwd("~/code/R/FooDBAnalysis/")


fruits <-  c('Apple', 'Pear', 'Pineapple', 'Peach', 'Watermelon')

data <- read.csv("filtered_data.csv")

names(data) <- c('x', 'fruit', 'compound', 'concentration', 'description', 'mass', 'inchikey')

data <- filter(data, compound != 'WATER') 

data$mass <- as.numeric(as.character(data$mass))
data$concentration <- as.numeric(as.character(data$concentration))

data <- data[complete.cases(data),]


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("checkbox", "Select Fruit", 
                               c("Apple" = "Apple",
                                 "Peach" = "Peach",
                                 "Pear" = "Pear",
                                 "Pineapple" = "Pineapple",
                                 "Watermelon" = "Watermelon"),
                               selected = "Apple"), 
                               
                                 
            htmlOutput("event")
        ),
        mainPanel(plotlyOutput("fruitPlot"))
    )
)


server <- function(input, output) {

    filteredData <- reactive({
        filteredData <- filter(data, as.factor(fruit) %in% input$checkbox)
    })
    
    
    output$fruitPlot <- renderPlotly({
           plot_ly(filteredData(), x = ~mass, y = ~concentration, color = ~filteredData()$fruit,
                   type='scatter', mode = 'markers')
    })
    

    output$event <- renderPrint({
        point <- event_data("plotly_click")$x
        
        
        if (is.null(point)) "No Point Selected"
        else {
           
            
            data2 <- filter(filteredData(), mass == point)[1,]
            
            HTML( paste("<b>Compound:</b>", data2$compound, "<br/>",
                  "<b>Mass:</b>", data2$mass, "<br/>",
                  "<b>InChiKey:</b> ", 
                  paste("Click ", "<a target=\"_blank\"", "href=http://www.chemspider.com/Search.aspx?q=",
                        data2$inchikey,"> <b>HERE</b> </a>",sep=""), "<br/>",
                  "<b>Description:</b>", data2$description))
            
        }
    })
}


shinyApp(ui = ui, server = server)
