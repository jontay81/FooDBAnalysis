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
        mainPanel(plotlyOutput("fruitPlot"),
                  htmlOutput("docs"))
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
    
    output$docs <- renderPrint({
    
        paste("<b>FooDB Data Viewer</b><br/>",
              "Use the checkbox group above to select fruits. <br/><br/>",
              
              "The plot will output the chemical compounds found in each fruit<br/>
               with their concentration on the Y axis and their Molecular Weights<br/>
               on the X axis.<br/><br/>",

              "Click on a point to view chemical data about that compound<br/><br/>",
              
              "Scalbert, A.; Andres-Lacueva, C.; Arita, M.; Kroon, P.; Manach, C.; Urpi-Sarda, M.; Wishart, D.S. (2011). <br/>
               'Databases on Food Phytochemicals and Their Health-Promoting Effects'. <br/>
               J. Agric. Food Chem. 59 (9): 4331–4348. doi:10.1021/jf200591d. PMID 21438636.<br/><br/>",
              
              paste( "<a target='_blank'href=http://foodb.ca/",
                    " <b>FooDB.ca</b> </a>",sep=""),
              
              sep="")
        
    })
    
}


shinyApp(ui = ui, server = server)
