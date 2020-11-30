library(shiny)
library(ggplot2)
data(mpg)
attach(mpg)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("PRÁCTICA 1 SHINY MDS_CUNEF"),
    sidebarLayout(
        sidebarPanel( #selectores
            selectInput("variable1",
                        label = "Seleccionamos la primera variable",
                        choices = list("engine displacement litres" = "displ",
                                       "number of cylinders" = "cyl",
                                      "city miles per gallon" = "cty",
                                      "highway miles per gallon" = "hwy")),
            
            selectInput("variable2",
                        label = "Seleccionamos la segunda variable",
                        choices = list("engine displacement litres" = "displ",
                                       "number of cylinders" = "cyl",
                                       "city miles per gallon" = "cty",
                                       "highway miles per gallon" = "hwy")),
            
            sliderInput("tamaño", #barra de tamaño de la muestra
                        label = "Tamaño de nuestra muestra",
                        min = 2,
                        max = nrow(mpg),
                        value = 10),
            
            actionButton("actualizar", label = "Actualizar los datos"),
        ), #boton para ejecutar la orden
        
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("histograma_1"),
           verbatimTextOutput("summary1"),
           plotOutput("histograma_2"),
           verbatimTextOutput("summary2"),
           plotOutput("disp"),
           verbatimTextOutput("pearson")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    muestra <- reactive({
        input$actualizar #actualizar la muestra
        isolate(mpg[sample(1:nrow(mpg), 
                           input$tamaño,
                           replace = FALSE), ])
    })
    
        
    variable1 <- reactive({
        input$actualizar #muestra variable 1
        isolate(muestra()[[input$variable1]])
    })
    
    variable2 <- reactive({
        input$actualizar #muestra variable 2
        isolate(muestra()[[input$variable1]])
    })
    
    output$histograma_1 <- renderPlot({
        hist(variable1(), #histograma conectado al botón variable 1
             main = paste0("Histograma de ",
                           isolate(input$variable1)),
             col = "orange",
             border = "blue")
    })
    
    output$histograma_2 <- renderPlot({
        hist(variable2(), #histograma conectado al botón variable 2
             main = paste0("Histograma de ",
                                   isolate(input$variable2)),
             col = "orange",
             border = "blue")
    })
    
    output$summary1 <- renderPrint({ #summary conectado al botón variable 1
        input$actualizar
        isolate(summary(variable1()))
    }) 
    
    output$summary2 <- renderPrint({ #summary conectado al botón variable 2
        input$actualizar
        isolate(summary(variable2()))
    })
    
    output$pearson <- renderPrint({ #correlación pearson conectada al botón
        input$actualizar
        cor(variable1(),variable2())
        
    })
    
    output$disp <- renderPlot({ #corrplot conectado al botón
        plot(variable1(),variable2(), 
        main = paste0("Diagrama de dispersión de ",
                      isolate(input$variable1 )," ", isolate(input$variable2 )),
        xlab = input$variable1, 
        ylab = input$variable2, 
        pch = 15,
        col = "orange")
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
