library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(h1("Ejercicio 2 de Shiny
",align = "center",style='background-color:orange;
                     padding-left: 15px')),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(tags$style("#sampleSize {border: 2px solid #dd4b39;}"),
                     tags$style("#sampleSize {background-color:orange;}"),
            numericInput("sampleSize", 
                                  label = h3("Tamaño Muestral del Gráfico",align = "center"), 
                                  min = 30,
                                  value = 30) ),#ponerlo para que no aparezca
        mainPanel(
            plotOutput("distPlot", hover =  "hover"), # hover, de este modo se genera la interacción del gráfico y el hover.
            #click es el evento que vas a recibir, el input click se va a recibir cuando se clique sobre la gráfica displot
            dataTableOutput("Filtrado")#ha de ser siempre un dataframe
        )
    )
)

server <- function(input, output) {
    
    dataset <- reactive({
        data.frame(x=rnorm(input$sampleSize), y=rnorm(input$sampleSize)) # dataframe con ambos ejes en distribución normal de tamaño 100
    })
    output$distPlot <- renderPlot({ #especificaciones del gráfico
        ggplot(dataset(), aes(x=x, y=y)) + #data = dataset y los ejes son la distribución normal
            ggtitle(paste0(isolate(input$sampleSize)," Número de muestras para el gráfico de dispersión de X e Y")) + #buscar como hacer el titulo automatico con el numero de variables
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_point(shape = 21, colour = "orange", fill = "black", size = 3, stroke = 3) +
            theme(plot.background = element_rect(colour = "orange", size = 1),  panel.background = element_rect(fill = "lightblue", colour = NA),plot.title = element_text(face = "bold", colour = "orange")
)
    })
    
    
    
    output$Filtrado <- renderDataTable({
        nearPoints(dataset(),input$hover,maxpoints = 2,  threshold = 5, addDist = TRUE) #añadimos threshold = 5#para indicar que la distancia maxima ha de ser de 5 entre los 2 puntos
        # Añadimos addDist para ver que como maximo los puntos estan a una distancia de 5
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
