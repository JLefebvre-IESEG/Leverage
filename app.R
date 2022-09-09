## Reproduce the graph WACC per leverage from Berk & DeMarzo

library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("WACC and Leverage"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "DoA",
                  label = "Leverage ratio D/A:",
                  min = 0,
                  max = 0.9,
                  value = 0),


      sliderInput(inputId = "rD",
              label = "Cost of debt:",
              min = 0,
              max = 0.5,
              value = 0.05),


      sliderInput(inputId = "rU",
              label = "Cost of unlevered equity:",
              min = 0,
              max = 0.5,
              value = 0.1)
    ),

            
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    rU <- input$rU
    rD <- input$rD
    DoA <- input$DoA
    rE <- rU+DoA/(1-DoA)*(rU-rD)
    WACC    <- DoA*rD+(1-DoA)*rE

    barplot(c(WACC,rD,rE), space = 1, names.arg = c("WACC", "rD", "rE"), col = "#75AADB", border = "white",
         main = "Cost of capital")
    
  })
  
}

shinyApp(ui = ui, server = server)
