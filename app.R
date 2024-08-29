#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Cell Detection Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num_cells", "Number of cells:", value = 5, min = 1),
      numericInput("num_tubes", "Number of tubes:", value = 30, min = 1),
      sliderInput("sampling_fraction", "Fraction of sample mass used for test:", min = 0, max = 1, value = 0.1),
      sliderInput("detection_probability", "Probability test will detect a cell if it is present in the sampled material:", min = 0, max = 1, value = 0.9),
      numericInput("num_simulations", "Number of Simulations:", value = 10000, min = 1000),
      actionButton("run_simulation", "Run Simulation")
    ),
    
    mainPanel(
      h4("Probability of detecting at least one cell:"),
      verbatimTextOutput("probability_output")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$run_simulation, {
    # Run the simulation when the button is clicked
    found_cell <- replicate(input$num_simulations, {
      
      # Assign cells randomly to tubes using the multinomial distribution
      tubes <- rmultinom(1, input$num_cells, rep(1/input$num_tubes, input$num_tubes))
      
      # Check to see if the cell is in the sampled portion and detected
      sampled_and_detected <- (tubes > 0) & 
        (runif(input$num_tubes) <= input$sampling_fraction) & 
        (runif(input$num_tubes) <= input$detection_probability)
      
      # Check to see if least one cell is found
      any(sampled_and_detected)
    })
    
    # Calculate the probability of finding at least one cell
    probability <- mean(found_cell)
    
    # Display the probability
    output$probability_output <- renderText({
      round(probability, 4)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

