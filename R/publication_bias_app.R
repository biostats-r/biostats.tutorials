#' Exploration of publication bias

#' @import shiny
#' @export

publication_bias_app <- function() {
  ui <-  fluidPage(
    # Application title
    titlePanel("Simulating Publication Bias"),
    
    sidebarLayout(
     
      # Sidebar with a slider input
      sidebarPanel(width = 3,
        sliderInput(
          "n",
          "Number of observations:",
          min = 10,
          max = 100,
          value = 30,
          step = 1
        ),
        sliderInput(
          "delta",
          "True difference in means:",
          min = 0,
          max = 10,
          value = 1
        ),
        sliderInput(
          "sd",
          "Standard deviation of each group:",
          min = 0,
          max = 10,
          value = 5
        ),
        shinyWidgets::sliderTextInput(inputId = "p", 
                                      label = "Publish if p value <", 
                                      choices = c(0.001, 0.01, 0.05, 0.1, 0.5, 1),
                                      selected = 0.05,
                                      grid = TRUE
                                      )
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(width = 9,
        fluidRow(
          column(12, 
            h4("Population distributions"),
            plotOutput("pop_plot")
          )
        ),
        fluidRow(
          column(12,
            h4("Effect size from 100 experiments"),
            plotOutput("effect_plot")
          )
        )
      )
    ))
    
  
  # Supress Shiny's auto-load behaviour
  old <- options(shiny.autoload.r = FALSE)
  on.exit(options(old), add = TRUE)  
  
  shinyApp(ui, server = publication_bias_server)
}