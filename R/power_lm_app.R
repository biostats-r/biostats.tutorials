#' Simulations of power in lm

#' @import shiny
#' @export

power_lm_app <- function() {
  ui <-  fluidPage(
    # Application title
    titlePanel("Simulating linear regressions"),
    
    sidebarLayout(
     
      # Sidebar with a slider input
      sidebarPanel(width = 3,
        radioButtons("type", label = "Type of predictor variable", 
                     choices = c("Categorical", "Continuous"), 
                     selected = "Categorical"),
        sliderInput(
          "n",
          "Number of observations:",
          min = 2,
          max = 100,
          value = 10,
          step = 1
        ),
        sliderInput(
          "delta",
          "True difference in means:",
          min = 0,
          max = 10,
          value = 5
        ),
        sliderInput(
          "sd",
          "Standard deviation of each group:",
          min = 0,
          max = 10,
          value = 5
        ),
        shinyWidgets::sliderTextInput(inputId = "p", 
                                      label = "p level:", 
                                      choices = c(0.001, 0.01, 0.05, 0.1),
                                      selected = 0.05,
                                      grid = TRUE
                                      )
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(width = 9,
        fluidRow(
          column(6, 
            h4("Population distributions"),
            plotOutput("pop_plot")
          ),
          column(6,
            h4(textOutput("samp_size")),
            plotOutput("sample_plot")
          )
        ),
        fluidRow(
          column(4, 
            h4(textOutput("uncertainty_text")),
            plotOutput("uncertainty_plot")
          ),
          column(4,
            h4("Effect size of 100 trials"),
            plotOutput("effect_plot")
          ),
          column(4,
            h4("P-values of 100 trials"),
            plotOutput("p_plot")
          )
        )
      )
    ))
    
  
  # Supress Shiny's auto-load behaviour
  old <- options(shiny.autoload.r = FALSE)
  on.exit(options(old), add = TRUE)  
  
  shinyApp(ui, server = power_lm_server)
}