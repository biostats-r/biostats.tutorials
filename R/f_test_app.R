#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#' @describeIn biostats_apps f-test and f-distribution
#' @importFrom shinyWidgets chooseSliderSkin
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel sliderInput
#' radioButtons mainPanel plotOutput renderPlot shinyApp
#' @importFrom stats df qf
#' @export

f_test_app <- function() {
  # Define UI for application 
  ui <- fluidPage(
  
      # Application title
      titlePanel("F test"),
  
      # Sidebar with a slider input for number of bins
      sidebarLayout(
          sidebarPanel(
            withMathJax(helpText("$$\\color{black}{\\frac{SS_{regression}/df_{regression}}{SS_{residual}/df_{residual}}}$$")),
           chooseSliderSkin("Round"),
              sliderInput("numerator",
                          "Regression degrees of freedom:",
                          min = 1,
                          max = 10,
                          round = TRUE,
                          value = 5),
              sliderInput("denominator",
                          "Residual degrees of freedom:",
                          min = 1,
                          max = 10,
                          round = TRUE,
                          value = 5),
              radioButtons("alpha",
                          "\u03b1:",
                          c("p = 0.05" = "0.05", "p = 0.01" = "0.01")
              )
          ),
  
          # Show a plot of the generated distribution
          mainPanel(
             plotOutput("distPlot")
          )
      )
  )
  # Run the application
  shinyApp(ui = ui, server = f_test_server)
}

# Define server logic required to draw a histogram
f_test_server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
       axis_max <- 500
        xmax <- min(axis_max, qf(p = 0.995, df1 = input$numerator, df2 = input$denominator))
        x    <- seq(0, ceiling(xmax), length.out = 200)
        y <- df(x, df1 = input$numerator, df2 = input$denominator)

        xthresh <- qf(p = 1 - as.numeric(input$alpha), df1 = input$numerator, df2 = input$denominator)
        if(xthresh > axis_max) {
          xthresh <- NA_real_
          x2 <- numeric(0)
        } else {
          x2 <- seq(xthresh, ceiling(xmax), length.out = 100)
        }
        y2 <- df(x2, df1 = input$numerator, df2 = input$denominator)
        df2 <- data.frame(x = x2, y = y2)
        data.frame(x = x, y = y) |>
          ggplot(aes(x = x, y = y)) +
          geom_area(fill = "grey90", colour = "black") +
          geom_area(data = df2, fill = "red", alpha = 0.5, colour = "red") +
          labs(x = expression(italic(F)~value), y = "Density" ) +
          annotate(geom = "text", x = xthresh, y = y2[1] + 0.05 * (max(y[is.finite(y)])- y2[1]), label = glue::glue("italic(F)[{input$numerator}*','~{input$denominator}*';'~{input$alpha}]=={round(xthresh, 2)}"), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
          theme_bw(base_size = 16)
    })
}


