#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#' @describeIn biostats_apps t-tests and t-distribution
#' @importFrom shinyWidgets chooseSliderSkin
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel sliderInput
#' radioButtons mainPanel plotOutput renderPlot shinyApp
#' @importFrom stats dt qt
#' @export

t_test_app <- function(){
  # Define UI for application
  ui <- fluidPage(
      # Application title
      titlePanel("t test"),
  
      # Sidebar with a slider input for number of bins
      sidebarLayout(
          sidebarPanel(
              chooseSliderSkin("Round"),
              sliderInput("df",
                          "Number of degrees of freedom:",
                          min = 1,
                          max = 50,
                          round = TRUE,
                          value = 5),
              radioButtons("alpha",
                          "\u03b1:",
                          c("p = 0.05" = "0.05", "p = 0.01" = "0.01")
              ),
              radioButtons("type",
                           "Alternative:",
                           c("two-sided", "less than", "greater than")
              ),
          ),
  
          # Show a plot of the generated distribution
          mainPanel(
             plotOutput("distPlot")
          )
      )
  )
  # Run the application
  shinyApp(ui = ui, server = t_test_server)
}
# Define server logic required to draw a histogram
t_test_server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        xmax <- qt(p = 0.999, df = input$df)
        x    <- seq(-ceiling(xmax), ceiling(xmax), length.out = 200)
        y <- dt(x, df =  input$df)


        base <- data.frame(x = x, y = y) |>
          ggplot(aes(x = x, y = y)) +
          geom_area(fill = "grey90", colour = "black") +
          labs(x = expression(t~value), y = "Density" ) +
          theme_bw(base_size = 16)

        # critical regions

        if (input$type != "two-sided") {
          xthresh <- qt(p = 1 - as.numeric(input$alpha), df = input$df)
          x2 <- seq(xthresh, ceiling(xmax), length.out = 100)
          if (input$type == "less than") {
            xthresh <- -xthresh
            x2 <- -x2
          }

          y2 <- dt(x2, df = input$df)
          df2 <- data.frame(x = x2, y = y2)

          base +
            geom_area(data = df2, fill = "red", alpha = 0.5, colour = "red") +
            annotate(geom = "text",
                     x = xthresh,
                     y = y2[1] + 0.05 * (max(y)- y2[1]),
                     label = glue::glue("t[{input$df}*';'~{input$alpha}]=={round(xthresh, 2)}"),
                     hjust = 0, vjust = 0, parse = TRUE, size = 5)


        } else {
          xthresh <- qt(p = 1 - as.numeric(input$alpha)/2, df = input$df)
          x2 <- seq(xthresh, ceiling(xmax), length.out = 100)

          y2 <- dt(x2, df = input$df)
          df2 <- data.frame(x = x2, y = y2)

          base +
            geom_area(data = df2, fill = "red", alpha = 0.5, colour = "red") +
            annotate(geom = "text",
                     x = xthresh,
                     y = y2[1] + 0.05 * (max(y)- y2[1]),
                     label = glue::glue("t[{input$df}*';'~{input$alpha}]=={round(xthresh, 2)}"),
                     hjust = 0, vjust = 0, parse = TRUE, size = 5) +
            geom_area(data = df2, aes(x = -x), fill = "red", alpha = 0.5, colour = "red") +
            annotate(geom = "text",
                     x = - xthresh,
                     y = y2[1] + 0.05 * (max(y)- y2[1]),
                     label = glue::glue("t[{input$df}*';'~{input$alpha}]=={round(-xthresh, 2)}"),
                     hjust = 1, vjust = 0, parse = TRUE, size = 5)

        }

    })

}


