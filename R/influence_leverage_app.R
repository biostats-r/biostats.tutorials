#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# TODO
# Move R2 to figure annotation?
# colour table
# Replot diagnostic plots. Maybe combine data and facet.
# Explanatory text

#' Diagnostic plots with outliers, leverage points and influential points

#' @import shiny
#' @import ggplot2
#' @import ggfortify
#' @importFrom broom glance tidy
#' @importFrom dplyr summarise bind_rows select recode
#' @importFrom rlang .data
#' @export

# library(tidyverse)
# library(broom)
# library(ggfortify)

influence_leverage_app <- function() {
    # Define UI for application that draws a histogram
    ui <- fluidPage(
    
        # Application title
        titlePanel("Exploring influential and leverage points"),
        fluidRow(
            column(width = 3,
            radioButtons("choice", "diagnostic to plot", 
                         choiceNames = c("Residual vs. Fitted", "Normal Q-Q", "Scale-Location", "Cook's distance", "Residual vs Leverage", "Cook's distance vs Leverage"),
                        choiceValues = 1:6)
            ),
            column(width = 9,
                plotOutput("rawPlot", click = "plot_click")
            )
        ),
        fluidRow(
            column(
                width = 7,
                tableOutput("effect_table")
            ),
            column(
              width = 5, 
              tableOutput("performance_table")
            )
        ),
        fluidRow(
            column(width = 5, 
                   plotOutput("diagnostic_black")
            ),
            column(width = 7, 
                   plotOutput("diagnostic_red")
            )
        )
    )
    
    # Define server logic required to draw a histogram
    server <- function(input, output) {
    
     plot_data <- reactiveVal({
         t1 <- tibble(x = 1:10, y = x + rnorm(10, sd = 2), colour = "black") 
         t2 <- summarise(t1, x = mean(x), y = mean(y), colour = "red") 
         bind_rows(t1, t2)
     })
        
     observeEvent(input$plot_click, {
         old <- plot_data()
         old[nrow(old), 1:2] <- list(input$plot_click$x, input$plot_click$y)
         plot_data(old)
     })
     
     #fit models
     mods <-  reactive({
         mod_all <- lm(y ~ x, data = plot_data())
         mod_black <- lm(y ~ x, data = plot_data(), subset = colour == "black")
         list(mod_all = mod_all, mod_black = mod_black)
         }
     )
     
     #plot data
     output$rawPlot <- renderPlot({
         ggplot(plot_data(), aes(
             x = .data$x,
             y = .data$y,
             colour = I(.data$colour), 
             size = if_else(.data$colour == "red", 2, 1)
         )) +
             geom_point() +
             geom_smooth(
                 method = "lm",
                 formula = y ~ x,
                 mapping = aes(group = 1),
                 colour = "red",
                 fill = "red"
             ) +
             geom_smooth(
                 method = "lm",
                 formula = y ~ x,
                 data = plot_data() %>% filter(.data$colour != "red"),
                 colour = "black"
             ) +
             scale_x_continuous(limits = c(0, 20), expand = c(0, 0)) + 
             scale_y_continuous(limits = c(min(plot_data()$y) - 2, 20), expand = c(0, 0)) +
             theme(legend.position = "none")
         
     })
     
     #output tables
     output$effect_table <- renderTable({
         bind_rows(
           red = tidy(mods()$mod_all),
           black = tidy(mods()$mod_black),
           .id = "colour"
     ) %>% 
             select(-.data$statistic, coefficient = .data$term) %>% 
             mutate(coefficient = recode(.data$coefficient, "(Intercept)" = "Intercept", "x" = "Slope"))
             
         })
     
     output$performance_table <- renderTable({
         bind_rows(
             red = glance(mods()$mod_all),
             black = glance(mods()$mod_black),
             .id = "colour"
         ) %>% 
             select(.data$colour, .data$r.squared, .data$p.value)
     })
     
     #diagnostic plots
     output$diagnostic_black <- renderPlot(autoplot(mods()$mod_black, which = as.numeric(input$choice)))
     output$diagnostic_red <- renderPlot(
         autoplot(mods()$mod_all, which = as.numeric(input$choice), data = plot_data(), colour = "colour")
         )
     
     
    }
    
    # Supress Shiny's auto-load behaviour
    old <- options(shiny.autoload.r = FALSE)
    on.exit(options(old), add = TRUE)  
    
    # Run the application 
    shinyApp(ui = ui, server = server)
}