#' Server for lm power simulation
#' @param input see shinyApp
#' @param output see shinyApp
#' 
#' @import shiny
#' @import ggplot2
#' @importFrom purrr map rerun map_dfr
#' @importFrom tibble tibble
#' @importFrom broom tidy
#' @importFrom dplyr bind_rows filter arrange mutate row_number if_else
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom stats lm dnorm qnorm rnorm runif
#' @importFrom rlang .data
#' 

#### Server ####
publication_bias_server <- function(input, output) {
  theme_set(theme_bw())
  group_colours <- scale_colour_brewer(palette = "Dark2")
  group_fill <- scale_fill_brewer(palette = "Dark2")
    
  output$pop_plot <- renderPlot({
    
      upper <- max(0, input$delta) + input$sd * 3
      lower <- min(0, input$delta) - input$sd * 3
      
      dist <- bind_rows(
        control = tibble(
          x = seq(lower, upper, length.out = 101),
          y = dnorm(.data$x, mean = 0, sd = input$sd)
        ),
        treat = tibble(
          x = seq(lower, upper, length.out = 101),
          y = dnorm(.data$x, mean = input$delta, sd = input$sd)
        ),
        .id = "treatment"
      )
      
      pop_plot <- ggplot(data = dist, aes(x = .data$x, y = .data$y, fill = .data$treatment)) +
        geom_area(alpha = 0.3, position = "identity") +
        geom_vline(mapping = aes(xintercept = .data$x, colour = .data$treatment),
                   data = tibble(
                     x = c(0, input$delta), 
                     treatment = c("control", "treat")),
                   show.legend = FALSE) +
        labs(x = "Value", y = "Density", fill = "Treatment") +
        group_fill +
        group_colours + 
        theme(legend.position = c(.99, .99), legend.justification = c(1, 1))
        
    pop_plot
  })
  
  
  #simulate data
  mods <- reactive({
      sims <- rerun(100, tibble(
          treatment = rep(c("control", "treat"), each = input$n),
          value = rnorm(
            n = input$n * 2,
            mean = rep(c(0, input$delta), each = input$n),
            sd = input$sd
          )
        ))
  
  list(
    result = map(sims, ~lm(formula = value ~ treatment, data = .x)) %>% 
      map_dfr(tidy) %>% 
      filter(.data$term != "(Intercept)") %>%
      arrange(.data$estimate) %>% 
      mutate(n = row_number()),
    sample = sims[[1]]
  )
  
  })
  
 output$effect_plot <- renderPlot({
   overall_mean_effect <- mods()$result %>% 
     summarise(mean = mean(.data$estimate))
   sig_mean_effect <-mods()$result %>% 
     filter(.data$p.value < input$p) %>% 
     summarise(mean = mean(.data$estimate))
   means <- bind_rows(Overall = overall_mean_effect, 
                      Significant = sig_mean_effect, 
                      True = tibble(mean = input$delta), 
                      .id = "Mean")
     
  ggplot(mods()$result, aes(x = .data$estimate, fill = .data$p.value < input$p)) + 
    geom_histogram(bins = 25, alpha = 0.7) +
    geom_vline(aes(xintercept = .data$mean, colour = .data$Mean), data = means, linetype = "dashed") +
    labs(
      x = "Estimated difference in means",
      fill = glue("p < {input$p}")) +
    scale_colour_manual(values = c("grey60", "#377EB8", "#000000")) +
    scale_fill_manual(values = c("grey80", "#377EB8"))
    
})
 
  return(output)
}
  