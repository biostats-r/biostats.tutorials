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
power_lm_server <- function(input, output) {
  max_continuous <- 2
  group_colours <- scale_colour_brewer(palette = "Dark2")
  group_fill <- scale_fill_brewer(palette = "Dark2")
  significance_colours <- scale_colour_brewer(palette = "Set1")
  significance_fill <- scale_fill_brewer(palette = "Set1")
  
  
  output$samp_size <- renderText({
    if (input$type == "Categorical") {
      glue("Sample of {input$n} observations for each treatment")
    } else {
      glue("Sample of {input$n} observations") 
    }
  })
  
  output$uncertainty_text <- renderText({
    glue("{(1 - input$p) * 100}% confidence intervals of 100 trials")
  })
    
  output$pop_plot <- renderPlot({
    
    if (input$type == "Categorical") {
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
        
    } else {
      
      dist <- tibble(x = seq(0, max_continuous, length.out = 50),
                     value = .data$x * input$delta) 
      
      pop_plot <- ggplot(data = dist, aes(x = .data$x, y = .data$value)) +
        geom_ribbon(aes(ymin = .data$value - 2 * input$sd, ymax = .data$value + 2 * input$sd), alpha = 0.3) +
        geom_ribbon(aes(ymin = .data$value - input$sd, ymax = .data$value + input$sd), alpha = 0.3) +
        geom_line() +
        labs(x = "Predictor", y = "Response") 
    }
    pop_plot
  })
  
  
  #simulate data
  mods <- reactive({
    if (input$type == "Categorical") {
      sims <- rerun(100, tibble(
          treatment = rep(c("control", "treat"), each = input$n),
          value = rnorm(
            n = input$n * 2,
            mean = rep(c(0, input$delta), each = input$n),
            sd = input$sd
          )
        ))
    } else {
      #simulate continuous data
      sims <- rerun(100,
              tibble(
                treatment = runif(input$n, 0, max_continuous),
                value = .data$treatment * input$delta +
                  rnorm(
                    n = input$n,
                    mean = 0,
                    sd = input$sd
                  )
              ))
    }

  
  list(
    result = map(sims, ~lm(formula = value ~ treatment, data = .x)) %>% 
      map_dfr(tidy) %>% 
      filter(.data$term != "(Intercept)") %>%
      arrange(.data$estimate) %>% 
      mutate(n = row_number()),
    sample = sims[[1]]
  )
  
  })
  
  output$p_plot <- renderPlot({
    ggplot(mods()$result, aes(x = .data$p.value, fill = .data$p.value < input$p)) + 
      geom_histogram(boundary = 0, bins = 100) +
      xlim(0, 1) +
      geom_vline(xintercept = input$p, colour = "red", linetype = "dashed") +
      labs(x = "P value", fill = glue("p < {input$p}")) +
      significance_fill +
      theme(legend.position = c(.99, .99), legend.justification = c(1, 1))
  })
  
 output$effect_plot <- renderPlot({
  ggplot(mods()$result, aes(x = .data$estimate, fill = .data$p.value < input$p)) + 
    geom_histogram(bins = 25, show.legend = FALSE) +
    geom_vline(xintercept = input$delta) +
    labs(
      x = if_else(input$type == "Categorical", "Estimated difference in means", "Estimated slope"),
      fill = glue("p < {input$p}")) +
     significance_fill
})
 
 output$uncertainty_plot <- renderPlot({
   mult <- qnorm(1 - input$p / 2) 
   df <- mods()$result %>% 
     mutate(xmin =  .data$estimate - mult * .data$std.error,
            xmax = .data$estimate + mult * .data$std.error)
   
   ggplot(df, aes(x = .data$estimate, xmin =  .data$xmin, xmax = .data$xmax, y = .data$n, colour = .data$p.value < input$p)) + 
     geom_errorbarh(show.legend = FALSE) +
     geom_point(show.legend = FALSE) +
     geom_vline(xintercept = input$delta) +
     geom_vline(xintercept = 0, linetype = "dashed") +
     scale_y_continuous(expand = c(0.01, 0.01)) +
     labs(x = if_else(input$type == "Categorical", "Estimated difference in means", "Estimated slope"), 
          y = "Simulation number") +
     significance_colours
 })
 
 output$sample_plot <- renderPlot({
     if (input$type == "Categorical") {
       ggplot(mods()$sample, aes(x = .data$treatment, y = .data$value)) + 
         geom_boxplot(aes(fill = .data$treatment), alpha = 0.3, outlier.shape = NA) + 
         geom_jitter(aes(colour = .data$treatment), height = 0) +
         #    stat_summary(fun.data = "mean_sd") +
         # geom_vline(xintercept = input$delta) +
         # geom_vline(xintercept = 0, linetype = "dashed") +
         labs(x = "Treatment", y = "Response") +
         group_fill +
         group_colours + 
         theme(legend.position = "none")
     } else {
       ggplot(mods()$sample, aes(x = .data$treatment, y = .data$value)) + 
         geom_point() +
         geom_smooth(method = "lm", formula = y ~ x) +
         labs(x = "Predictor", y = "Response")
     }
 })
 
  return(output)
}
  