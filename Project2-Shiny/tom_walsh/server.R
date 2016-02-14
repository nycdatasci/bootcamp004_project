
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source('helpers.R')

library(shiny)
library(ggplot2)

lims = function(stat_string, data) {
  stat = data[, stat_string]
  sd.3 = 3 * sd(stat, na.rm=TRUE)
  m = mean(stat, na.rm=TRUE)
  lower = max(min(stat, na.rm=TRUE), m - sd.3)
  upper = min(max(stat, na.rm=TRUE), m + sd.3)
  return(c(lower, upper))
}

min_filter = function(minute_range) {
  return(nba[nba$MIN_TOTAL.lineup >= minute_range[1] & nba$MIN_TOTAL.lineup <= minute_range[2],])
}

parse_lineup_input = function(input_stat) {
  sprintf('%s.lineup', input_stat)
}

parse_player_input = function(input_stat, input_usage) {
  sprintf('%s.%s', input_stat, if(input_usage) 'usage' else 'player')
}

shinyServer(function(input, output) {
  
  data <- reactive({ min_filter(input$minute_range) })
  dim_data <- reactive({ min_filter(input$dim_minute_range) })
  density_data <- reactive({ min_filter(input$density_minute_range) })
  
  lineup_stat <- reactive({ parse_lineup_input(input$lineup_stat) })
  player_stat <- reactive({ parse_player_input(input$player_stat, input$usage) })
  dim_lineup_stat <- reactive({ parse_lineup_input(input$diminishing_stat)})
  dim_player_stat <- reactive({ parse_player_input(input$diminishing_stat, input$dim_usage) })
  dim_y_stat <- reactive({ 
    sprintf('%s - (%d * %s)', dim_lineup_stat(), if(input$dim_sum) 5 else 1, dim_player_stat()) 
  })
  dim_x_stat <- reactive({
    if(input$dim_usage) {
      '5 * USG_PCT.player - 1'
    } else {
      sprintf('%d * %s', if(input$dim_sum) 5 else 1, dim_player_stat())
    }
  })
  dim_base_stat <- reactive({
    if(input$dim_usage) {
      'USG_PCT.player'
    } else {
      dim_player_stat()
    }
  })
  density_lineup_stat = reactive({ parse_lineup_input(input$density_stat)})
  density_player_stat = reactive({ parse_player_input(input$density_stat, input$den_usage)})
  
  xlim <- reactive({ lims(player_stat(), data()) })
  ylim <- reactive({ lims(lineup_stat(), data()) })

  output$lineup_plot <- renderPlot({
    ggplot(data(), aes_string(x = player_stat(), y = lineup_stat())) + 
      coord_cartesian(xlim=xlim(), ylim=ylim()) + 
      geom_raster(interpolate = TRUE) + 
      geom_smooth()
  })
  
  output$diminishing_plot <- renderPlot({ 
    ggplot(dim_data(), aes_string(x = dim_x_stat(), y = dim_y_stat())) + 
      geom_smooth()
  })
  
  output$density_lineup_plot <- renderPlot({
    ggplot(density_data(), aes_string(x = density_lineup_stat())) +
      geom_density()
  })
  
  output$density_player_plot <- renderPlot({
    ggplot(density_data(), aes_string(x = density_player_stat())) +
      geom_density()
  })
  
  output$lineup_text <- renderPrint({
    writeLines(sprintf("Lineups in minute-filtered dataset: %d\n", nrow(data())))
    linear_fit = lm(as.formula(sprintf('%s~%s', lineup_stat(), player_stat())), data=data())
    p_value = anova(linear_fit)$`Pr(>F)`[1]
    if (p_value < 0.01) {
      writeLines(sprintf('Linear fit: %s = %f + %f * %s\n', lineup_stat(), summary(linear_fit)$coefficients[1,1], summary(linear_fit)$coefficients[2,1], player_stat()))  
    } else {
      writeLines(sprintf("There doesn't seem to be a significant linear relationship between\n%s and %s\n", player_stat(), lineup_stat()))
    }
    writeLines(sprintf('Pr(>F): %f', p_value))
  })
  
  output$diminishing_text <- renderPrint({
    writeLines(sprintf("Lineups in minute-filtered dataset: %d\n", nrow(data())))
    formula = as.formula(sprintf('(%s)~(%s)', dim_y_stat(), dim_base_stat()))
    linear_fit = lm(formula, data=dim_data())
    direction = (if (summary(linear_fit)$coefficients[1,1] > 0) 'diminishing' else 'increasing')
    p_value = anova(linear_fit)$`Pr(>F)`[1]
    if (p_value < 0.01) {
      writeLines(sprintf('There is evidence of %s returns for %s.\n', direction, input$diminishing_stat))
    } else {
      writeLines(sprintf('We do not see evidence of diminishing returns for %s.\n', input$diminishing_stat))
    }
    writeLines(sprintf('Pr(>F): %f', p_value))
  })
  
  output$density_text <- renderPrint({
    writeLines(sprintf("Lineups in minute-filtered dataset: %d\n", nrow(data())))
  })

})
