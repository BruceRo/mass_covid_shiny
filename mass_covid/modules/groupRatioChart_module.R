groupPercentageGraphUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("uiGroupPercentageGraph"))
}

groupPercentageGraph <- function(input, output, session, 
                                 df, df_pop, title = "Title") {
  output$uiGroupPercentageGraph <- renderUI({
    ns <- session$ns
    
    fluidRow(
      box(width = 3, title = "Select Numerator and Denominator", solidHeader = TRUE, status = "warning",
          selectInput(ns("numerator"), "Numerator",
                      choices = unique(df$Trait)),
          selectInput(ns("denominator"), "Denominator",
                      choices = c(unique(df$Trait), "Population"))
      ),
      box(width = 9, title = "Percent by Group", solidHeader = TRUE, status = "primary",
          plotOutput(ns("ratio_plot"))
      )
    )
  })
  
  output$ratio_plot <- renderPlot({
    temp <- df %>% 
      pivot_wider(names_from = Trait, values_from = Value) %>% 
      mutate(Population = get_pop(df_pop, Group))
    temp <- temp %>% 
      mutate(Ratio = !!sym(input$numerator)/!!sym(input$denominator)*100)
    ggplot(temp, aes(x = Group, y = Ratio, fill = Group)) +
      geom_col() +
      labs(title = title) +
      theme_minimal()
    
    #validate(need(input$groupCompare_vec, message = FALSE))
  })
}

