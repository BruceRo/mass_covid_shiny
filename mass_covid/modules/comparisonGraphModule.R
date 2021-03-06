groupComparisonGraphUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("uiComparisonGraph"))
}

groupComparisonGraph <- function(input, output, session, 
                            df, df_pop, title = "Title", perCapCapable = TRUE, starting_comparison = NULL) {
  output$uiComparisonGraph <- renderUI({
    ns <- session$ns
    
    fluidRow(
      if (perCapCapable){
        box(width = 3, title = "Trait", solidHeader = TRUE, status = "warning",
            selectInput(ns("groupCompare_vec"), "Select Groups",
                        choices = unique(df$Group),
                        multiple = TRUE,
                        selected = starting_comparison),
            selectInput(ns("trait"), "Select Trait",
                        choices = unique(df$Trait)),
            checkboxInput(ns("per_cap"), "Per 100,000", value = TRUE),
            checkboxInput(ns("trend_line"), "Show trend line", value = FALSE),
            dateRangeInput(ns("date_range"), "date range",
                           min = min(df$Date),
                           max = max(df$Date),
                           start = min(df$Date),
                           end = max(df$Date)
            )
        ) 
      } else {
        box(width = 3, title = "Trait", solidHeader = TRUE, status = "warning",
            selectInput(ns("groupCompare_vec"), "groups",
                        choices = unique(df$Group),
                        multiple = TRUE),
            selectInput(ns("trait"), "Trait",
                        choices = unique(df$Trait))
        )
      },
      box(width = 9, title = "Comparison Plot", solidHeader = TRUE, status = "primary",
          plotOutput(ns("compare_plot"))
      )
    )
  })
  
  output$compare_plot <- renderPlot({
    validate(need(input$groupCompare_vec, message = FALSE))
    comparison_plot(df = filter(df,  Date >= input$date_range[1] & Date <= input$date_range[2]), df_pop, input$groupCompare_vec, input$trait, perCap = input$per_cap, title = "Title", trend_line = input$trend_line)
  })
}

