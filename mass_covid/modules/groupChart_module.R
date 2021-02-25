groupChartUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("uiGroupChart"))
}

groupChart <- function(input, output, session, 
                       df, group_pop, title = "Title") {
  output$uiGroupChart <- renderUI({
    ns <- session$ns
    
    fluidRow(
      box(width = 3, title = "Trait", solidHeader = TRUE, status = "warning",
          selectInput(ns("trait"), "trait",
                      choices = unique(df$Trait)),
          checkboxInput(ns("Per_cap"), "Normalize for population (per 100,000)", value = FALSE)
      ),
      box(width = 9, title = "Group Breakdown", solidHeader = TRUE, status = "primary",
          fluidRow(
            column(width = 6, plotOutput(ns("bar_chart"))),
            column(width = 6, plotOutput(ns("pie_chart")))

          )
          
      )
    )
  })
  
  output$bar_chart <- renderPlot({
    if (input$Per_cap){
      df <- df %>% 
        mutate(Value = round(Value/get_pop(group_pop, Group)*100000))
    }
    print(pie_bar_chart(df, 
                        trait = input$trait, 
                        "bar", 
                        title = input$trait))
  })
  
  output$pie_chart <- renderPlot({
    if (input$Per_cap){
      df <- df %>% 
        mutate(Value = round(Value/get_pop(group_pop, Group)*100000))
    }
    print(pie_bar_chart(df, 
                        trait = input$trait, 
                        "pie", 
                        title = input$trait))
  })
}

