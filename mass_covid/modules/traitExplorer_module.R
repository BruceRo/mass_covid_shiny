traitExplorerUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("uiTraitExplorer"))
}

traitExplorer <- function(input, output, session, df, title = "Title") {
  output$uiTraitExplorer <- renderUI({
    ns <- session$ns
    
    fluidRow(
      box(width = 3, title = "Trait", solidHeader = TRUE, status = "warning",
          selectInput(ns("interactive_trait"), "trait",
                      choices = unique(df$Trait)),
          dateRangeInput(ns("date_range"), "date range",
                         min = min(df$Date),
                         max = max(df$Date),
                         start = min(df$Date),
                         end = max(df$Date)
                         ),
          checkboxInput(ns("trend_line"), "Show trend line", value = FALSE)
      ),
      box(width = 9, title = "Interactive Plot", solidHeader = TRUE, status = "primary",
          plotlyOutput(ns("interactive_trait_plot"))
      )
    )
  })
  
  output$interactive_trait_plot <- renderPlotly({
    ggplotly(interactive_plot(df = filter(df, Date >= input$date_range[1] & Date <= input$date_range[2]), trait_sel = input$interactive_trait, 
                              title = input$interactive_trait,
                              trend_line = input$trend_line))
  })
}


# traitExplorer 2 for df with group
traitExplorer2UI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("uiTraitExplorer"))
}

traitExplorer2 <- function(input, output, session, df, title = NULL) {
  output$uiTraitExplorer <- renderUI({
    ns <- session$ns
    
    fluidRow(
      box(width = 3, title = "Trait", solidHeader = TRUE, status = "warning",
          selectInput(ns("group"), "group",
                      choices = unique(df$Group)),
          selectInput(ns("interactive_trait"), "trait",
                      choices = unique(df$Trait)),
          dateRangeInput(ns("date_range"), "date range",
                         min = min(df$Date),
                         max = max(df$Date),
                         start = min(df$Date),
                         end = max(df$Date)
          ),
          checkboxInput(ns("trend_line"), "Show trend line", value = FALSE)
      ),
      box(width = 9, title = "Interactive Plot", solidHeader = TRUE, status = "primary",
          plotlyOutput(ns("interactive_trait_plot"))
      )
    )
  })
  
  #if (is_null(title)) title <- input$interactive_trait
  output$interactive_trait_plot <- renderPlotly({
    ggplotly(interactive_plot(df = filter(df,  Date >= input$date_range[1] & Date <= input$date_range[2] & Group == input$group), 
                              trait_sel = input$interactive_trait, 
                              title = input$interactive_trait,
                              trend_line = input$trend_line))
  })
}

