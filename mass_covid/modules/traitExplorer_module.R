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
                      choices = unique(df$Trait))
      ),
      box(width = 9, title = "Interactive Plot", solidHeader = TRUE, status = "primary",
          plotlyOutput(ns("interactive_trait_plot"))
      )
    )
  })
  
  output$interactive_trait_plot <- renderPlotly({
    ggplotly(interactive_plot(df, input$interactive_trait, input$interactive_trait))
  })
}

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
                      choices = unique(df$Trait))
      ),
      box(width = 9, title = "Interactive Plot", solidHeader = TRUE, status = "primary",
          plotlyOutput(ns("interactive_trait_plot"))
      )
    )
  })
  
  #if (is_null(title)) title <- input$interactive_trait
  output$interactive_trait_plot <- renderPlotly({
    ggplotly(interactive_plot(filter(df, Group == input$group), input$interactive_trait, title = input$interactive_trait))
  })
}

