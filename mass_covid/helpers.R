library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(rjson)
library(plotly)
library(lazyeval)
library(docxtractr)

## Utility Functions
cumulative_to_14_day <- function(cumulative){
  temp <- c(rep(0,14), cumulative[-((length(cumulative)-13):length(cumulative))])
  return(cumulative - temp)
}

cumulative_to_daily <- function(cumulative){
  temp <- c(c(0), cumulative[-length(cumulative)])
  return(cumulative - temp)
}

cumulative_to_active_town <- function(cumulative){
  temp <- c(c(0,0), cumulative[-((length(cumulative)-1):length(cumulative))])
  return(cumulative - temp)
}

get_pop <- function(pop_df, group){
  getPop <- Vectorize(function(group){
    filter(pop_df, Group == group)$Population
  })
  getPop(group)
  
}

get_value_by_date <- function(df, date, trait){
  df %>% filter(Date == date, Trait == trait) %>% `[[`("Value")
}

prep_town_data <- function(file, date){
  town_data <- docxtractr::read_docx(file)
  town_data <- docx_extract_tbl(town_data)
  town_data <- mcga(town_data)
  town_data <- town_data  %>% select(city_town, count) %>% mutate(count = as.numeric(count), Date = date)
}

build_town_data <- function(){
  town_file_list <- read_csv("town_file_list.csv")
  town_file_list <- town_file_list %>% mutate(date = mdy(date))
  for (i in 1:dim(town_file_list)[1]){
    if (i==1){
      town_data <- prep_town_data(town_file_list[[i,1]], town_file_list[[i,2]])
    } else {
      town_data <- rbind(town_data, prep_town_data(town_file_list[[i,1]], town_file_list[[i,2]]))
    }
  }
  return(town_data)
}

# comparison_plot <- function(df = all_data,  cat1_vec, cat2_vec, trait_vec, compare_cat, perCap = FALSE, title = "Title"){
#   df <- df %>% 
#     filter(cat1 %in% cat1_vec) %>% 
#     filter(cat2 %in% cat2_vec) %>% 
#     filter(Trait %in% trait_vec)
#   ggplot(df, aes(x = Date, y = Value, color = !!sym(compare_cat))) +
#     geom_line(alpha = 0.5) +
#     geom_smooth( ) +
#     labs(title = title) +
#     theme_minimal()
# }
#comparison_plot(df = all_data, cat1_vec = "state",cat2_vec = "Massachusetts", trait_vec = c("DeathsDaily", "PositiveDaily"), compare_cat = "Trait" )
comparison_plot <- function(df, df_pop = NULL, group_vec, trait,  perCap = FALSE, title = "Title", trend_line = FALSE){
  df <- df %>%
    filter(Group %in% group_vec) %>%
    filter(Trait == trait) 
  if (perCap) {
    df <- df %>% 
      mutate(Value = round(Value/get_pop(df_pop, Group)*100000))
  }
  if (trend_line){
    ggplot(df, aes(x = Date, y = Value, color = Group)) +
      geom_line(alpha = 0.5) +
      geom_smooth( ) +
      labs(title = title) +
      theme_minimal()
  } else {
    ggplot(df, aes(x = Date, y = Value, color = Group)) +
      geom_line(alpha = 0.5) +
      labs(title = title) +
      theme_minimal()
  }
  
}

interactive_plot <- function(df, trait_sel, title, trend_line = FALSE){
  df <- df %>% 
    filter( Trait == trait_sel)
  if (trend_line){
    ggplotly(
      ggplot(df, aes(x = Date, y = Value)) +
        geom_point() +
        geom_smooth() +
        labs(title = title) +
        theme_minimal()
    ) 
  } else {
    ggplotly(
      ggplot(df, aes(x = Date, y = Value)) +
        geom_point() +
        labs(title = title) +
        theme_minimal()
    )
  }
  
}
#interactive_plot("County", "Middlesex", "PositiveDaily")

# pie_chart <- function(cat1_sel, trait, remove_unknown = FALSE){
#   df <- all_data %>% 
#     filter(cat1 == cat1_sel,
#            Trait == trait,
#            Date == max(all_data$Date)
#     )
#   if (remove_unknown){
#     df <- df %>% 
#       filter(!(cat2 %in% c("Unknown", "Unknown1", "Unknown/Missing")))
#   }
#   g <- ggplot(df, aes(x = "", y = Value, fill = cat2)) +
#     geom_bar(width = 1, stat = "identity", color = "white") +
#     coord_polar("y", start = 0) +
#     labs(title = trait) +
#     theme_void()
# }
#print(pie_chart("Race_Ethnicity", "Positive", TRUE))

pie_bar_chart <- function(df, trait, type = "pie", title = "title"){
  df <- df %>% 
    filter(Trait == trait) %>% 
    select(Group, Value)
  
  if (type == "pie"){
    g <- ggplot(df, aes(x = "", y = Value, fill = Group)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      labs(title = title) +
      #scale_fill_manual(values = ) +
      theme_void()
  } else if (type == "bar"){
    g <- ggplot(df) +
      geom_col(aes(x = Group, y = Value, fill = Group) ) +
      labs(title = title) +
      theme_minimal()
  }
  
}

bar_chart <- function(cat1_sel, trait, remove_unknown = FALSE, interactive_g = TRUE){
  df <- all_data %>% 
    filter(cat1 == cat1_sel,
           Trait == trait,
           Date == max(all_data$Date)
    )
  if (remove_unknown){
    df <- df %>% 
      filter(!(cat2 %in% c("Unknown", "Unknown1", "Unknown/Missing")))
  }
  g <- ggplot(df, aes(x = cat2, y = Value, fill = cat2)) +
    geom_col() +
    #coord_polar("y", start = 0) +
    labs(title = trait) +
    theme_void()
  if (interactive_g) g <- ggplotly(g)
  g
}






















































