#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("mass_covid/helpers.R")
source("build_data.R")

source("mass_covid/modules/traitExplorer_module.R")
source("mass_covid/modules/comparisonGraphModule.R")
source("mass_covid/modules/groupChart_module.R")
source("mass_covid/modules/groupRatioChart_module.R")

ui <- dashboardPage(
    dashboardHeader(
        titleWidth = 350,
        title = "Massachusetts Covid19 Explorer" 
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Quick Facts", tabName = "quick_facts", icon = icon("dashboard")),
            menuItem("State Level Explorer", tabName = "state_explorer", icon = icon("dashboard")),
            menuItem("County Level Explorer", tabName = "county_comparison", icon = icon("dashboard")),
            menuItem("City/Town Explorer", tabName = "city_town", icon = icon("dashboard")),
            menuItem("Age Group Explorer", tabName = "age_comparison", icon = icon("dashboard")),
            menuItem("Testing Data", tabName = "testing_data", icon = icon("dashboard")),
            menuItem("Race_Ethnicity Data", tabName = "race_eth", icon = icon("dashboard")),
            menuItem("Sex Data", tabName = "sex", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
          tabItem(tabName = "quick_facts",
                  
                  p("All data from Massachusetts COVID-19 site."),
                  a(href = "https://www.mass.gov/info-details/covid-19-response-reporting", "Mass.gov"),
                  p("Made for personal use by: b u romano at me dot com")
          ),
          
            
            tabItem(tabName = "state_explorer",
                    traitExplorerUI("state_TE"),
                    h1("Hospitalization Data"),
                    traitExplorerUI("hospital_TE")
            ),
          

            tabItem(tabName = "county_comparison",
                    traitExplorer2UI("county_TE2"),
                    groupComparisonGraphUI("county_GCG"),
                    groupChartUI("county_GC"),
                    groupPercentageGraphUI("countyGPG")
            ),
            
            tabItem(tabName = "age_comparison",
                    traitExplorer2UI("age_TE2"),
                    groupComparisonGraphUI("age_GCG"),
                    groupChartUI("age_GC"),
                    groupPercentageGraphUI("ageGPG")
            ),
            
            tabItem(tabName = "testing_data",
                    traitExplorerUI("testing_TE")
            ),
            
            tabItem(tabName = "city_town",
                    traitExplorer2UI("town_TE2"),
                    groupComparisonGraphUI("town_GCG"),
                    fluidRow(
                      box(width = 12, title = "Town Cumulative Positive", solidHeader = TRUE, status = "primary",
                          DT::dataTableOutput("towns_DT_table")
                          )
                    )
                    ),
            
            tabItem(tabName = "race_eth",
                    h3("Note: About 40% of the cumulative data for Positive and Deaths  and 30% of everHospitalized were not classified."),
                    groupChartUI("race_GC"),
                    traitExplorer2UI("race_TE2"),
                    groupComparisonGraphUI("race_GCG"),
                    groupPercentageGraphUI("raceGPG")
                    ),
 tabItem(tabName = "sex",
         groupChartUI("sex_GC"),
         traitExplorer2UI("sex_TE2"),
         groupComparisonGraphUI("sex_GCG")
 )
        )
    ) 
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  callModule(traitExplorer, id = "state_TE", state_data)
  callModule(traitExplorer, id = "hospital_TE", hospitalization_data)
  callModule(traitExplorer, id = "testing_TE", testing_data)
  callModule(traitExplorer2, id = "race_TE2", race_eth_data)
  callModule(traitExplorer2, id = "sex_TE2", filter(sex_data, Group != "Unknown"))
  callModule(traitExplorer2, id = "county_TE2", county)
  callModule(traitExplorer2, id = "age_TE2", age_data)
  callModule(traitExplorer2, id = "town_TE2", town_data)
  
  callModule(groupComparisonGraph, id = "county_GCG", county, county_populations, starting_comparison ="Middlesex")
  callModule(groupComparisonGraph, id = "age_GCG", age_data, age_populations)
  callModule(groupComparisonGraph, id = "town_GCG", town_data, town_population, starting_comparison = c("State Total", "Winchester"))
  callModule(groupComparisonGraph, id = "race_GCG", race_eth_data, race_population)
  callModule(groupComparisonGraph, id = "sex_GCG", filter(sex_data, Group != "Unknown"), sex_population, starting_comparison = c("Male", "Female"))
  
  callModule(groupChart, id = "age_GC", latest_age_data, age_populations)
  callModule(groupChart, id = "county_GC", latest_county_data, county_populations)
  callModule(groupChart, id = "race_GC", filter(latest_race_data, Group != "Unknown/Missing"), race_population)
  callModule(groupChart, id = "sex_GC", filter(sex_data, Group != "Unknown"), sex_population)
  
  callModule(groupPercentageGraph, id = "ageGPG", filter(latest_age_data, Trait %in% c("Cases", "Hospitalized", "Deaths")), age_populations)
  callModule(groupPercentageGraph, id = "countyGPG", filter(latest_county_data, Trait %in% c("Count", "ActiveApproximation", "Deaths")), county_populations)
  callModule(groupPercentageGraph, id = "raceGPG", filter(latest_race_data, Trait %in% c("Positive", "everHospitalized", "Deaths")), race_population)
  

    output$n_day_ave_html <- renderUI({
        box(width = 9, title = paste("Avergage of last", input$n_days, "days"), solidHeader = TRUE, status = "primary",
            h3(n_day_ave(reported_cases, latest_date, "TotalCases", input$n_days, 0), " reported positive cases per day."),
            h3(n_day_ave(testing_data, latest_date, "Molecular Total", input$n_days, 0), " tests per day, with a ", 
               round(n_day_ave(testing_data, latest_date, "MolecularPositiveNewTotal", input$n_days, 0)/n_day_ave(testing_data, latest_date, "Molecular Total", input$n_days, 0)*100,1),
               "% postive rate."),
            h3(n_day_ave(reported_deaths, latest_date, "TotalReportedDeaths", input$n_days, 0), " reported deaths per day."),
            h3(n_day_ave(hospitalization_data, latest_date - 1, "Total_number_of_COVID_patients_in_hospital_today", input$n_days, 0), " change in number of hospitalized patients per day."),
            h3(n_day_ave(hospitalization_data, latest_date - 1, "ICU", input$n_days, 0), " change in number of ICU patients per day."),
            h3(round(n_day_growth(reported_cases, latest_date, "TotalCases", input$n_days),5), ": growth factor for the total amount of positive tests -- corresponding to a doubling rate of ",
               round(doubling_time(n_day_growth(reported_cases, latest_date, "TotalCases", input$n_days)), 1), " days.")
            
        )
    })
    

    ## city town tab
    output$towns_DT_table <- DT::renderDataTable(
      DT::datatable(towns_DT)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
