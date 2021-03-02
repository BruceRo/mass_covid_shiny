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
            menuItem("Testing Data", tabName = "testing_data", icon = icon("dashboard"))#,
#            menuItem("Race_Ethnicity Data", tabName = "race_eth", icon = icon("dashboard")),
#            menuItem("Sex Data", tabName = "sex", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
          tabItem(tabName = "quick_facts",
                  fluidRow(
                              box(width = 12, title = paste("Latest numbers as of", latest_date), solidHeader = TRUE, status = "primary",
                                  h3(get_value_by_date(reported_cases, latest_date, "Positive New"), "new positive cases of COVID-19 in Massachusetts. A change of ",
                                     get_value_by_date(reported_cases, latest_date, "Positive New")-get_value_by_date(reported_cases, latest_date-1, "Positive New"),
                                     " from the previous day."),
                                  h3(get_value_by_date(reported_cases, latest_date, "Estimated active cases"), "estimated active cases of COVID-19 in Massachusetts. Compared to  ",
                                     get_value_by_date(reported_cases, latest_date-7, "Estimated active cases"),
                                     " from a week ago."),
                                  h3(get_value_by_date(reported_deaths, latest_date, "DeathsConfNew"), " deaths have been reported for a total of ",
                                     h3(get_value_by_date(reported_deaths, latest_date, "DeathsConfTotal"),
                                     " .")),
                                  h3(get_value_by_date(reported_deaths, latest_date, "Confirmed_Reported_Deaths_last_two_weeks"), " deaths in last two weeks. Compared to ",
                                     get_value_by_date(reported_deaths, latest_date-7, "Confirmed_Reported_Deaths_last_two_weeks"),
                                     " from a week ago."),
                                  h3(get_value_by_date(hospitalization_from_hospitals, latest_date-1, "Total number of COVID patients in hospital today"), 
                                     " hospitalized patients (as of ", latest_date-1, "). A change of ",
                                     get_value_by_date(hospitalization_from_hospitals, latest_date-1, "Net new number of COVID patients in hospital today"), 
                                     "from the previous day."),
                                  h3(get_value_by_date(hospitalization_from_hospitals, latest_date-1, "ICU"), " ICU patients (as of ", latest_date-1, "). A change of ",
                                     get_value_by_date(hospitalization_from_hospitals, latest_date-1, "Net New number ICU"), "from the previous day."),
                                  h3(get_value_by_date(hospitalization_from_hospitals, latest_date-1, "7 day average of COVID hospitalizations"), 
                                     " for seven day average of COVID hospitalizations. Compared to ",
                                     get_value_by_date(hospitalization_from_hospitals, latest_date-8, "7 day average of COVID hospitalizations"),
                                     " from a week ago.")
                              )
                            ),
                  
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
                    h1("All values are for a two week period."),
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
                      box(width = 12, title = "Town Latest Data", solidHeader = TRUE, status = "primary",
                          DT::dataTableOutput("towns_DT_table")
                          )
                    )
                    )#,
            
            # tabItem(tabName = "race_eth",
            #         h3("Note: About 40% of the cumulative data for Positive and Deaths  and 30% of everHospitalized were not classified."),
            #         groupChartUI("race_GC"),
            #         traitExplorer2UI("race_TE2"),
            #         groupComparisonGraphUI("race_GCG"),
            #         groupPercentageGraphUI("raceGPG")
            #         ),
 # tabItem(tabName = "sex",
 #         groupChartUI("sex_GC"),
 #         traitExplorer2UI("sex_TE2"),
 #         groupComparisonGraphUI("sex_GCG")
 # )
        )
    ) 
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  callModule(traitExplorer, id = "state_TE", state_data)
  callModule(traitExplorer, id = "hospital_TE", hospitalization_from_hospitals)
  callModule(traitExplorer, id = "testing_TE", testing_data)
  callModule(traitExplorer2, id = "race_TE2", race_eth_data)
  callModule(traitExplorer2, id = "sex_TE2", filter(sex_data, Group != "Unknown"))
  callModule(traitExplorer2, id = "county_TE2", county_daily)
  callModule(traitExplorer2, id = "age_TE2", age_data)
  callModule(traitExplorer2, id = "town_TE2", weekly_city_town)
  
  callModule(groupComparisonGraph, id = "county_GCG", county_daily, county_populations, starting_comparison ="Middlesex")
  callModule(groupComparisonGraph, id = "age_GCG", age_data, age_populations)
  callModule(groupComparisonGraph, id = "town_GCG", weekly_city_town, town_populations, starting_comparison = c("Arlington", "Winchester"))
  #callModule(groupComparisonGraph, id = "race_GCG", race_eth_data, race_population)
  #callModule(groupComparisonGraph, id = "sex_GCG", filter(sex_data, Group != "Unknown"), sex_population, starting_comparison = c("Male", "Female"))
  
  callModule(groupChart, id = "age_GC", latest_age_data, age_populations)
  callModule(groupChart, id = "county_GC", latest_county_data, county_populations)
  #callModule(groupChart, id = "race_GC", filter(latest_race_data, Group != "Unknown/Missing"), race_population)
  #callModule(groupChart, id = "sex_GC", filter(sex_data, Group != "Unknown"), sex_population)
  
  callModule(groupPercentageGraph, id = "ageGPG", latest_age_data, age_populations)
  callModule(groupPercentageGraph, id = "countyGPG", 
             filter(latest_county_data, Trait %in% c("Total Confirmed Cases", "cases_last_two_weeks", "Total Probable and Confirmed Deaths")), 
             county_populations)
  #callModule(groupPercentageGraph, id = "raceGPG", filter(latest_race_data, Trait %in% c("Positive", "everHospitalized", "Deaths")), race_population)
  

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
