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

comparison_plot <- function(df = all_data,  cat1_vec, cat2_vec, trait_vec, compare_cat, perCap = FALSE, title = "Title"){
  df <- df %>% 
    filter(cat1 %in% cat1_vec) %>% 
    filter(cat2 %in% cat2_vec) %>% 
    filter(Trait %in% trait_vec)
  ggplot(df, aes(x = Date, y = Value, color = !!sym(compare_cat))) +
    geom_line(alpha = 0.5) +
    geom_smooth( ) +
    labs(title = title) +
    theme_minimal()
}
#comparison_plot(df = all_data, cat1_vec = "state",cat2_vec = "Massachusetts", trait_vec = c("DeathsDaily", "PositiveDaily"), compare_cat = "Trait" )

interactive_plot <- function(cat1_sel, cat2_sel, trait_sel){
  df <- all_data %>% 
    filter(cat1 == cat1_sel,
           cat2 == cat2_sel,
           Trait == trait_sel)
  ggplotly(
    ggplot(df, aes(x = Date, y = Value)) +
      geom_point() +
      geom_smooth() +
      theme_minimal()
  )
}
#interactive_plot("County", "Middlesex", "PositiveDaily")

pie_chart <- function(cat1_sel, trait, remove_unknown = FALSE){
  df <- all_data %>% 
    filter(cat1 == cat1_sel,
           Trait == trait,
           Date == max(all_data$Date)
    )
  if (remove_unknown){
    df <- df %>% 
      filter(!(cat2 %in% c("Unknown", "Unknown1", "Unknown/Missing")))
  }
  g <- ggplot(df, aes(x = "", y = Value, fill = cat2)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    labs(title = trait) +
    theme_void()
}
#print(pie_chart("Race_Ethnicity", "Positive", TRUE))

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
#print(bar_chart("Race_Ethnicity", "Positive", TRUE, FALSE))


## Data input
ma_deathByDate <- read_csv("mass_raw_data/DateofDeath.csv")
ma_deathByDate <- ma_deathByDate %>% 
  rename(Date = `Date of Death`, 
         Deaths = `Running Total`,
         DeathsDaily = `New Deaths`) %>% 
  mutate(Date = mdy(Date))
ma_deathByDate$cat1 <- "state"
ma_deathByDate$cat2 <- "Massachusetts"
ma_deathByDate <- ma_deathByDate %>% 
  pivot_longer(c(Deaths, DeathsDaily), names_to = "Trait", values_to = "Value")

cases <- read_csv("mass_raw_data/Cases.csv")
cases <- cases %>% 
  select(Date, Cases, New) %>% 
  mutate(Date = mdy(Date),
         Active = cumulative_to_14_day(cases$Cases)) %>% 
  rename(Positive = Cases,
         PositiveDaily = New)
cases$cat1 <- "state"
cases$cat2 <- "Massachusetts"
cases <- cases %>% 
  pivot_longer(c(Positive, PositiveDaily, Active), names_to = "Trait", values_to = "Value")

county <- read_csv("mass_raw_data/County.csv")
county <- county %>% 
  mutate(Date = mdy(Date)) %>% 
  rename(Positive = Count,
         cat2 = County) %>% 
  group_by(cat2) %>% 
  mutate(PositiveDaily = cumulative_to_daily(Positive),
         DeathsDaily = cumulative_to_daily(Deaths))
county$cat1 <- "County"
county <- county %>% 
  pivot_longer(c(Positive, PositiveDaily, Deaths, DeathsDaily), names_to = "Trait", values_to = "Value")

county_list <- unique(county$cat2)

testing_data <- read_csv("mass_raw_data/Testing2.csv")
testing_data <- testing_data %>% 
  mutate(Date = mdy(`Date`)) %>% 
  rename(Tests = Total,
         TestsDaily = New)
testing_data <- left_join(testing_data, select(filter(cases, Trait == "PositiveDaily"),Date, Value), by = "Date")
testing_data <- testing_data %>% mutate(
  Proportion_positive = Value/TestsDaily
)
testing_data$Value = NULL
testing_data$cat1 <- "state"
testing_data$cat2 <- "Massachusetts"
testing_data <- testing_data %>% 
  pivot_longer(c(Tests, TestsDaily, Proportion_positive), names_to = "Trait", values_to = "Value")

testing_data_byDate <- read_csv("mass_raw_data/TestingByDate.csv")
testing_data_byDate <- testing_data_byDate %>% 
  mutate(Date = mdy(`Date`)) %>% 
  rename(TestsByDate = Total,
         TestsByDateDaily = New,
         PositiveByDateDaily = Positive,
         MissingDaily = Missing) %>% 
  mutate(PositiveByDate = cumsum(PositiveByDateDaily),
         Proportion_positiveByDate = PositiveByDateDaily/TestsByDateDaily)
testing_data_byDate$cat1 <- "state"
testing_data_byDate$cat2 <- "Massachusetts"
testing_data_byDate <- testing_data_byDate %>% 
  pivot_longer(c(TestsByDate, TestsByDateDaily, PositiveByDate,
                 PositiveByDateDaily, MissingDaily, Proportion_positiveByDate), names_to = "Trait", values_to = "Value")


age_data <- read_csv("mass_raw_data/Age.csv")
age_data <- age_data %>% 
  mutate(Date = mdy(Date)) %>% 
  rename(Positive = Cases,
         cat2 = Age) %>% 
  group_by(cat2) %>% 
  mutate(PositiveDaily = cumulative_to_daily(Positive),
         DeathsDaily = cumulative_to_daily(Deaths),
         HosipitalizedDaily = cumulative_to_daily(Hospitalized))
age_data$cat1 <- "AgeGroup"
age_data <- age_data %>% 
  pivot_longer(c(Positive, PositiveDaily, Hospitalized, HosipitalizedDaily, Deaths, DeathsDaily), names_to = "Trait", values_to = "Value")

reported_deaths <- read_csv("mass_raw_data/DeathsReported.csv")
reported_deaths <- reported_deaths %>% 
  mutate(Date = mdy(Date)) %>% 
  rename(reportedDeaths = Deaths,
    reportedDeathsDaily = New)
reported_deaths$cat1 <- "state"
reported_deaths$cat2 <- "Massachusetts"
reported_deaths <- reported_deaths %>% 
  pivot_longer(c(reportedDeaths, reportedDeathsDaily), names_to = "Trait", values_to = "Value")

hospitalization_data <- read_csv("mass_raw_data/Hospitalization\ from\ Hospitals.csv")
hospitalization_data <- hospitalization_data %>% 
  mutate(Date = mdy(Date)) %>% 
  rename(inHospital = `Total number of COVID patients in hospital today`,
         inHospitalDaily = `Net new hospitalizations`)
hospitalization_data$`5 day average of net new hospitalizations` <- NULL
hospitalization_data$cat1 <- "state"
hospitalization_data$cat2 <- "Massachusetts"
hospitalization_data <- hospitalization_data %>% 
  pivot_longer(c(inHospital, inHospitalDaily, ICU), names_to = "Trait", values_to = "Value")

town_data <- build_town_data()
town_data <- town_data %>% 
  rename(cat2 = city_town,
         Positive = count) %>% 
  group_by(cat2) %>% 
  mutate(PositiveWeekly = cumulative_to_daily(Positive))
town_data$cat1 <- "city_town"
town_data <- town_data %>% 
  pivot_longer(c(Positive, PositiveWeekly), names_to = "Trait", values_to = "Value")

race_eth_data <- read_csv("mass_raw_data/RaceEthnicity.csv")
race_eth_data <- race_eth_data %>% 
  mutate(Date = mdy(Date)) %>% 
  rename(cat2 = `Race/Ethnicity`,
         Positive = `All Cases`,
         everHospitalized = `Ever Hospitaltized`) %>% 
  group_by(cat2) %>% 
  mutate(PositiveDaily = cumulative_to_daily(Positive),
         DeathsDaily = cumulative_to_daily(Deaths))
race_eth_data$cat1 <- "Race_Ethnicity"
race_eth_data <- race_eth_data %>% 
  pivot_longer(c(Positive, PositiveDaily, Deaths, DeathsDaily, everHospitalized), names_to = "Trait", values_to = "Value")

sex_data <- read_csv("mass_raw_data/Sex.csv")
sex_data <- sex_data %>% 
  mutate(Date = mdy(Date)) %>% 
  pivot_longer(c(Male, Female, Unknown), names_to = "cat2", values_to = "Positive") %>% 
  group_by(cat2) %>% 
  mutate(PositiveDaily = cumulative_to_daily(Positive)) %>% 
  pivot_longer(c(Positive, PositiveDaily), names_to = "Trait", values_to = "Value" )
sex_data$cat1 <-  "sex"

all_data <- bind_rows(ma_deathByDate,
                      cases,
                      county,
                      testing_data,
                      testing_data_byDate,
                      age_data,
                      reported_deaths,
                      hospitalization_data,
                      town_data,
                      race_eth_data,
                      sex_data)


ageGroup_list <- unique(age_data$Age)
county_populations <- read_csv("countyPopulations.csv")























































