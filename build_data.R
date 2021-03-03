# https://www.mass.gov/doc/covid-19-raw-data-february-25-2021/download
# download new data

update_data <- function(){
  # try download new data from current day, if not available steps back day
  # writes data to "today.xlsx"
  make_link_date <- function(a_date){
    month_list <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
    return(paste(month_list[month(a_date)], day(a_date), year(a_date), sep = "-"))
  }
  
  read_url <- function(link){
    out <- tryCatch(
      download.file(link, "today.xlsx"),
      warning = function(w){
        return("bad link")
      },
      error = function(e){
        #message(cond)
        return("bad link")
      }
    )
  }
  
  #link_table <- read_csv("dates_links.csv")
  last_update <- read_csv("last_update")[[1,1]]
  current_day <- today()
  link <- paste0("https://www.mass.gov/doc/covid-19-raw-data-", make_link_date(current_day) , "/download")
  if (current_day > last_update){
    print("in if loop")
    print(read_url(link)[[1]])
    while(read_url(link) == "bad link"){
      print("try download")
      current_day <- current_day - 1
      link <- paste0("https://www.mass.gov/doc/covid-19-raw-data-", make_link_date(current_day) , "/download")
    }
    #print("unzip")
    #unzip("today.zip", list = F, overwrite = TRUE, exdir = "mass_raw_data")
    write_csv(tibble(last_update = current_day), "last_update")
  }
}

# make cases dataframe
make_reported_cases <- function(){
  reported_cases  <- read_csv("mass_covid/data/CasesThroughMay.csv")
  
  reported_cases <- reported_cases %>% 
    mutate(Date = mdy(Date)) %>% 
    select(Date, Cases, New) %>% 
    rename("Positive Total" = Cases) %>% 
    rename("Positive New" = New) 
  reported_cases$`Probable Total`<-  NA
  reported_cases$`Probable New` <-  NA
  reported_cases$`Estimated active cases` <-  NA
  
  temp <- readxl::read_xlsx("today.xlsx", 
                            sheet = "Cases (Report Date)")
  
  temp <- temp %>% 
    mutate(Date = ymd(Date))
  
  reported_cases <- rbind(reported_cases, temp)
  rm(temp)
  
  reported_cases <- reported_cases %>% 
    mutate(Cases_last_two_weeks = cumulative_to_14_day(`Positive Total`))
  
  reported_cases <- reported_cases %>% 
    pivot_longer(names(reported_cases)[names(reported_cases) != "Date"], names_to = "Trait", values_to = "Value")
  
  return(reported_cases)
}

make_death_by_date <- function(){
  death_by_date <- readxl::read_xlsx("today.xlsx", 
                            sheet = "DateofDeath")
  death_by_date <- death_by_date %>% 
    mutate(Date = ymd(`Date of Death`),
           Death_by_date_last_two_weeks = cumulative_to_14_day(`Confirmed Total`))
  death_by_date$`Date of Death` = NULL
  death_by_date <- death_by_date %>% 
    pivot_longer(names(death_by_date)[names(death_by_date) != "Date"], names_to = "Trait", values_to = "Value")
  
  return(death_by_date)
}

make_reported_deaths <- function(){
  reported_deaths  <- read_csv("mass_covid/data/DeathsReportedThroughMay.csv")
  
  reported_deaths <- reported_deaths %>% 
    mutate(Date = mdy(Date)) %>% 
    #select(Date, Cases, New) %>% 
    rename(DeathsConfTotal = Deaths) %>% 
    rename(DeathsConfNew = New) 
  reported_deaths$DeathsProbTotal<-  NA
  reported_deaths$DeathsProbNew <-  NA

  temp <- readxl::read_xlsx("today.xlsx", 
                            sheet = "DeathsReported (Report Date)")
  
  temp <- temp %>% 
    mutate(Date = ymd(Date))
  
  reported_deaths <- rbind(reported_deaths, temp)
  rm(temp)
  
  reported_deaths <- reported_deaths %>% 
    mutate(Confirmed_Reported_Deaths_last_two_weeks = cumulative_to_14_day(DeathsConfTotal))
  
  reported_deaths <- reported_deaths %>% 
    pivot_longer(names(reported_deaths)[names(reported_deaths) != "Date"], names_to = "Trait", values_to = "Value")
  
  return(reported_deaths)
}

make_hospitalization_from_hospitals <- function(){
  hospitalization_from_hospitals <- readxl::read_xlsx("today.xlsx", 
                                                      sheet = "Hospitalization from Hospitals")
  hospitalization_from_hospitals <- hospitalization_from_hospitals %>% 
    mutate(Date = ymd(`Date`))
  hospitalization_from_hospitals <- hospitalization_from_hospitals %>% 
    pivot_longer(names(hospitalization_from_hospitals)[names(hospitalization_from_hospitals) != "Date"], names_to = "Trait", values_to = "Value")
  
  return(hospitalization_from_hospitals)
}

make_county_daily <- function(){
  county_daily <- readxl::read_xlsx("today.xlsx", 
                                    sheet = "County_Daily")
  county_daily <- county_daily %>% 
    mutate(Date = ymd(`Date`))
  county_daily <- county_daily %>% 
    group_by(County) %>% 
    mutate(cases_last_two_weeks = cumulative_to_14_day(`Total Confirmed Cases`))
  county_daily$cases_last_two_weeks[county_daily$Date < ymd("2020-9-3")] <- NA
  county_daily <- county_daily %>% 
    pivot_longer(names(county_daily)[names(county_daily) != "Date" & names(county_daily) != "County"], names_to = "Trait", values_to = "Value")
  county_daily <- county_daily %>% 
    rename("Group" = "County")
  
  return(county_daily)
}

make_weekly_city_town <- function(){
  weekly_city_town <- readxl::read_xlsx("today.xlsx", 
                                        sheet = "Weekly_City_Town")
  weekly_city_town$`Total Case Counts`[weekly_city_town$`Total Case Counts`=="<5"] <- NA
  weekly_city_town$`Two Week Case Counts`[weekly_city_town$`Two Week Case Counts`=="<5"] <- NA
  weekly_city_town$`Total Case Counts` <- as.numeric(weekly_city_town$`Total Case Counts`)
  weekly_city_town$`Two Week Case Counts` <- as.numeric(weekly_city_town$`Two Week Case Counts`)
  weekly_city_town$`Average Daily Rate` <- as.numeric(weekly_city_town$`Average Daily Rate`)
  weekly_city_town$`Percent Positivity` <- as.numeric(weekly_city_town$`Percent Positivity`)
  
  weekly_city_town <- weekly_city_town %>% 
    rename(Date = `Report Date`) %>% 
    mutate(Date = ymd(`Date`)) 
  
  weekly_city_town <- weekly_city_town %>% 
    select(`City/Town`, `Total Case Counts`, `Two Week Case Counts`,
           `Total Tests`, `Total Tests Last Two Weeks`, `Total Positive Tests`,
           Date, `Average Daily Rate`, `Percent Positivity`)
  weekly_city_town <- weekly_city_town %>% 
    rename("Average Daily Incidence Rate" = "Average Daily Rate",
           "Testing Percent Positivity" = "Percent Positivity")
  weekly_city_town <- weekly_city_town  %>%
    pivot_longer(names(weekly_city_town)[names(weekly_city_town) != "Date" & names(weekly_city_town) != "City/Town"], names_to = "Trait", values_to = "Value")
  weekly_city_town <- weekly_city_town %>% 
    rename("Group" = "City/Town")
  
  return(weekly_city_town)
}

make_town_DT <- function(){
  weekly_city_town <- readxl::read_xlsx("today.xlsx", 
                                        sheet = "Weekly_City_Town")
  weekly_city_town$`Total Case Counts`[weekly_city_town$`Total Case Counts`=="<5"] <- NA
  weekly_city_town$`Two Week Case Counts`[weekly_city_town$`Two Week Case Counts`=="<5"] <- NA
  weekly_city_town$`Total Case Counts` <- as.numeric(weekly_city_town$`Total Case Counts`)
  weekly_city_town$`Two Week Case Counts` <- as.numeric(weekly_city_town$`Two Week Case Counts`)
  weekly_city_town$`Average Daily Rate` <- as.numeric(weekly_city_town$`Average Daily Rate`)
  weekly_city_town$`Percent Positivity` <- as.numeric(weekly_city_town$`Percent Positivity`)
  
  weekly_city_town <- weekly_city_town %>% 
    rename(Date = `Report Date`) %>% 
    mutate(Date = ymd(`Date`)) %>% 
    filter(`City/Town` != "Unknown town")
  towns_DT <- weekly_city_town %>% 
    filter(Date == max(weekly_city_town$Date)) %>% 
    select("City/Town", "Total Case Counts", "Two Week Case Counts", "Average Daily Rate", "Color","Change in Last Week","Percent Positivity") %>% 
    mutate("Average Daily Rate" = round(`Average Daily Rate`, 2), 
           "Percent Positivity" = round(`Percent Positivity`, 2)) %>% 
    arrange(desc(`Average Daily Rate`))
  towns_DT <- towns_DT %>% 
    rename("Average Daily Incidence Rate" = "Average Daily Rate",
           "Testing Percent Positivity" = "Percent Positivity")
  
  return(towns_DT)
}

# make_age_data <- function(){
#   cases_by_age <- readxl::read_xlsx("today.xlsx", 
#                                     sheet = "CasesbyAge")
#   cases_by_age <- cases_by_age %>%
#     mutate(Date = ymd(`Date`)) %>%
#     select(-`Average Age of Cases that were hospitalized`, -`Average age of deaths`,
#            -`Average daily incidence rate per 100,000 (last 14 days)`) 
#   cases_by_age <- cases_by_age %>%
#     pivot_longer(names(cases_by_age)[names(cases_by_age) != "Date"], names_to = "Group", values_to = "Value")
#   cases_by_age$Trait <- "Positive Case"
#   
#   return(cases_by_age)
# }

make_age_data <- function(){
  cases_by_age <- readxl::read_xlsx("today.xlsx", 
                                    sheet = "AgeLast2Weeks")
  cases_by_age <- cases_by_age %>%
    mutate(Date = ymd(`Date`)) %>%
    select(-`Start_Date`, -`End_Date`) %>% 
    rename("Group" = "Age")
  cases_by_age <- cases_by_age %>%
    pivot_longer(names(cases_by_age)[names(cases_by_age) != "Date" & names(cases_by_age) != "Group"], names_to = "Trait", values_to = "Value")
  cases_by_age <- cases_by_age %>% 
    filter(Group != "Unknown")

  return(cases_by_age)
}

make_testing_data <- function(){
  testing_data <- readxl::read_xlsx("today.xlsx", 
                                    sheet = "Testing2 (Report Date)")
  testing_data <- testing_data %>%
    mutate(Date = ymd(`Date`))
  testing_data <- testing_data %>%
    pivot_longer(names(testing_data)[names(testing_data) != "Date"], names_to = "Trait", values_to = "Value")
  
}

##
latest_date <- read_csv("last_update")[[1,1]]
update_data()
latest_date <- read_csv("last_update")[[1,1]]
death_by_date <- make_death_by_date()
reported_cases <- make_reported_cases()
reported_deaths <- make_reported_deaths()

state_data <- rbind(death_by_date, reported_cases, reported_deaths)

hospitalization_from_hospitals <- make_hospitalization_from_hospitals()

county_daily <- make_county_daily()
latest_county_data <- county_daily %>% filter(Date == max(county_daily$Date), Group != "Unknown", Group != "Dukes and Nantucket")

weekly_city_town <- make_weekly_city_town()
towns_DT <- make_town_DT()

age_data <- make_age_data()
latest_age_data <- age_data %>% filter(Date == max(age_data$Date))

testing_data <- make_testing_data()

county_populations <- read_csv("mass_covid/populations/countyPopulations.csv")
county_populations <- county_populations %>% rename("Group" = "County")

town_populations <- read_csv("mass_covid/populations/town_populations.csv")
town_populations <- town_populations %>% rename("Group" = "city_town")

age_populations <- read_csv("mass_covid/populations/age_group_population.csv")
age_populations <- age_populations %>% 
  filter(Age != "Unknown") %>% 
  rename("Group" = "Age")

