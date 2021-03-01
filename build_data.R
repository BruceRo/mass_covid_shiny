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


##
update_data()
death_by_date <- make_death_by_date()
reported_cases <- make_reported_cases()
reported_deaths <- make_reported_deaths()

state_data <- rbind(death_by_date, reported_cases, reported_deaths)

hospitalization_from_hospitals <- make_hospitalization_from_hospitals()

county_daily <- make_county_daily()
latest_county_data <- county_daily %>% filter(Date == max(county_daily$Date), Group != "Unknown", Group != "Dukes and Nantucket")
#print(bar_chart("Race_Ethnicity", "Positive", TRUE, FALSE))


## Data input

# county <- read_csv("mass_raw_data/County.csv")
# county <- county %>% 
#   mutate(Date = mdy(Date)) %>% 
#   rename(Positive = Count,
#          cat2 = County) %>% 
#   group_by(cat2) %>% 
#   mutate(PositiveDaily = cumulative_to_daily(Positive),
#          DeathsDaily = cumulative_to_daily(Deaths))
# county$cat1 <- "County"
# county <- county %>% 
#   pivot_longer(c(Positive, PositiveDaily, Deaths, DeathsDaily), names_to = "Trait", values_to = "Value")
# 
# county_list <- unique(county$cat2)

# testing_data <- read_csv("mass_raw_data/Testing2.csv")
# testing_data <- testing_data %>% 
#   mutate(Date = mdy(`Date`)) %>% 
#   rename(Tests = Total,
#          TestsDaily = New)
# testing_data <- left_join(testing_data, select(filter(cases, Trait == "PositiveDaily"),Date, Value), by = "Date")
# testing_data <- testing_data %>% mutate(
#   Proportion_positive = Value/TestsDaily
# )
# testing_data$Value = NULL
# testing_data$cat1 <- "state"
# testing_data$cat2 <- "Massachusetts"
# testing_data <- testing_data %>% 
#   pivot_longer(c(Tests, TestsDaily, Proportion_positive), names_to = "Trait", values_to = "Value")

# testing_data_byDate <- read_csv("mass_raw_data/TestingByDate.csv")
# testing_data_byDate <- testing_data_byDate %>% 
#   mutate(Date = mdy(`Date`)) %>% 
#   rename(TestsByDate = Total,
#          TestsByDateDaily = New,
#          PositiveByDateDaily = Positive,
#          MissingDaily = Missing) %>% 
#   mutate(PositiveByDate = cumsum(PositiveByDateDaily),
#          Proportion_positiveByDate = PositiveByDateDaily/TestsByDateDaily)
# testing_data_byDate$cat1 <- "state"
# testing_data_byDate$cat2 <- "Massachusetts"
# testing_data_byDate <- testing_data_byDate %>% 
#   pivot_longer(c(TestsByDate, TestsByDateDaily, PositiveByDate,
#                  PositiveByDateDaily, MissingDaily, Proportion_positiveByDate), names_to = "Trait", values_to = "Value")


# age_data <- read_csv("mass_raw_data/Age.csv")
# age_data <- age_data %>% 
#   mutate(Date = mdy(Date)) %>% 
#   rename(Positive = Cases,
#          cat2 = Age) %>% 
#   group_by(cat2) %>% 
#   mutate(PositiveDaily = cumulative_to_daily(Positive),
#          DeathsDaily = cumulative_to_daily(Deaths),
#          HosipitalizedDaily = cumulative_to_daily(Hospitalized))
# age_data$cat1 <- "AgeGroup"
# age_data <- age_data %>% 
#   pivot_longer(c(Positive, PositiveDaily, Hospitalized, HosipitalizedDaily, Deaths, DeathsDaily), names_to = "Trait", values_to = "Value")

# reported_deaths <- read_csv("mass_raw_data/DeathsReported.csv")
# reported_deaths <- reported_deaths %>% 
#   mutate(Date = mdy(Date)) %>% 
#   rename(reportedDeaths = Deaths,
#     reportedDeathsDaily = New)
# reported_deaths$cat1 <- "state"
# reported_deaths$cat2 <- "Massachusetts"
# reported_deaths <- reported_deaths %>% 
#   pivot_longer(c(reportedDeaths, reportedDeathsDaily), names_to = "Trait", values_to = "Value")

# hospitalization_data <- read_csv("mass_raw_data/Hospitalization\ from\ Hospitals.csv")
# hospitalization_data <- hospitalization_data %>% 
#   mutate(Date = mdy(Date)) %>% 
#   rename(inHospital = `Total number of COVID patients in hospital today`,
#          inHospitalDaily = `Net new hospitalizations`)
# hospitalization_data$`5 day average of net new hospitalizations` <- NULL
# hospitalization_data$cat1 <- "state"
# hospitalization_data$cat2 <- "Massachusetts"
# hospitalization_data <- hospitalization_data %>% 
#   pivot_longer(c(inHospital, inHospitalDaily, ICU), names_to = "Trait", values_to = "Value")

# town_data <- build_town_data()
# town_data <- town_data %>% 
#   rename(cat2 = city_town,
#          Positive = count) %>% 
#   group_by(cat2) %>% 
#   mutate(PositiveWeekly = cumulative_to_daily(Positive))
# town_data$cat1 <- "city_town"
# town_data <- town_data %>% 
#   pivot_longer(c(Positive, PositiveWeekly), names_to = "Trait", values_to = "Value")

# race_eth_data <- read_csv("mass_raw_data/RaceEthnicity.csv")
# race_eth_data <- race_eth_data %>% 
#   mutate(Date = mdy(Date)) %>% 
#   rename(cat2 = `Race/Ethnicity`,
#          Positive = `All Cases`,
#          everHospitalized = `Ever Hospitaltized`) %>% 
#   group_by(cat2) %>% 
#   mutate(PositiveDaily = cumulative_to_daily(Positive),
#          DeathsDaily = cumulative_to_daily(Deaths))
# race_eth_data$cat1 <- "Race_Ethnicity"
# race_eth_data <- race_eth_data %>% 
#   pivot_longer(c(Positive, PositiveDaily, Deaths, DeathsDaily, everHospitalized), names_to = "Trait", values_to = "Value")

# sex_data <- read_csv("mass_raw_data/Sex.csv")
# sex_data <- sex_data %>% 
#   mutate(Date = mdy(Date)) %>% 
#   pivot_longer(c(Male, Female, Unknown), names_to = "cat2", values_to = "Positive") %>% 
#   group_by(cat2) %>% 
#   mutate(PositiveDaily = cumulative_to_daily(Positive)) %>% 
#   pivot_longer(c(Positive, PositiveDaily), names_to = "Trait", values_to = "Value" )
# sex_data$cat1 <-  "sex"

# all_data <- bind_rows(ma_deathByDate,
#                       cases,
#                       county,
#                       testing_data,
#                       testing_data_byDate,
#                       age_data,
#                       reported_deaths,
#                       hospitalization_data,
#                       town_data,
#                       race_eth_data,
#                       sex_data)


#ageGroup_list <- unique(age_data$Age)
county_populations <- read_csv("mass_covid/populations/countyPopulations.csv")
county_populations <- county_populations %>% rename("Group" = "County")

