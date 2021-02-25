
# download new data

# make cases dataframe

reported_cases  <- read_csv("mass_covid/data/CasesThroughMay.csv")

reported_cases <- reported_cases %>% 
  mutate(Date = mdy(Date)) %>% 
  select(Date, Cases, New) %>% 
  rename("Positive Total" = Cases) %>% 
  rename("Positive New" = New) 
reported_cases$`Probable Total`<-  NA
reported_cases$`Probable New` <-  NA
reported_cases$`Estimated active cases` <-  NA

temp <- readxl::read_xlsx("mass_covid/covid-19-raw-data-2-24-2021.xlsx", 
                          sheet = "Cases (Report Date)")

temp <- temp %>% 
  mutate(Date = ymd(Date))

reported_cases <- rbind(reported_cases, temp)
rm(temp)

reported_cases <- reported_cases %>% 
  mutate(Cases_last_two_weeks = cumulative_to_14_day(`Positive Total`))

reported_cases <- reported_cases %>% 
  pivot_longer(names(reported_cases)[names(reported_cases) != "Date"], names_to = "Trait", values_to = "Value")


