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

