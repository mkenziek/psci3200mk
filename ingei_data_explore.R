
library(foreign)
library(tidyverse)
library(magrittr)

onUser<-function(x){
  user<-Sys.info()["user"]
  onD<-grepl(x,user,ignore.case=TRUE)
  return(onD)
}

if(onUser("kenziekmac")){
  setwd("~/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk")
  figures<-"~/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk/Figures"
  tables<-"~/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk/Tables"
}



election_returns <- read.csv("Data/aymu1970-on.coalSplit.csv")

homicide_rates <- readr::read_delim("Data/mexico-muni-month-homicide-rates-2000-2021.csv", delim = "|")

# have leading zeros so all municipal level identifiers are 5 digits

head(unique(homicide_rates$ent_mun))

# does not have leading zeros 

head(unique(election_returns$inegi))

# let's add leading zeros to the election_returns data 

# Function to add leading zeros

add_leading_zeros <- function(x) {
  ifelse(nchar(x) == 4, paste0("0", x), as.character(x))
}

# Apply the function to the column

election_returns$inegi <- as.numeric(election_returns$inegi)

election_returns$inegi <- sapply(election_returns$inegi, add_leading_zeros) 


election_returns %<>% rename( "ent_mun" = "inegi")
  
# Check for municipal codes present in df1 but not in df2

# these are missing election returns/ newly created municipalities 

records_only_in_df1 <- anti_join(election_returns, homicide_rates, by = "ent_mun")

# drop these municipalities since they don't have corresponding homicide rates 

ent_mun_drops_er <- records_only_in_df1$ent_mun

# 34,634 rows at start 

election_returns %<>% filter(!is.na(ent_mun)) %>%
                      filter(! ent_mun %in% ent_mun_drops_er)

# 34,624 after, dropped 10 rows 


# Check for municipal codes present in df2 but not in df1

# these are missing homicide rates

records_only_in_df2 <- anti_join(homicide_rates, election_returns, by = "ent_mun")

# drop these municipalities since they don't have corresponding election returns 

ent_mun_drops_hr <- records_only_in_df2$ent_mun

# 651,288 rows at start 

homicide_rates %<>% filter(!is.na(ent_mun)) %>%
  filter(! ent_mun %in% ent_mun_drops_hr)

# 650,232 after, dropped 1056 rows

# pick rows we need

election_returns %<>% select(-c(1,3,5,8)) 

# break election date into month and year so we can merge on these variables to match monthly homicide rates

election_returns$date<- as.character(election_returns$date)
  
election_returns %<>% mutate( date = as.Date(election_returns$date, format = "%Y%m%d"),
                              year = format(date, "%Y"),
                              month = format(date, "%m"))

# pick rows we need, again

election_returns %<>% select(-c(1,4))

# filter years bc homicide data starts at 2000

election_returns %<>% filter(year >= 2000)

# add leading zeros to homicide month col.


add_leading_zeros_2 <- function(x) {
  ifelse(nchar(x) == 1, paste0("0", x), as.character(x))
}

# Apply the function to the column


homicide_rates$month <- as.numeric(homicide_rates$month)

homicide_rates$month <- sapply(homicide_rates$month, add_leading_zeros_2) 


# merge 

election_returns$year <- as.numeric(election_returns$year )

test <- full_join(homicide_rates,election_returns, by = c("ent_mun", "month", "year")) %>%
        filter(!is.na(mun))






