
library(foreign)
library(tidyverse)
library(magrittr)
library(rio)




election_returns <- read.csv("/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk/Data/aymu1970-on.coalSplit.csv")

homicide_rates <- readr::read_delim("/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk/Data/mexico-muni-month-homicide-rates-2000-2021.csv", delim = "|")

acled <- import("/Users/kenziekmac/Downloads/LatinAmerica_2018-2024_Mar08.xlsx") %>%
         filter(COUNTRY == "Mexico")

unique(acled$SUB_EVENT_TYPE)

unique(acled$AC)

unique(acled$ACTOR1)

sum(acled$ACTOR2 == "Local Administrators")

blH <- filter(acled, grepl(" local administrators",TAGS))


unique(blH$ADMIN1)
mun_names <- unique(election_returns$mun)

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

test <- full_join(homicide_rates,election_returns, by = c("ent_mun", "month", "year"))

# take homicide rates in months after an election occurs 
# i would expect the main impact to be concentrated in x time after election 
# how long does it take them to occupy office - 6 months after ? 
# is there places where this relationship should be strong or weak?

# long-term power base? or entrenched power base 
# split up time so examine power base from 2000-2015 then analyze 2015-present 




