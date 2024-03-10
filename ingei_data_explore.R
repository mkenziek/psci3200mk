
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

# these are missing election returns 

records_only_in_df1 <- anti_join(election_returns, homicide_rates, by = "ent_mun")

records_only_in_df1 


# Check for municipal codes present in df2 but not in df1

# these are missing homicide rates

records_only_in_df2 <- anti_join(homicide_rates, election_returns, by = "ent_mun")
          

# next steps

# drop rows from anti_join

# drop ent_mun NAs


# calc. pan/pri vote variable 

# pick columns you need

# pick time frame 

# merge 






