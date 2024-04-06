
library(foreign)
library(tidyverse)
library(magrittr)
library(rio)




election_returns <- read.csv("/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk/Data/aymu1970-on.coalSplit.csv")

homicide_rates <- readr::read_delim("/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk/Data/mexico-muni-month-homicide-rates-2000-2021.csv", delim = "|")


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


# v01, v02, … = raw vote for candidate 1, 2, etc.

# l01, l02, … = label of candidate 1's, 2's, … party or coalition.

# i want the data to have a col name pri_vote and show raw vote for pri candidate,
# right now the raw vote and party are seperate columns 

# Fix candidate columns 

typeof(election_returns$l25)
typeof(election_returns$l01)

election_returns %<>%
  mutate_at(vars(starts_with("l")), ~replace(., . == 0, NA)) %>%
  mutate_at(vars(starts_with("l")), as.character)


typeof(election_returns$l25)
typeof(election_returns$l01)



# drop all candidates and their votes that aren't number 1 or number 2

# Selecting only the desired columns

election_returns <- election_returns[, c(1,2,3,4,5,6,55,59,58)]%>% 
  filter(!is.na(l01) & !is.na(l02)) %>%
      filter( (l01 == "pri" |l01 == "pan") & (l02 == "pri" | l02 == "pan" )) # 12705 rows


# pri wins only 

pri_winners <- election_returns %>%
  filter(l01 == "pri") %>%
  rename( "pri_raw_votes" = "v01",
          "pan_raw_votes" = "v02") %>%
  select(-c(l01, l02))


# pan winners only 

pan_winners <- election_returns %>%
  filter(l01 == "pan") %>%
  rename( "pan_raw_votes" = "v01",
          "pri_raw_votes" = "v02") %>%
  select(-c(l01, l02))


election_returns <- rbind(pri_winners, pan_winners)


min(election_returns$year)

max(election_returns$year)

election_returns %<>% arrange(year) 

test <- election_returns%>%
  filter(year == 2013) %>%
  filter(l01 == "pan")

test_2 <- election_returns%>%
  filter(year == 2004) %>%
  filter(is.na(l01 ))


test_3 <- election_returns%>%
  filter(year == 2001)


# calc. vote shares


election_returns$year <- as.numeric(election_returns$year)

test <- full_join(homicide_rates,election_returns, by = c("ent_mun", "month", "year"))

# missing homicide rates 

sum(is.na(test$homicide_rate))

election_returns %<>% 
  mutate( pri_vote_share = pri_raw_votes/efec,
          pan_vote_share = pan_raw_votes/efec)


big_rates <- test %>% 
  filter (homicide_rate > 1500)

# example of how homicide rate is calc. 

(3 / ((1852 / 365.25) * 31)) * 100000

test %>%
  mutate(`Party Won` = ifelse(pan_vote_share < .5, "PRI", "PAN")) %>% 
  filter(!is.na(`Party Won`)) %>% 
  filter(pan_vote_share >= .4 & pan_vote_share <= .6) %>% 
  ggplot(aes(x = pan_vote_share, y = homicide_rate, group=`Party Won`, color=`Party Won`)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylim(0,25)


# take homicide rates in months after an election occurs 
# i would expect the main impact to be concentrated in x time after election 
# how long does it take them to occupy office - 6 months after ? 
# is there places where this relationship should be strong or weak?

# long-term power base? or entrenched power base 

# split up time so examine power base from 2000-2015 then analyze 2015-present 
# 
# I will need to calculate a vote share variable. I will do this in the following steps:
# Determine the total votes cast for the PAN party (PAN_votes) and the total votes cast for all other parties (other_votes). 
# Calculate the margin of victory or defeat for each election as the absolute difference between PAN_votes and other_votes.
# Define a threshold for what constitutes a "closely contested" election.
# Filter the elections where the margin of victory or defeat is within the defined threshold.



