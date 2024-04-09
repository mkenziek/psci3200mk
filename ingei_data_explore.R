
library(foreign)
library(magrittr)
library(rio)
library(ggplot2)
library(tidyverse)


# load data 

election_returns <- read.csv("/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk/Data/aymu1970-on.coalSplit.csv")

homicide_rates <- readr::read_delim("/Users/kenziekmac/Dropbox/Mac/Documents/GitHub/psci3200mk/Data/mexico-muni-month-homicide-rates-2000-2021.csv", delim = "|")


# data cleaning 

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


         
# For Loop to find the first and second place parties for each election  

# Initialize lists to store results
highest_col <- vector("list", length = nrow(election_returns))
second_highest_col <- vector("list", length = nrow(election_returns))

# Loop through each row
for (i in 1:nrow(election_returns)) {
  # Get the row values excluding non-numeric columns
  row_values <- as.numeric(unlist(election_returns[i, grep("^v", names(election_returns))]))
  
  # Find the index of the highest value
  highest_index <- which.max(row_values)
  
  # Set the highest value to -Inf so it's excluded when finding the second highest
  row_values[highest_index] <- -Inf
  
  # Find the index of the second highest value
  second_highest_index <- which.max(row_values)
  
  # Get the column names corresponding to the highest and second-highest values
  highest_col[[i]] <- names(election_returns)[grep("^v", names(election_returns))][highest_index]
  second_highest_col[[i]] <- names(election_returns)[grep("^v", names(election_returns))][second_highest_index]
}

# Convert lists to character vectors
highest_col <- sapply(highest_col, function(x) ifelse(is.null(x), NA, x))
second_highest_col <- sapply(second_highest_col, function(x) ifelse(is.null(x), NA, x))

# Add the results to the original dataset
election_returns$highest_col <- highest_col
election_returns$second_highest_col <- second_highest_col




# For Loop to Create Raw Votes Columns 

# Initialize a list to store selected columns
selected_columns <- vector("list", length = nrow(election_returns))

# Loop through each row
for (i in 1:nrow(election_returns)) {
  # Get the column names for the current row
  highest_col_name <- election_returns$highest_col[i]
  second_highest_col_name <- election_returns$second_highest_col[i]
  
  # Select columns based on the column names
  first_place_raw_votes <- election_returns[[highest_col_name]][i]
  second_place_raw_votes <- election_returns[[second_highest_col_name]][i]
  
  
  # Combine the selected columns into a data frame
  selected_columns[[i]] <- cbind(first_place_raw_votes, second_place_raw_votes)
}

# Convert the list to a data frame
selected_raw_votes_df <- as.data.frame(do.call(rbind, selected_columns)) %>%
                       cbind(election_returns[,c(1,2,55,59,58)]) %>%
                       select(3,4,1,2,5,6,7)




# For loop to create party columns 

# Initialize a list to store selected columns
selected_columns <- vector("list", length = nrow(election_returns))

# Loop through each row
for (i in 1:nrow(election_returns)) {
  # Get the party names for the current row
  highest_col_party <- paste0("l", substr(election_returns[i, "highest_col"], 2, 3))
  second_highest_col_party <- paste0("l", substr(election_returns[i, "second_highest_col"], 2, 3))
  
  # Select columns based on the party names
  first_place_party <- election_returns[i, highest_col_party]
  second_place_party <- election_returns[i, second_highest_col_party]
  
  # Combine the selected columns into a data frame
  selected_columns[[i]] <- cbind(first_place_party, second_place_party)
}

# Convert the list to a data frame
selected_parties_df <- as.data.frame(do.call(rbind, selected_columns))


election_returns_cleaned <- cbind(selected_raw_votes_df, selected_parties_df)

# Create Independent Variables 


# municipal alternation index variable 


election_returns_cleaned$year <- as.numeric(election_returns_cleaned$year)


# Function to count cumulative alternations in party power for each municipality and year
count_alternations <- function(data) {
  # Check if there's only one party or no data
  if (nrow(unique(data[, c("year", "first_place_party")])) <= 1) {
    return(rep(0, nrow(data)))
  }
  
  # Initialize alternation score
  alternation_score <- numeric(nrow(data))
  
  # Compute cumulative alternation score
  for (i in 2:nrow(data)) {
    if (!is.na(data$first_place_party[i]) && !is.na(data$first_place_party[i - 1]) &&
        data$first_place_party[i] != data$first_place_party[i - 1]) {
      alternation_score[i] <- alternation_score[i - 1] + 1
    } else {
      alternation_score[i] <- alternation_score[i - 1]
    }
  }
  
  return(alternation_score)
}

# Calculate cumulative alternation index for each municipality and year
election_returns_cleaned <- election_returns_cleaned %>%
  group_by(ent_mun) %>%
  mutate(alternation_index = count_alternations(cur_data())) %>%
  ungroup()
 

# PAN power index variable 

# Function to count consecutive years in power for the "pan" party for each municipality
count_years_in_power <- function(data) {
  # Initialize years in power count
  years_in_power <- rep(0, nrow(data))
  
  # Compute consecutive years in power for the "pan" party
  for (i in 1:nrow(data)) {
    if (!is.na(data$first_place_party[i]) && data$first_place_party[i] == "pan") {
      years_in_power[i] <- ifelse(i == 1, 1, years_in_power[i - 1] + 1)
    }
  }
  
  return(years_in_power)
}

# Calculate years in power index for the "pan" party for each municipality

election_returns_cleaned <- election_returns_cleaned %>%
  group_by(ent_mun) %>%
  mutate(pan_consec_yrs_in_pwr = count_years_in_power(cur_data())) %>%
  ungroup()


# victory of the PAN (National Action Party) candidate variable 

# first filter down to election where pan or pri was first or second place party 


pri_winners <- election_returns_cleaned %>%
  filter(first_place_party == "pri") %>%
  filter(second_place_party == "pan") %>%
  rename( "pri_raw_votes" = "first_place_raw_votes",
          "pan_raw_votes" = "second_place_raw_votes") %>%
  select(-c(first_place_party, second_place_party))

# pan winners only 


pan_winners <- election_returns_cleaned %>%
  filter(first_place_party == "pan") %>%
  filter(second_place_party == "pri") %>%
  rename( "pan_raw_votes" = "first_place_raw_votes",
          "pri_raw_votes" = "second_place_raw_votes") %>%
  select(-c(first_place_party, second_place_party))

# close election dataset 

pan_pri_close_elections <- rbind(pri_winners, pan_winners) %>%
                     mutate( pri_vote_share = pri_raw_votes/efec,
                             pan_vote_share = pan_raw_votes/efec,
                             pan_margin_of_victory = pan_vote_share - pri_vote_share) %>%
                    filter(pan_margin_of_victory >= -0.05 & pan_margin_of_victory <= 0.05)

pan_pri_elections <- rbind(pri_winners, pan_winners) %>%
  mutate( pri_vote_share = pri_raw_votes/efec,
          pan_vote_share = pan_raw_votes/efec,
          pan_margin_of_victory = pan_vote_share - pri_vote_share) 



# join elections and homicide data 

elections_hom <- full_join(homicide_rates,election_returns_cleaned, by = c("ent_mun", "month", "year")) %>%
                  filter( year >= 2001)

elections_hom_pan <- full_join(homicide_rates,pan_pri_elections, by = c("ent_mun", "month", "year"))

elections_hom_close <- full_join(homicide_rates,pan_pri_elections, by = c("ent_mun", "month", "year"))




# Convert 'year' and 'month' to a Date object
elections_hom$date <- as.Date(paste(elections_hom$year, elections_hom$month, "01", sep = "-"))

elections_hom <- elections_hom %>%
  filter(date >= as.Date("2001-08-01")) %>%
  mutate( election_year = ifelse(is.na(pan_consec_yrs_in_pwr), 0, 1)) %>%
  filter(year < 2020)


# Define a function to calculate post-election average homicide rate
calculate_post_election_avg <- function(data, election_date, next_election_date, municipality) {
  post_election_data <- data %>%
    filter(ent_mun == municipality) %>%
    filter(date < as.Date(next_election_date)) %>%
    filter(date != as.Date(election_date))
  post_election_avg_rate <- mean(post_election_data$homicide_rate, na.rm = TRUE)
  return(post_election_avg_rate)
}




post_election_avg_rates <- numeric(nrow(elections_hom))


for (i in 1:5000) {
  election_date <- elections_hom$date[i]
  mun <- elections_hom$ent_mun[i]
  next_election_date <- election_date + years(3)
  post_election_avg_rates[i] <- calculate_post_election_avg(elections_hom, election_date, next_election_date, mun)
}



# Add the calculated post-election average homicide rates as a new column in the dataset
elections_hom$post_election_avg_rate <- post_election_avg_rates

elections_hom$post_election_avg_rate[is.na(elections_hom$pan_consec_yrs_in_pwr)] <- NA


2004 + 3

# View the updated dataset
head(elections_hom)

# View the updated dataset
test <- elections_hom[2:36,] %>%
        summarize(post_elec_rate = mean(homicide_rate))

# View the updated dataset
head(elections_hom)
# example of how homicide rate is calc. 

(3 / ((1852 / 365.25) * 31)) * 100000

# Dell's RDD method 

elections_hom_close %>%
   mutate(`Party Won` = ifelse(pan_margin_of_victory < 0, "PRI", "PAN")) %>% 
  filter(!is.na(`Party Won`)) %>% 
  ggplot(aes(x = pan_margin_of_victory, y = homicide_rate, group=`Party Won`, color=`Party Won`)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylim(0,25)


# Alternative RDD method 

elections_hom_pan %>%
  mutate(`Party Won` = ifelse(pan_vote_share < .5, "PRI", "PAN")) %>% 
  filter(!is.na(`Party Won`)) %>% 
  filter(pan_vote_share >= .4 & pan_vote_share <= .6) %>% 
  ggplot(aes(x = pan_vote_share, y = homicide_rate, group=`Party Won`, color=`Party Won`)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylim(0,25)







