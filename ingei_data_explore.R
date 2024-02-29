
library(foreign)
library(tidyverse)

mx_mortality_21 <- read.dbf("/Users/kenziekmac/Downloads/defunciones_base_datos_2021_dbf/defun21.dbf")

load("/Users/kenziekmac/Downloads/clea_lc_20220908_r/clea_lc_20220908.RData")

mx_low <- clea_lc_20220908 %>% 
  filter(ctr_n == "Mexico",
         yr == 2012) 

acled <- read.csv("/Users/kenziekmac/Downloads/2020-01-01-2024-02-29-Mexico.csv")
a$MUN_OCURR


max(as.numeric(as.character( a$MUN_OCURR )))