
library(foreign)

a <- read.dbf("/Users/kenziekmac/Downloads/defunciones_base_datos_2021_dbf/defun21.dbf")

b <- read.dbf("/Users/kenziekmac/Downloads/defunciones_base_datos_2021_dbf/CAPGPO.dbf")


a$MUN_OCURR


max(as.numeric(as.character( a$MUN_OCURR )))