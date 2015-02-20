library(ggplot2)
library(dplyr)
library(xlsx)


## load table with population by town size
censo <- read.csv("data/popless2500_censo1900.csv", header = TRUE, skip = 4, 
                  skipNul = TRUE, fileEncoding = "latin1")

censo <- tbl_df(censo) 

## create table that only includes municipalities with more that 75% 
## of the popultion living in towns of less than 2500 people
rural <- censo %>%
  mutate(muncode = as.numeric(as.character(Clave)),
                              less2500 = as.numeric(as.character(Menos.de.2.500.habitantes)), #change class and name of variables of interest
                              total = as.numeric(as.character(Total)), prop = less2500/total) %>%
  select(muncode, less2500, total, prop) %>%
  filter(!is.na(total) & prop > .75 & muncode > 1000) #filter out by pop criteria and state totals

## load table with 
oaxaca <- read.xlsx("data/oaxaca_30distritos_2002.xls", 3, startRow = 5, endRow = 690, encoding = "latin1")

oaxaca <- tbl_df(oaxaca) 
#create table with muncodes and distritos
distritos <- oaxaca %>%
  select(mun = CLAVE) %>%
  filter(!is.na(mun)) %>%
  mutate(mun2 = as.numeric(as.character(mun)),
         muncode = ifelse(mun2 > 99, paste("20", mun, sep = ""), paste("200", mun, sep = ""))) #add 20 or 200 depending un number of integers in Clave
 
##next step is label each muncode with a distrito number. every time there is an NA in muncodes, a new distrito starts