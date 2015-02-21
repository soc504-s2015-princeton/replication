library(ggplot2)
library(dplyr)
library(xlsx)
library(zoo)

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

##create table with oaxaca municipalities. 
oaxaca.pop <- censo %>%
  filter(Clave > 20000 & Clave < 21000) %>%
  mutate(muncode = as.numeric(as.character(Clave)),
         less2500 = as.numeric(as.character(Menos.de.2.500.habitantes)), #change class and name of variables of interest
         total = as.numeric(as.character(Total)), prop = less2500/total) %>%
  select(muncode, less2500, total, prop)
  
  

## load table with conversion table between municipalities and distritos for Oaxaca
oaxaca <- read.xlsx("data/oaxaca_30distritos_2002.xls", 3, startRow = 5, endRow = 690, encoding = "latin1")

oaxaca <- tbl_df(oaxaca) 

#create table with muncodes 
distritos <- oaxaca %>%
  select(mun = CLAVE) %>%
  filter(!is.na(mun)) %>%
  mutate(mun = as.numeric(as.character(mun)), 
         muncode = (mun + 20000))

## create column that assigns distritos to each muncode
distritos$distrito = rep(NA, nrow(distritos))
distritos$distrito[is.na(distritos$mun)] <- c(1:30)
distritos$distrito <- na.locf(distritos$distrito)

##filter out NA rows with distrito names
distritos <- distritos %>%
  filter(!is.na(mun))

##join distrito table with oaxaca population table by distrito and generate new muncodes with distrito number.
##filter out new municipalies(distritos) with les 75% of pop living in towns of less thatn 2500 pop.

oaxaca.dist <- oaxaca.pop %>%
  left_join(distritos, oaxaca.pop, by = "muncode") %>%
  group_by(distrito) %>%
  summarise(less2500 = sum(less2500), total = sum(total), prop = less2500/total) %>%
  mutate(muncode = distrito + 20000) %>%
  select(muncode, less2500, total, prop) %>%
  filter(prop > .75)

## Yeiii!!! 697 municipalities, just like Villareal.
new.rural <- rural %>%
  filter(muncode > 21000 | muncode < 20000) %>%
  rbind(oaxaca.dist)


