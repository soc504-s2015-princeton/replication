library(ggplot2)
library(dplyr)
library(readr)
library(xlsx)
library(zoo)



# poverty and inequality (2010) --------------------------------------------------


##load CONEVAL poverty and inequality database
coneval <- read_csv("data/3.3 Concentrado, indicadores de pobreza por municipio.csv", skip = 6, col_names = FALSE)


##create table with muncode and percent in poverty, in extreme poverty and gini index 
poor <- coneval %>%
  filter(!is.na(X3)) %>%
  select(muncode = X3, pct.poor = X6, 
         pct.extpoor = X10, gini = X45) 


# Human development index (2010, per capita GNP, schooling, health) ------------

HDI <- read_csv("data/HDI_2010_UNDP.csv", skip = 3, 
                col_names = c("state", "mun", "Entidad",  "Municipio",	"school.level",
                              "expected.school",	"GNP.pc", 	"child.mort",	"education.index",
                              "income.index", 	"health.index",	"HDI"))
HDI <- HDI %>%
  mutate(muncode = state*1000 + mun) %>%
  select(muncode, HDI)


# doctors per 1000 pop ----------------------------------------------------

doctors <- read_csv("data/doctors_2005_2010.csv", skip = 2, n_max = 2456)

doctors <- doctors %>%
  select(muncode = Clave, docs = `2010`)



# police per 1000 pop -----------------------------------------------------

police <- read_csv("data/police_2010.csv", skip = 2, n_max = 2462, 
                   col_names = c("muncode", "state", "mun", "total.pers", "pol100k", "pol1k"))


police <- police %>%
  select(muncode, pol1k) %>%
  mutate(pol1k = ifelse(pol1k == "n. d.", NA, pol1k))

 

# oaxaca distritos 2010 --------------------------------------------------

oaxaca.distritos <- read.xlsx("data/oaxaca_distritos_2010.xls", 3, 
                              startRow = 6, endRow = 685, encoding = "latin1")
oaxaca.distritos <- tbl_df(oaxaca.distritos) 

#create table with municipality codes 
distritos <- oaxaca.distritos  %>%
  select(mun = Clave) %>%
  filter(!is.na(mun)) %>%
  mutate(mun = as.numeric(as.character(mun)), 
         muncode = (mun + 20000))

#create column that assigns districts to each muncode
distritos$distrito = rep(NA, nrow(distritos))
distritos$distrito[is.na(distritos$mun)] <- c(1:30)
distritos$distrito <- na.locf(distritos$distrito)

#filter out NA rows with district names
distritos <- distritos %>%
  filter(!is.na(mun))


# homicides 2006, 2007, 2008 ----------------------------------------------

homicides <- read_csv(file="data/homicide_1990_2013_INEGI.csv", col_names = FALSE, skip = 6)

## new coloumn names, default were unreadable
colnames(homicides) <- c("muncode", "name", 2013:1990)
homicides[is.na(homicides)] <- 0

##clean out NAs and others, 
homicides <- homicides %>%
  filter(!grepl("996|997|998|991|993|992", muncode), muncode > 1000) %>%
  select(muncode, name, `2008`:`2006`)
