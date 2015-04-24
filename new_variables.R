library(ggplot2)
library(dplyr)
library(readr)
library(xlsx)
library(zoo)
library(readxl)
library(SDMTools)



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



# census 2010 -------------------------------------------------------------


census <- read_delim("data/ITER_NALTXT10.TXT", delim = "\t", col_names = TRUE,
                     col_types = list(P3YM_HLI = col_numeric(), P8A14AN = col_numeric(), 
                                      P15YM_AN = col_numeric(), HOGJEF_F = col_numeric(),
                                      P_15A17_M = col_numeric(), P_18A24_M = col_numeric()))

census[is.na(census)] <- 0

## select and calculate total population, elevation, indigenous population, female hedade households, 
## muncode, total illiteracy, young males 15-24.

total_pop <- census %>%
  select(state = ENTIDAD, mun = MUN, mun.name = NOM_MUN, state.name = NOM_ENT, twn = LOC, pop = POBTOT,
         elev = ALTITUD, indi = P3YM_HLI, P8A14AN, P15YM_AN, fem.house = HOGJEF_F,
         P_15A17_M, P_18A24_M) %>%
  mutate(muncode = state*1000 + mun,
         illiteracy = P8A14AN + P15YM_AN, 
         young.males = P_15A17_M + P_18A24_M) %>%
  select(-state, -mun, -P8A14AN, -P15YM_AN, -P_15A17_M, -P_18A24_M) %>%
  filter(twn != "0", twn != "9999", twn != "9998") %>%
  group_by(muncode, mun.name, state.name)  %>%
  summarise(total.pop = sum(pop), weigh.elev = wt.sd(elev, pop),  sd.elev = sd(elev),
            illiteracy = sum(illiteracy), indi = sum(indi), young.males = sum(young.males), 
            fem.house = sum(fem.house))


## did not group by MUNCODE, needs Oaxaca subsetting before doing that. 


## all names have to be fixed so that they can be joined by name to the agr variables which don't have muncode.

total_pop$mun.name <- gsub("\xed", "i", total_pop$mun.name)
total_pop$mun.name <- gsub("\xfa", "u", total_pop$mun.name)
total_pop$mun.name <- gsub("\xf3", "o", total_pop$mun.name)
total_pop$mun.name <- gsub("\xe9", "e", total_pop$mun.name)
total_pop$mun.name <- gsub("\xe1", "a", total_pop$mun.name)
total_pop$mun.name <- gsub("\xfc\xbe\x98\xb6\x98\xbc", "u", total_pop$mun.name)
total_pop$mun.name <- gsub("\xc1", "A", total_pop$mun.name)
total_pop$mun.name <- gsub("\xfc\xbe\x8c\x96\x98\xbc", "n", total_pop$mun.name)

total_pop$state.name <- gsub("\xed", "i", total_pop$state.name)
total_pop$state.name <- gsub("\xfa", "u", total_pop$state.name)
total_pop$state.name <- gsub("\xf3", "o", total_pop$state.name)
total_pop$state.name <- gsub("\xe9", "e", total_pop$state.name)
total_pop$state.name <- gsub("\xe1", "a", total_pop$state.name)

total_pop$mun.name <- toupper(total_pop$mun.name)
total_pop$state.name <- toupper(total_pop$state.name)

total_pop$state.name <- gsub("COAHUILA DE ZARAGOZA", "COAHUILA", total_pop$state.name)
total_pop$state.name <- gsub("VERACRUZ DE IGNACIO DE LA LLAVE", "VERACRUZ", total_pop$state.name)
total_pop$state.name <- gsub("MICHOACAN DE OCAMPO", "MICHOACAN", total_pop$state.name)



total_pop <- total_pop %>%
  mutate(name = paste(mun.name, state.name, sep = ", "))


mun_total <- census %>%
  filter(LOC == 0 & MUN != 0) %>%
  mutate(muncode = ENTIDAD*1000 + MUN) %>%
  select(muncode, POBTOT)

less_2500 <- census %>%
  filter(LOC != 0 & LOC != 9998 & LOC != 9999, POBTOT < 2500) %>%
  mutate(muncode = ENTIDAD*1000 + MUN) %>%
  group_by(muncode) %>%
  summarise(pop.less.2500 = sum(POBTOT))

mun_total <- left_join(mun_total, less_2500, by = "muncode") %>%
  mutate(prop.2500 = pop.less.2500/POBTOT)

mun_total <- left_join(mun_total, total_pop, by = "muncode")

## command to group by muncode, and calculate sd elevation, and sd elevation weighed by pop.
#

# agricultural variables --------------------------------------------------



prod_units <- read_excel("data/VIII Censo Agrícola 2007_Municipal.xls", sheet= "Cuadro 1", skip = 8,
                         col_names = FALSE) ## ignore crazy numbers in console

## all this needs to be turned into a function, it identifies if the name is of a state or a municipality 
## if there are two leading white spaces, if there is only one white space it is a state and moved to a new column
prod_units1 <- prod_units %>%
  select(mun.name = X1, total.units = X2, surface = X3) %>%
  filter(!is.na(mun.name), row_number() <= 2482, grepl("^\\s+|\\s+$", mun.name)) %>%
  mutate(state.name = ifelse(grepl("^\\s\\s+|\\s+$", mun.name), NA, mun.name))


prod_units1$state.name <- na.locf(prod_units1$state.name) ## subsistutes NAs with state names

prod_units1 <- prod_units1 %>%
  filter(grepl("^\\s\\s+|\\s+$", mun.name)) # filters out state names in original column


## standardizes state names
prod_units1$state.name <- gsub("`|\\'", "", iconv(prod_units1$state.name, to="ASCII//TRANSLIT"))
prod_units1$state.name <- gsub("COAHUILA DE ZARAGOZA", "COAHUILA", prod_units1$state.name)
prod_units1$state.name <- gsub("VERACRUZ LLAVE", "VERACRUZ", prod_units1$state.name)
prod_units1$state.name <- gsub("MICHOACAN DE OCAMPO", "MICHOACAN", prod_units1$state.name)
  
## removes accents and tilde in municipal names
prod_units1$mun.name<- gsub("`|\\'", "", iconv(prod_units1$mun.name, to="ASCII//TRANSLIT"))
prod_units1$mun.name <- gsub("~","", prod_units1$mun.name)
prod_units1$mun.name <- gsub("¨","", prod_units1$mun.name)
prod_units1$mun.name <- gsub("~","", prod_units1$mun.name)
prod_units1$mun.name <- gsub("\"","", prod_units1$mun.name)



prod_units1 <- prod_units1 %>%
  mutate(name = paste(mun.name, state.name, sep = ","))

library(gdata)

prod_units1$name <- trim(prod_units1$name)

## test if census and agricultural variables can be joined

sample_main <- full_join(mun_total, prod_units1, by = "name")

## 30 don't match due to differences in spelling. 
