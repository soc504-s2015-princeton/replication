library(ggplot2)
library(dplyr)
library(xlsx)
library(zoo)
library(foreign)


##load main 1990 censo database from inegi to the town level
censo90 <- read.table("data/censo_1990_37_var.txt", header = TRUE, sep = "\t")

##create table with variables of interest and population
censo.1 <- tbl_df(censo90) %>%
  filter(mun !=0 ) %>%
  select(state = entidad, mun, twn = loc, total.pop = p_total, no.literacy = analfbet,
         n_hab_esp, habla_esp) %>%
  mutate(indi = as.numeric(as.character(n_hab_esp)) + as.numeric(as.character(habla_esp)), 
         muncode = (state * 1000) + mun, no.literacy = as.numeric(as.character(no.literacy))) %>%
  filter(muncode > 1000) %>%
  select(muncode, twn:indi, -state, -mun, -n_hab_esp,-habla_esp)

## create variable for population in towns of less than 2500 population
mun.total <- censo.1 %>%
  filter(twn == 0)

less.2500 <- censo.1 %>%
  filter(twn != 0 & twn != 9998 & twn != 9999, total.pop < 2500) %>%
  group_by(muncode) %>%
  summarise (pop.less.2500 = sum(total.pop))

mun.total <- left_join(mun.total, less.2500, by = "muncode")

#filtering out Oaxaca municipalities for analysis later
oaxaca <- filter(mun.total, muncode %in% 20001:20570) 

mun.total <- filter(mun.total, !(muncode %in% 20001:20570))

##create variables with proportion of people living in towns of less than 2500, proportion
## of indigenous speakers and proportion of illiteracy
mun.total <- mun.total %>%
  mutate(prop.less.2500 = pop.less.2500/total.pop, prop.indi = indi/total.pop, 
         prop.no.lit = no.literacy/total.pop) %>%
  select(-twn, -no.literacy, -indi, -pop.less.2500)

## elevation, no information on the 1990 census, used the 1995 Conteo. 

conteo.95 <- read.table("data/conteo_1995_37_var.txt", sep = "\t")
conteo.95 <- tbl_df(conteo.95)

##select variables create muncodes, filter NAs
elev <- conteo.95 %>%
  select(state = V1, mun = V3, elev = V9) %>%
  mutate(muncode = (state*1000) + mun) 

elev[elev == ""] <- NA
elev <- elev %>%
  filter(!is.na(elev))

#separating out oaxaca 
#THERE COULD BE A PROBLEM HERE DUE TO ADDITIONAL/CHANGED MUNICIPALITY CODES FOR OAXACA, ALSO WITH TWN/MUN CODING FOR OAXACA
elev.oax <- filter(elev, muncode %in% 20001:20570) 
elev <- filter(elev, !(muncode %in% 20001:20570))

##table with standard deviation for elevation among towns in a municipality
elev <- elev %>%
  group_by(muncode) %>%
  summarise(sd.elev = sd(elev))


##POPULATION DENSITY
##pull data with municipal area in meters to calculate population density, there is no GIS map, 
## for 1990, oldest one is 1995.#THERE COULD BE A PROBLEM HERE DUE TO ADDITIONAL/CHANGED MUNICIPALITY CODES FOR OAXACA

map1995 <- read.dbf("data/inegi_map1995.dbf")
map1995 <- tbl_df(map1995)

## area of each municipality in squared kilometers
area <- map1995 %>%
  mutate(muncode = paste(CVE_ENT, CVE_MUN, sep =""), sqkm = (AREA/1000^2)) %>%
  mutate(muncode = as.numeric(as.character(muncode))) %>%
  select(muncode, sqkm)

#separating out oaxaca
area.oax <- filter(area, muncode %in% 20001:20570)
area <- filter(area, !(muncode %in% 20001:20570))

## population density, total population over area. 
pop.dens <- left_join(mun.total, area, by = "muncode") %>%
  mutate(pop.dens = total.pop/sqkm)  %>%
  select(muncode, pop.dens)

#DOCTORS
##table with number of doctors by municipality 
docs <- read.csv(file="data/doctors_censo1990.csv", header=TRUE, skip = 4, encoding = "latin1")
docs <- tbl_df(docs)

##clean out NAs and others. 
docs <- docs %>%
  select(muncode = Clave, total = Total, doctors = Medicina) %>%
  mutate(doctors = as.numeric(as.character(doctors))) %>%
  filter(!grepl("996|997", muncode), muncode > 1000)

docs[is.na(docs)] <- 0

#separating out oaxaca
docs.oax <- filter(docs, muncode %in% 20001:20570)
docs <- filter(docs, !(muncode %in% 20001:20570))

##create variable with doctors por 10000 population
docs.per.10k <- left_join(mun.total, docs, by = "muncode") %>%
  mutate(docs.per.10k = (doctors/total.pop)*10000)  %>%
  select(muncode, docs.per.10k)


#FEMALE HEADED HOUSEHOLDS
fems <- read.csv(file="data/headhome_censo1990.csv", header=TRUE, skip = 4, encoding = "latin1")
fems <- tbl_df(fems)
##clean out NAs and others.
fems <- fems %>%
  select(muncode = Clave, total = Total, fem.house = Mujeres) %>%
  filter(!grepl("996|997", muncode), muncode > 1000)

fems[is.na(fems)] <- 0

#filtering out oaxaca
fems.oax <- filter(fems, muncode %in% 20001:20570)
fems <- filter(fems, !(muncode %in% 20001:20570))

#create variable with percent of female headed households
fems <- fems %>%
  mutate(pct.fem.house = fem.house/total) %>%
  select(muncode, pct.fem.house)


#HOMICIDES
homicides <- read.csv(file="data/homicide_1990_2013_INEGI.csv", header=FALSE, skip = 6, encoding = "latin1")
homicides <- tbl_df(homicides) %>%
  select(-V3:-V23)

## new coloumn names, default were unreadable
colnames(homicides) <- c("muncode", "Nombre", "hom.1992", "hom.1991", "hom.1990")
homicides[is.na(homicides)] <- 0

##clean out NAs and others, sum 1990,1991, 1992
homicides <- homicides %>%
  filter(!grepl("996|997|998|991|993|992", muncode), muncode > 1000) %>%
  mutate(hom.total = hom.1992+ hom.1991 + hom.1990) %>%
  select(muncode, hom.total)

#filtering out oaxaca
homicides.oax <- filter(homicides, muncode %in% 20001:20570)
homicides <- filter(homicides, !(muncode %in% 20001:20570))

##calculate homicide rate with three tiems the total population at risk, 
##for three yeras of homicides
homicide.rate <- right_join(homicides, mun.total, by = "muncode") %>%
  mutate(hom.rate = hom.total/(total.pop*3)*100000)  %>%
  select(muncode, hom.rate)

#YOUNG MALES
men <- read.csv(file="data/censo_1990_age.csv", header=FALSE, skip = 7, encoding = "latin1")
men <- tbl_df(men)

##clean out NAs and others.
young <- men %>%
  select(muncode = V1, up19 = V7, up24 = V8, up29 = V9) %>%
  filter(muncode != "#NAME?") 

##remove commas from data.
young$muncode <- gsub(" ","",young$muncode)
young$up19 <- gsub(",","",young$up19)
young$up24 <- gsub(",","",young$up24)
young$up29 <- gsub(",","",young$up29)

##create variable with total young men from 15 years old to 29
young <- young %>%
  mutate(muncode = as.numeric(as.character(muncode)),
         up19 = as.numeric(as.character(up19)),
         up24 = as.numeric(as.character(up24)),
         up29 = as.numeric(as.character(up29))) %>%
  filter(!is.na(muncode)) %>%
  mutate(young.total = up19+up24+up29)

#filtering out oaxaca
young.oax <- filter(young, muncode %in% 20001:20570)
young <- filter(young, !(muncode %in% 20001:20570))
  
##percent of young males per municipality
prct.young <- right_join(young, mun.total, by = "muncode") %>%
  mutate(prct.young = young.total/total.pop)  %>%
  select(muncode, prct.young)


##join all new variables to creat the main data frame with control and dependent variables

main <- left_join(mun.total, elev, by = "muncode")
main <- left_join(main, pop.dens, by = "muncode")
main <- left_join(main, docs.per.10k, by = "muncode")
main <- left_join(main, fems, by = "muncode")
main <- left_join(main, prct.young, by = "muncode")
main <- left_join(main,  homicide.rate, by = "muncode")

#DUMMY VARIABLE
#adding dummy variable for state of Mexico

main <- main %>% 
  mutate(dummy.SOM = as.numeric(muncode %in% 15001:15125))
  
#filter out all municipalities with more than 75% of pop living in towns of less than 2500 pop 
main <- filter(main, prop.less.2500 > .75)

## load table with conversion table between municipalities and distritos for Oaxaca
oaxaca.distritos <- read.xlsx("data/oaxaca_30distritos_2002.xls", 3, startRow = 5, endRow = 690, encoding = "latin1")

oaxaca.distritos <- tbl_df(oaxaca.distritos) 

#create table with muncodes 
distritos <- oaxaca.distritos  %>%
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

<<<<<<< HEAD

#the percentage of rural production units of five hectares of land or less, 
#the log of the average plot size, 
#and the log of persons per hectare of agricultural land
#the log of maize yields (expressed as tons per hectare) 
##with the unusual advantage that the exact mean of each category of plot size 
##is known (since both the total surface area and the number of units 
##in each of the eight plot size categories are known).
##percentage of the rural surface area in each municipality constituted by ejidos and 
##communal units according to the Agricultural and Livestock Census
##the percentage of subsistence agricultural units, defined as those 
##producing only for internal consumption within the household and not for sale in the market.
##the percentage of agricultural units with individual (as opposed to group) organization of production.
##the percentage of units dedicated to harvesting coffee and raising cattle.





land.area <- read.csv(file="data/censo_agricola_1991_land_area.csv", skip = 3, header = TRUE, encoding = "latin1")
land.area <- tbl_df(land.area)







#filter out new municipalies(distritos) with les 75% of pop living in towns of less thatn 2500 pop.

## creat a table to keep at hand showing which of the municipalities  is in which distrito
#oaxaca.mun <- oaxaca %>%
#  left_join(distritos, oaxaca, by = "muncode") %>%
#  select(muncode, less2500, total, prop2500, distrito) 

=======
>>>>>>> e8d61049ec89fe2f040cc5c1d7ff29adec07a634
##join distrito table with oaxaca population table by distrito and generate new muncodes with distrito number.
##filter out new municipalies(distritos) with more than 75% of pop living in towns of less thatn 2500 pop.
oaxaca.dist <- oaxaca %>%
  left_join(distritos, oaxaca, by = "muncode") 
oaxaca.dist <- left_join(oaxaca.dist, elev.oax, by = "muncode")
oaxaca.dist <- left_join(oaxaca.dist, area.oax, by = "muncode")
oaxaca.dist <- left_join(oaxaca.dist, docs.oax, by = "muncode")
oaxaca.dist <- left_join(oaxaca.dist, fems.oax, by = "muncode")
oaxaca.dist <- left_join(oaxaca.dist, homicides.oax, by = "muncode")
oaxaca.dist <- left_join(oaxaca.dist, young.oax, by = "muncode")
  
oaxaca.dist <- oaxaca.dist  %>% #add the rest of the permutations  
  group_by(distrito) %>% #will this work as a giant chunk? or will the different sample sizes cause a problem (due to NAs?)
  summarise(pop.less.2500 = sum(pop.less.2500), total.pop = sum(total.pop), prop.less.2500 = pop.less.2500/total.pop,
            indi = sum(indi), prop.indi = indi/total.pop, 
            no.literacy = sum(no.literacy), prop.no.lit = no.literacy/total.pop,
            sqkm = sum(sqkm), pop.dens = total.pop/sqkm, 
            doctors = sum(doctors), docs.per.10k = (doctors/total.pop)*10000, 
            fem.house = sum(fem.house), total = sum(total), pct.fem.house = fem.house/total,
            hom.total = sum(hom.total), home.rate = hom.total/(total.pop*3)*100000,
            young.total = sum(young.total), prct.young = young.total/total.pop, 
            sd.elev = sd(elev.oax)) %>%
  mutate(muncode = distrito + 20000) %>%
  select(muncode, total.pop, prop.less.2500, prop.indi, prop.no.lit, pop.dens, docs.per.10k, pct.fem.house, hom.rate, prct.young, elev.oax) 

#filtering to oaxaca sample, adding SOM dummy variable
oax.main <- oaxaca.dist %>%  
  filter(prop.less.2500 > .75) %>%
  mutate(dummy.SOM = 0)

## create a table to keep at hand showing which of the municipalities  is in which distrito
oaxaca.mun <- oaxaca %>%
  left_join(distritos, oaxaca, by = "muncode") %>%
  select(muncode, pop.less.2500, total.pop, distrito) 

## Yeiii!!! 697 municipalities, just like Villareal.
sample <- rbind(main, oax.main)
