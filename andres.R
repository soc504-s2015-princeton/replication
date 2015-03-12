library(ggplot2)
library(dplyr)
library(xlsx)
library(zoo)
library(foreign)
library(tidyr)
library(gdata)

##load main 1990 censo database from inegi to the town level
censo90 <- read.table("data/censo_1990_37_var.txt", 
                      header = TRUE, sep = "\t", encoding = "latin1")

##create table with variables of interest and population
censo.1 <- tbl_df(censo90) %>%
  filter(mun !=0 ) %>%
  select(state = entidad, mun, twn = loc, total.pop = p_total, no.literacy = analfbet,
         n_hab_esp, habla_esp, state_name = nom_ent, mun_name = nom_mun) %>%
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

##change names to capital letters and remove accents
mun.total$state_name <- toupper(mun.total$state_name)
mun.total$state_name <- gsub("`|\\'", "", iconv(mun.total$state_name, to="ASCII//TRANSLIT"))
mun.total$state_name <- gsub("COAHUILA DE ZARAGOZA", "COAHUILA", mun.total$state_name)
mun.total$state_name <- gsub("VERACRUZ DE IGNACIO DE LA LLAVE", "VERACRUZ", mun.total$state_name)
mun.total$state_name <- gsub("MICHOACAN DE OCAMPO", "MICHOACAN", mun.total$state_name)

##change names to capital letters and remove accents

mun.total$mun_name <- toupper(mun.total$mun_name)
mun.total$mun_name <- gsub("`|\\'", "", iconv(mun.total$mun_name, to="ASCII//TRANSLIT"))
mun.total$mun_name <- gsub("~","", mun.total$mun_name)

#filtering out Oaxaca municipalities for analysis later
oaxaca <- filter(mun.total, muncode %in% 20001:20570) 
oaxaca$pop.less.2500[is.na(oaxaca$pop.less.2500)] <- 0

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
elev.oax <- filter(elev, muncode %in% 20001:20570) 
elev <- filter(elev, !(muncode %in% 20001:20570))

#table with standard deviation for elevation among towns in a municipality
elev <- elev %>%
  group_by(muncode) %>%
  summarise(sd.elev = sd(elev))

##POPULATION DENSITY
##pull data with municipal area in meters to calculate population density, there is no GIS map, 
## for 1990, oldest one is 1995.#THERE COULD BE A PROBLEM HERE DUE TO ADDITIONAL/CHANGED MUNICIPALITY CODES FOR OAXACA

map1995 <- read.dbf("data/inegi_map1995.DBF")
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
colnames(docs.oax) <- c("muncode", "total.d", "doctors")

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

main <- left_join(mun.total, pop.dens, by = "muncode")
main <- left_join(main, docs.per.10k, by = "muncode")
main <- left_join(main, fems, by = "muncode")
main <- left_join(main, prct.young, by = "muncode")
main <- left_join(main,  homicide.rate, by = "muncode")
main <- left_join(main, elev, by = "muncode")

#DUMMY VARIABLE
#adding dummy variable for state of Mexico and creating name variable, in
##order to join with agricultural variables dataframe

main <- main %>% 
  mutate(dummy.SOM = as.numeric(muncode %in% 15001:15125)) %>%
  mutate(name = paste(mun_name, state_name, sep = ", "))
  
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

##join distrito table with oaxaca population table by distrito and generate new muncodes with distrito number.
##filter out new municipalies(distritos) with more than 75% of pop living in towns of less thatn 2500 pop.
oaxaca$pop.less.2500[is.na(oaxaca$pop.less.2500)] <- 0

oaxaca.dist <- oaxaca %>%
  left_join(distritos, oaxaca, by = "muncode") 
oaxaca.dist <- left_join(oaxaca.dist, area.oax, by = "muncode")
oaxaca.dist <- left_join(oaxaca.dist, docs.oax, by = "muncode")
oaxaca.dist <- left_join(oaxaca.dist, fems.oax, by = "muncode")
oaxaca.dist <- left_join(oaxaca.dist, young.oax, by = "muncode") 
oaxaca.dist <- left_join(oaxaca.dist, homicides.oax, by = "muncode")
  
oaxaca.dist <- oaxaca.dist  %>% #add the rest of the permutations  
  group_by(distrito) %>% 
  summarise(pop.less.2500 = sum(pop.less.2500), total.pop = sum(total.pop), prop.less.2500 = pop.less.2500/total.pop,
            indi = sum(indi), prop.indi = indi/total.pop, 
            no.literacy = sum(no.literacy), prop.no.lit = no.literacy/total.pop,
            sqkm = sum(sqkm), pop.dens = total.pop/sqkm, 
            doctors = sum(doctors), docs.per.10k = (doctors/total.pop)*10000, 
            fem.house = sum(fem.house), total = sum(total), pct.fem.house = fem.house/total,
            young.total = sum(young.total), prct.young = young.total/total.pop, 
            hom.total = sum(hom.total), hom.rate = hom.total/(total.pop*3)*100000) %>%
  mutate(muncode = distrito + 20000) %>%
  select(muncode, total.pop, prop.less.2500, prop.indi, prop.no.lit, pop.dens, docs.per.10k, pct.fem.house, prct.young, hom.rate) 

#getting SD of elevation and SOM dummy variable and then adding them to the oaxaca dataset
elev.oax <- left_join(distritos, elev.oax, by = "muncode") %>%
  group_by(distrito) %>%
  summarise(sd.elev = sd(elev)) %>%
  mutate(muncode = distrito + 20000, dummy.SOM = 0) %>%
  select(muncode, sd.elev, dummy.SOM)

oaxaca.dist <- left_join(oaxaca.dist, elev.oax, by = "muncode")

#filtering to oaxaca sample 
oax.main <- oaxaca.dist %>%  
  filter(prop.less.2500 > .75) 
  
## create a table to keep at hand showing which of the municipalities  is in which distrito
oaxaca.mun <- oaxaca %>%
  left_join(distritos, oaxaca, by = "muncode") %>%
  select(muncode, pop.less.2500, total.pop, distrito) 

## Yeiii!!! 697 municipalities, just like Villareal.
sample <- rbind(main, oax.main)

#agricultural variables
##load all the files
prod_units <- read.csv(file="data/censo_agricola_1991_produnits.csv", header=FALSE, skip = 8, stringsAsFactors = FALSE, encoding = "UTF-8")
prod_units <- tbl_df(prod_units)
land <- read.csv(file="data/censo_agricola_1991_land_area.csv", header=FALSE, skip = 8, stringsAsFactors = FALSE, encoding = "UTF-8")
land <- tbl_df(land)
subs <- read.csv("data/censo_agricola_1991_autoconsumo.csv", header = FALSE, skip = 11, stringsAsFactors = FALSE, encoding = "UTF-8")
subs <- tbl_df(subs)
cattle <- read.csv("data/censo_agricola_1991_cattle.csv", header = FALSE, skip = 10, stringsAsFactors = FALSE, encoding = "UTF-8")
cattle <- tbl_df(cattle)
corn <- read.csv("data/censo_agricola_1991_corn.csv", header = FALSE, skip = 6187, nrows = 2426, stringsAsFactors = FALSE, encoding = "UTF-8")
corn <- tbl_df(corn)

##function that corrects municipality names and creates a new variable
##with pasted municipality name and state
naming <- function(df) {
  df$V3 <- gsub("[[:space:]]", "", df$V3)
  df$V4 <- gsub("[[:space:]]", "", df$V4)
  df$V5 <- gsub("[[:space:]]", "", df$V5)
  df$V6 <- gsub("[[:space:]]", "", df$V6)
  
  
  df1 <- df %>%
    select(state = V1, mun = V2, V3, V4, V5, V6) %>%
    mutate(state = as.character(state), mun = as.character(mun), 
           V3 = as.numeric(V3), V4 = as.numeric(V4),
           V5 = as.numeric(V5), V6 = as.numeric(V6))             
  
  df1$state[df1$state == ""] <- NA
  df1$mun[df1$mun == ""] <- NA
  df1$mun <- gsub("\x84", "N",df1$mun) 
  df1$state <- na.locf(df1$state)
  
  df1 <- df1 %>%
    separate(mun, into = c("mun", "art"), sep = ",", extra = "merge") %>%
    trim(land1$art) %>%
    mutate(mun = ifelse(!is.na(art), paste(art, mun, sep = " "), mun)) %>%
    filter(!is.na(mun)) %>%
    mutate(name = paste(mun, state, sep = ", ")) %>%
    select(-art)
         
}

##run the naming function for each file
prod_units <- naming(prod_units)
land <- naming(land)
subs <- naming(subs)
cattle <- naming(cattle)
corn <- naming(corn)


##get total producton units
prod_units[is.na(prod_units)] <- 0
prod_units1 <- prod_units %>%
  select(name, total_prod_units = V3) %>%
  mutate(total_prod_units = as.numeric((total_prod_units)))


##calculate proportions for land surface area according to property type
land[is.na(land)] <- 0
land1 <- land %>%
  select(name, total_area = V3, ejidal = V4, 
         comunal = V5,  private = V6) %>%
  mutate(total_area = as.numeric((total_area)), 
         ejidal = as.numeric(ejidal), 
         comunal = as.numeric(comunal), 
         private = as.numeric(private),
         prop.ej.comm = (ejidal+comunal)/total_area *100, 
         prop.individual = private/total_area*100)

##calculate proportion of units dedicate to subsistance farming

subs[is.na(subs)] <- 0
subs1 <- subs %>%
  select(name, total_units = V5, subs_units = V6) %>%
  mutate(total_units = as.numeric((total_units)), 
         subs_units = as.numeric(subs_units), 
         prop.subs = (subs_units/total_units *100))


## proportion of units with cattle needs to be calculate from 
##total production units.
cattle[is.na(cattle)] <- 0
cattle1 <- cattle %>%
  select(name, total_cattle_units = V5)

##maize yields in ton per hectare
corn$V6[is.na(corn$V6)] <- 0
corn1 <- corn %>%
  select(name, corn_tons = V5)


agr_var <- left_join(prod_units1, land1, by = "name")
agr_var <- left_join(agr_var, subs1, by = "name" )
agr_var <- left_join(agr_var, corn1, by = "name" )
agr_var <- left_join(agr_var, cattle1, by = "name" )

##join all control variables with agricultrual variables
final.df <- left_join(agr_var, main, by = "name")

##agricultural variables still needed
#the percentage of rural production units of five hectares of land or less 
#the log of the average plot size
#Thiel's index
##the percentage of units dedicated to harvesting coffee
#the log of persons per hectare of agricultural land

#OLD CODE TO CHECK OAXACA SAMPLE
##join distrito table with oaxaca population table by distrito and generate new muncodes with distrito number.
##filter out new municipalies(distritos) with les 75% of pop living in towns of less thatn 2500 pop.
#oaxaca.dist1 <- oaxaca %>%
  #left_join(distritos, oaxaca, by = "muncode") %>%
  #group_by(distrito) %>%
  #summarise(pop.less.2500 = sum(pop.less.2500), total.pop = sum(total.pop), prop.2500 = pop.less.2500/total.pop) %>% 
  #mutate(muncode = distrito + 20000) %>%
  #select(muncode, pop.less.2500, total.pop, prop.2500) %>%
  #filter(prop.2500 > .75)


