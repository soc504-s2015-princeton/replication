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

write.csv(distritos, "distritos_2010.csv")
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

mun_total %>%
  filter(prop.2500 > .75)

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



corn <- read_excel("data/VIII Censo Agrícola 2007_Municipal.xls", 
                            sheet= "Cuadro 8", skip = 12, col_names = FALSE) 

corn1 <- corn %>%
  filter(row_number() <= 12730) %>%
  mutate(product = ifelse(!grepl("^\\s+|\\s+$", X1), X1, NA))

corn1$product <- na.locf(corn1$product)
corn1$product <- gsub("`|\\'", "", iconv(corn1$product, to="ASCII//TRANSLIT"))

corn1 <- corn1 %>%
  filter(grepl("^\\s+|\\s+$", X1)) %>% 
  mutate(state.name = ifelse(!grepl("^\\s\\s\\s+|\\s+$", X1), X1, NA))

corn1$state.name <- na.locf(corn1$state.name)
corn1$state.name <- trim(corn1$state.name)



corn1 <- corn1 %>%
  filter(grepl("^\\s\\s\\s+|\\s+$", X1), grepl("MAIZ", product)) 


coffee <-  read_excel("data/VIII Censo Agrícola 2007_Municipal.xls", 
                  sheet= "Cuadro 10", skip = 12, col_names = FALSE) 

coffee <- coffee %>%
  filter(row_number() <= 10448) %>%
  mutate(product = ifelse(!grepl("^\\s+|\\s+$", X1), X1, NA))

coffee$product <- na.locf(coffee$product)
coffee$product <- gsub("`|\\'", "", iconv(coffee$product, to="ASCII//TRANSLIT"))

coffee <- coffee %>%
  filter(grepl("^\\s+|\\s+$", X1)) %>% 
  mutate(state.name = ifelse(!grepl("^\\s\\s\\s+|\\s+$", X1), X1, NA))

coffee$state.name <- na.locf(coffee$state.name)
coffee$state.name <- trim(coffee$state.name)

coffee <- coffee%>%
  filter(grepl("^\\s\\s\\s+|\\s+$", X1), grepl("CAFE", product)) 

cattle <- read_excel("data/VIII Censo Agrícola 2007_Municipal.xls", 
                   sheet= "Cuadro 31", skip = 11, col_names = FALSE) 

cattle1 <- cattle %>%
  filter(row_number() <= 2481) %>%
  mutate(state.name = ifelse(!grepl("^\\s+|\\s+$", X1), X1, NA))

cattle1$state.name <- na.locf(cattle1$state.name)

cattle1 <- cattle1 %>%
  filter(grepl("^\\s+|\\s+$", X1))


cattle1$X1 <- trim(cattle1$X1)




naming <- function(df) {
  

  ## standardizes state names
  df$state.name <- gsub("`|\\'", "", iconv(df$state.name, to="ASCII//TRANSLIT"))
  df$state.name <- gsub("COAHUILA DE ZARAGOZA", "COAHUILA", df$state.name)
  df$state.name <- gsub("VERACRUZ LLAVE", "VERACRUZ", df$state.name)
  df$state.name <- gsub("MICHOACAN DE OCAMPO", "MICHOACAN", df$state.name)
  
  ## removes accents and tilde in municipal names
  df$mun.name<- gsub("`|\\'", "", iconv(df$mun.name, to="ASCII//TRANSLIT"))
  df$mun.name <- gsub("~","", df$mun.name)
  df$mun.name <- gsub("¨","", df$mun.name)
  df$mun.name <- gsub("~","", df$mun.name)
  df$mun.name <- gsub("\"","", df$mun.name)
  
  
  df$mun.name <- trim(df$mun.name)
  df$state.name <- trim(df$state.name)
  df <- df %>%
    mutate(name = paste(mun.name, state.name, sep = ", "))
  
}

map2010 <- read.dbf("data/Municipios_2010_5area.dbf")
map2010 <- tbl_df(map2010)

area <- map2010  %>%
  mutate(sqkm = (AREA/1000^2)) %>%
  select(muncode, sqkm)


#map.data <- rbind(main.for.map, oax.for.map)
# 
# map.dbf <-read.dbf("data/maps/municipios1995-distritos.dbf")
# #map.dbf <- map.dbf %>%
#    #mutate(CVE_MUN = as.numeric(as.character(CVE_MUN)),
#    #       CVE_ENT = as.numeric(as.character(CVE_ENT)),
#    #       muncode = (CVE_ENT*1000) + CVE_MUN)
#             
#map.merged <- left_join(map.dbf, map.data, by = "muncode" )
# map.merged <- map.merged[order(map.merged$OID), ]
# 
# #write.dbf(map.dbf, "data/maps/old_municipios1995-distritos.dbf") 
#write.dbf(map.merged, "data/maps/municipios1995-distritos.dbf")
# 
# #intervals for color-coding and legend
# #classIntervals(map.data$sd.elev, n = 4)
# #classIntervals(map.data$hom.rate, n = 4)
breaks_hom <- c(0, 15, 45, 75, 15432.1)
labels_hom <- c('[0 - 15]', '[15 - 45]', '[45 - 75]', '[75 - ]')
breaks_elev <- c(0, 100, 200, 390, 1040.154)
labels_elev <- c('[0 - 100]', '[100 - 200]', '[200 - 390]', '[390 - ]')

#preparing maps
map.shp <-  readShapePoly("data/maps/municipios1995-distritos.shp")

p <- ggplot(map.shp@data, aes(sd_elev, hom_rate))


map_geom <- fortify(map.shp, region = "muncode")

map_geom <- merge(map_geom, map.shp@data, by.x="id",  by.y="muncode")

map_geom$hom_breaks <- cut(map_geom$hom.rate, breaks = breaks_hom, labels = labels_hom, include.lowest = TRUE)

map_geom$elev_breaks <- cut(map_geom$sd.elev, breaks = breaks_elev, labels = labels_elev, include.lowest = TRUE)


Map1 <- ggplot(map_geom, aes(long,lat, group = group, fill = hom_breaks)) + geom_polygon(fill = NA, color = "black", size = 0.25)+ coord_equal() + 
  labs(x="", y="",fill= "Homicide Rate") + ggtitle ("Homicide Rate in Mexican Municipalities, 1990 - 1992")
# 
Map1 + scale_fill_brewer(palette = "Oranges") + guides(fill = guide_legend(reverse = TRUE)) + theme(axis.ticks = element_blank(), axis.text = element_blank()) + geom_polygon()

Map2 <- ggplot(map_geom, aes(long,lat, group = group, fill = elev_breaks))+ geom_polygon(fill = NA, color = "black", size = 0.25)+ coord_equal() + 
  labs(x="", y="",fill= "Std. Deviation of Elevation") + ggtitle ("Standard Deviation of Elevation in Mexican Municipalities")

Map2 + scale_fill_brewer(palette = "Oranges") + guides(fill = guide_legend(reverse = TRUE)) + theme(axis.ticks = element_blank(), axis.text = element_blank()) + geom_polygon()
