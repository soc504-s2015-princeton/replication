library(dplyr)
library(xlsx)
censo.1990 <- read.xlsx(file="popless2500_censo1900.xlsx", 1, header=FALSE, startRow=6, endRow=2610)

#taking off weird rows on top and selecting the variables we care about
censo.1990 <- select(censo.1990, X1:X4)
colnames(censo.1990) <- c("Clave", "Nombre", "Total", "Menos.2500")

#removing missing values, takes out 37ish municipalities
censo.1990[censo.1990 == ""] <- NA
censo.1990 <- filter(censo.1990, !is.na(Total) | !is.na(Menos.2500))
filter(censo.1990, is.na(Total) | is.na(Menos.2500) #to see what was dropped

#removing state overalls
a <- c(1:32, "01", "02", "03", "04", "05", "06", "07", "08", "09")
censo.muni <- filter(censo.1990, !(Clave %in% a))
filter(censo.1990, Clave %in% a) #for reference. 

#separating Oaxaca into separate dataset
Oaxaca <- filter(censo.muni, Clave %in% 20001:20570)
Oaxaca <- filter(Oaxaca, !is.na(Menos.2500))
censo.muni.noOax <- filter(censo.muni, !(Clave %in% 20001:20570))
#havent converted them into distritos yet.
 
#finding and filtering for percent of people living in towns of less than 2,500 for all municipalities
censo.muni.noOax$Total <- as.numeric(as.character(censo.muni.noOax$Total))
censo.muni.noOax$Menos.2500 <- as.numeric(as.character(censo.muni.noOax$Menos.2500))
censo.muni.noOax <- mutate(censo.muni.noOax, pct.rural = Menos.2500/Total)
sample.muni.noOax <- filter(censo.muni.noOax, pct.rural > .75) 

#doctors, the homicide rate(killed per 100,000 population), percentage of indigenous language speakers, and homes headed by women
#used my ugly code below because my brain is too tired to understand yours right now, but want to learn...

#DOCTORS
data_docs <- read.xlsx(file="doctors_censo1990.xlsx", 1, header=FALSE, startRow=6, endRow=2610)
data_docs <- tbl_df(data_docs)
data_docs <- select(data_docs, X1:X6) %>%
  select(-X5)
colnames(data_docs) <- c("Clave", "Nombre", "Total", "Doctors", "Menos.2500")
data_docs[data_docs == ""] <- NA
data_docs <- filter(data_docs, !is.na(Doctors)) #results in smaller number because some municipios dont have values for # of doctors
data_docs$Total <- as.numeric(as.character(data_docs$Total))
data_docs$Doctors <- as.numeric(as.character(data_docs$Doctors))

#adding doctors per 10k in each municipality.
docs_main <- filter(data_docs, !(Clave %in% a)) %>% 
  filter(Clave %in% sample.muni.noOax$Clave)  %>% #I know this isnt the right way...how should this be done in dplyr??
  mutate(docs_pr_10k = (Doctors/Total)*10000)
docs_oax <-filter(data_docs, !(Clave %in% a)) %>%
  filter(Clave %in% Oaxaca$Clave) %>%
  mutate(docs_pr_10k = (Doctors/Total)*10000)
#didn't filter out the sample of municipios or Oaxaca...want to talk to you about how we are doing that merging first


#FEMALE HEADED HOUSEHOLDS
fems <- read.xlsx(file="headhome_censo1990.xlsx", 1, header=FALSE, startRow=6, endRow=2610)
fems <- tbl_df(fems)
fems <- select(fems, X1:X5) %>%
  select(-X4)
colnames(fems) <- c("Clave", "Nombre", "Total_Hog", "Muj_Heads")
fems[fems == ""] <- NA
fems <- filter(fems, !is.na(Total_Hog) | !is.na(Muj_Heads))
fems$Total_Hog <- as.numeric(as.character(fems$Total_Hog))
fems$Muj_Heads <- as.numeric(as.character(fems$Muj_Heads))

#adding percent female headed households
fems_main <- filter(fems, !(Clave %in% a)) %>%
  filter(Clave %in% sample.muni.noOax$Clave) %>%
  mutate(Pct_Muj = Muj_Heads/Total_Hog)
fems_oax <- filter(fems, !(Clave %in% a)) %>%
  filter(Clave %in% Oaxaca$Clave) %>% 
  mutate(Pct_Muj = Muj_Heads/Total_Hog)


#INDIGENOUS LANGUAGE SPEAKERS
indig <- read.xlsx(file="indigenous_censo1990.xlsx", 1, header=FALSE, startRow=6, endRow=2610)
indig <- tbl_df(indig)
indig <- select(indig, X1:X4)
colnames(indig) <- c("Clave", "Nombre", "Total", "Habla_Lengua")
indig[indig == ""] <- NA
indig <- filter(indig, !is.na(Habla_Lengua))
indig$Total <- as.numeric(as.character(indig$Total))
indig$Habla_Lengua <- as.numeric(as.character(indig$Habla_Lengua))

#adding percent indigenous language speakers
indig_main <- filter(indig, !(Clave %in% a)) %>%
  filter(Clave %in% sample.muni.noOax$Clave) %>%
  mutate(Pct_Habla = Habla_Lengua/Total)
indig_oax <- filter(indig, !(Clave %in% a)) %>%
  filter(Clave %in% Oaxaca$Clave) %>%
  mutate(Pct_Habla = Habla_Lengua/Total)

#HOMICIDES
homicides <- read.xlsx(file="homicide_1990_2013_INEGI.xlsx", 1, header=FALSE, startRow=7, endRow=2610)
homicides <- tbl_df(homicides)
homicides <- select(homicides, -(X3:X23))
colnames(homicides) <- c("Clave", "Nombre", "Hom_1992", "Hom_1991", "Hom_1990")
homicides[homicides == ""] <- NA
homicides <- filter(homicides, !is.na(Hom_1992)) %>% #for some reason this wasnt working all together
  filter(!is.na(Hom_1991)) %>%
  filter(!is.na(Hom_1990))
homicides$Hom_1992 <- as.numeric(as.character(homicides$Hom_1992))
homicides$Hom_1991 <- as.numeric(as.character(homicides$Hom_1991))
homicides$Hom_1990 <- as.numeric(as.character(homicides$Hom_1990))

#adding homicide rate
homs_main <- filter(homicides, !(Clave %in% a)) %>%
  filter(Clave %in% sample.muni.noOax$Clave) %>%
  mutate(Hom_rate = ((Hom_1990 + Hom_1991 + Hom_1992)/(3*Total))*100,000) #doesnt work yet, have to add municipal totals to dataset
homs_oax <- filter(homicides, !(Clave %in% a)) %>% #same
  filter(Clave %in% Oaxaca$Clave) %>%
  mutate(Hom_rate = ((Hom_1990 + Hom_1991 + Hom_1992)/(3*Total))*100,000)

#DUMMY VARIABLE
#adding dummy variable for state of Mexico
censo.muni.noOax$Clave <- as.numeric(as.character(censo.muni.noOax$Clave))
censo.muni.noOax <- mutate(censo.muni.noOax, dummy_SOM = as.numeric(Clave %in% 15001:15125))

#% rural surface area in ejidos/comunal units
#Censo Agricola TAB05 col (D + E)/C * 100

#% subsistence agriculture
#Censo Agricola TAB10 col F/E*100

#log persons per hectare of agricultural land
#hectares of agricultural land TAB10 col E? 

#% agriculture units dedicated to coffee and cattle
#Cattle agricultural unity TAB11A Col E?

#THINGS I DONT THINK WE CAN DO WITH THE DATA WE HAVE
#% rural production units 5 hectares or less
#log average plot size
#log maize yield
#Inequality in land distribution (Thiele index)
#% agriculture units with individual production