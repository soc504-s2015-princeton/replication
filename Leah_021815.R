library(dplyr)
library(xlsx)
censo.1990 <- read.xlsx(file="popless2500_censo1900.xlsx", 1, header=FALSE)

#taking off weird rows on top and selecting the variables we care about
censo.1990 <- slice(censo.1990, 5:1000)
censo.1990 <- select(censo.1990, X1:X4)

#removing missing values, takes out 37ish municipalities
censo.1990[censo.1990 == ""] <- NA
censo.1990 <- filter(censo.1990, !is.na(X3) | !is.na(X4))
filter(censo.1990, is.na(X3) | is.na(X4)) #to see what was dropped

#removing state overalls
a <- c(1:18, "01", "02", "03", "04", "05", "06", "07", "08", "09")
censo.muni <- filter(censo.1990, !(X1 %in% a))
filter(censo.1990, X1 %in% a) #for reference. Where is Oaxaca? See the note at the end of this script

#renaming and removing extra row, should have done this earlier
colnames(censo.muni) <- c("Clave", "Nombre", "Total", "Menos.2500")
censo.muni <- slice(censo.muni, 2:909)

#finding and filtering for percent of people living in towns of less than 2,500 for all municipalities
censo.muni$Total <- as.numeric(censo.muni$Total) #I don't know why this was coded incorrectly.
censo.muni$Menos.2500 <- as.numeric(censo.muni$Menos.2500) #I don't know why this was coded incorrectly.
censo.muni <- mutate(censo.muni, pct.rural = Menos.2500/Total)
sample.muni <- filter(censo.muni, pct.rural > .75) 
View(sample.muni)

#FOR SOME REASON I AM GETTING 545 instead of 697. WHAT AM I DOING WRONG?? Some ideas below:
#His actual number is 686 after he dropped missing values. However, more than 11 municipios had missing values 
#Note from paper "Municipalities belonging to the state of Oaxaca were grouped into 30 districts commonly used for statistical purposes (INEGI 2002)." 
#Oaxaca has 570 municipios, more than any other state, which are grouped into 30 distritos. These do not seem to be included in the data. http://es.wikipedia.org/wiki/Regiones_de_Oaxaca

