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





