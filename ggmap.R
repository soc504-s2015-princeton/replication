library(ggmap)
library(ggplot2)
library(maps)
library(rgdal)
library(maptools)
library(gpclib) 
gpclibPermit()

load("MEX_adm2.RData")
mex.mun <- get("gadm")
mex.mun.df <- fortify(mex.mun, region = "NAME_2")
mex.mun.df1 <- left_join(mex.mun.df, homicide.rate, by = "id")

p <- ggplot(mex.mun.df, aes(x = long, y = lat, group = group))
p + geom_polygon(aes(fill = cut(hom.rate, 4))) + 
  labs(x=" ", y=" ") + 
  theme_bw() + 
  scale_fill_brewer("Ave Homicide Rate 1990-2 (Per 100,000)", palette  = 'PuRd') + 
  coord_map() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank())

#CREATING DATAFRAME OF HOMICIDE RATES 1990-2
#population total
censo90 <- read.table("data/censo_1990_37_var.txt", header = TRUE, sep = "\t")
censo.1 <- tbl_df(censo90) %>%
  filter(mun !=0 ) %>%
  select(state = entidad, mun, twn = loc, total.pop = p_total, no.literacy = analfbet,
         n_hab_esp, habla_esp) %>%
  mutate(indi = as.numeric(as.character(n_hab_esp)) + as.numeric(as.character(habla_esp)), 
         muncode = (state * 1000) + mun, no.literacy = as.numeric(as.character(no.literacy))) %>%
  filter(muncode > 1000) %>%
  select(muncode, twn:indi, -state, -mun, -n_hab_esp,-habla_esp)
mun.total <- censo.1 %>%
  filter(twn == 0)

#homicide rate
homicides <- read.xlsx(file="data/homicide_1990_2013_INEGI.xlsx", 1, header=FALSE, startRow=6, endRow=2610)
homicides <- tbl_df(homicides) %>%
  select(-X3:-X23)
colnames(homicides) <- c("muncode", "name", "hom.1992", "hom.1991", "hom.1990")
homicides[is.na(homicides)] <- 0
homicides$hom.1992 <- as.numeric(as.character(homicides$hom.1992))
homicides$hom.1991 <- as.numeric(as.character(homicides$hom.1991))
homicides$hom.1990 <- as.numeric(as.character(homicides$hom.1990))
homicides$muncode <- as.numeric(as.character(homicides$muncode))
homicides <- homicides %>%
  filter(!is.na(hom.1992)) %>% 
  filter(!is.na(hom.1991)) %>% 
  filter(!is.na(hom.1990)) %>% 
  filter(!is.na(muncode)) %>% 
  filter(muncode > 1000) %>%
  mutate(hom.total = hom.1992 + hom.1991 + hom.1990) %>%
  select(muncode, name, hom.total)
homicide.rate <- right_join(homicides, mun.total, by = "muncode") %>%
  mutate(hom.rate = hom.total/(total.pop*3)*100000)  %>%
  select(name, hom.rate) %>%
  filter(!is.na(name))
colnames(homicide.rate) <- c("id", "hom.rate")

http://spatioanalytics.com/2014/02/20/shapefile-polygons-plotted-on-google-maps-using-ggplot-throw-some-throw-some-stats-on-that-mappart-2/
http://stackoverflow.com/questions/17723822/administrative-regions-map-of-a-country-with-ggmap-and-ggplot2
