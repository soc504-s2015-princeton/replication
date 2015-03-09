install.packages("ggmap")
library(ggmap)
library(ggplot2)

Location <- "Mexico"
Mex.map <- get_map(location = Location, source = "stamen", maptype = "toner", crop = FALSE)
ggmap(Mex.map)

http://www.inside-r.org/packages/cran/RArcInfo/docs/plotpoly
http://stackoverflow.com/questions/17723822/administrative-regions-map-of-a-country-with-ggmap-and-ggplot2