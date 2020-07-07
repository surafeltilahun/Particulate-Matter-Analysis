library(ggplot2)
library(openair)
library(tidyverse)
library(dplyr)


#import the data sets into R envoronment 
pmanzac <- read.csv("C:/Users/khalsapro/Desktop/University/2019/Geocomp/Data/Full Clean/pmanzac.csv")
pmash <- read.csv("C:/Users/khalsapro/Desktop/University/2019/Geocomp/Data/Full Clean/pmash.csv")
pmger <- read.csv("C:/Users/khalsapro/Desktop/University/2019/Geocomp/Data/Full Clean/pmger.csv")
pmwash <- read.csv("C:/Users/khalsapro/Desktop/University/2019/Geocomp/Data/Full Clean/pmwash.csv")


#average the monthly dataset
anzac1 <- timeAverage(anzac, avg.time = "month")
splitAnzac <- anzac1%>% separate(date,c('Year','Month','Day'), sep ='-')
splitAnzac %>% 
  ggplot(aes(x= Year, y=PM10, fill=factor(Year))) +
  geom_boxplot()

splitAsh <- ash%>% separate(date,c('Year','Month','Day'), sep ='-')
splitAsh %>% 
  ggplot(aes(x= Year, y=PM10)) +
  geom_boxplot()

splitGer <- ger%>% separate(date,c('Year','Month','Day'), sep ='-')
splitGer %>% 
  ggplot(aes(x= Year, y=PM10)) +
  geom_boxplot()

splitWash <- wash%>% separate(date,c('Year','Month','Day'), sep ='-')
splitWash %>% 
  ggplot(aes(x= Year, y=PM10)) +
  geom_boxplot()



#visualize the time series of each region
par(mfrow=c(2,2))
a <- timePlot(anzac, pollutant = "PM10", main = "PM10 Observations in Anzac Square")
b <- timePlot(ash, pollutant = "PM10", main = "PM10 Observations in Ashburton")
c <- timePlot(ger, pollutant = "PM10", main = "PM10 Observations in Geraldine")
d <- timePlot(wash, pollutant = "PM10", main = "PM10 Obervations in Washdyke")

print(a, split = c(1, 1, 2, 1))
print(b, split = c(2, 1, 2, 1), newpage = FALSE)

print(c, split = c(1, 1, 2, 1))
print(d, split = c(2, 1, 2, 1), newpage = FALSE)

#visualize the wind speed of PM10 concentraion in each reagion
{r, eval=FALSE}
e <- pollutionRose(anzac[,-c(1,2)], ws = "ws", wd = "wd", pollutant = "PM10", main = "Pollution Rose Anzac Square")
f <- pollutionRose(ash[,-c(1,2)], ws = "ws", wd = "wd", pollutant = "PM10", main = "Pollution Rose Ashburton")
g <- pollutionRose(ger[,-c(1,2)], ws = "ws", wd = "wd", pollutant = "PM10", main = "Pollution Rose Geraldine")
h <- pollutionRose(wash[,-c(1,2)], ws = "ws", wd = "wd", pollutant = "PM10", main = "Pollution Rose Washdyke")

print(e, split = c(1, 1, 2, 1))
print(f, split = c(2, 1, 2, 1), newpage = FALSE)

print(g, split = c(1, 1, 2, 1))
print(h, split = c(2, 1, 2, 1), newpage = FALSE)


## Diurnal Plots

{r, include=FALSE}
dianzac <- timeVariation(anzac, pollutant = "PM10",
                         group = "season", key.columns = 4, hemisphere = "southern")

diash <- timeVariation(ash, pollutant = "PM10",
                       group = "season", key.columns = 4, hemisphere = "southern")

diger <- timeVariation(ger, pollutant = "PM10",
                       group = "season", key.columns = 4, hemisphere = "southern")

diwash <- timeVariation(wash, pollutant = "PM10",
                        group = "season", key.columns = 4, hemisphere = "southern")



dianzac$plot$hour$main <- "Diurnal variation of PM10 in Anzac Square"
diash$plot$hour$main <- "Diurnal variation of PM10 in Ashburton"
diger$plot$hour$main <- "Diurnal variation of PM10 in Geraldine"
diwash$plot$hour$main <- "Diurnal variation of PM10 in Washdyke"

a <- dianzac$plot$hour
b <- diash$plot$hour
c <- diger$plot$hour
d <- diwash$plot$hour

print(a, split = c(1, 1, 2, 1))
print(b, split = c(2, 1, 2, 1), newpage = FALSE)

print(c, split = c(1, 1, 2, 1))
print(d, split = c(2, 1, 2, 1), newpage = FALSE)



#visualize the correlation between each feature of each region
a <- corPlot(anzac[-c(1,3)], main="Correlation Plot - Anzac Square")
b <- corPlot(ash[-c(1,3)], main="Correlation Plot - Ashburton")
c <- corPlot(ger[-c(1,3)], main="Correlation Plot - Geraldine")
d <- corPlot(wash[-c(1,3)], main="Correlation Plot - Washdyke")


#visualizing daily averaged data 
avganzac <- timeAverage(pmanzac, avg.time = "day")
avgash <- timeAverage(pmash, avg.time = "day")
avgger <- timeAverage(pmger, avg.time = "day")
avgwash <- timeAverage(pmwash, avg.time = "day")

#polar plot
q <- polarPlot(avganzac, pollutant = "PM10", type="season", main="Polar Plot - Anzac Square", hemisphere="southern")
w <- polarPlot(avgash, pollutant = "PM10", type="season", main="Polar Plot - Ashburton", hemisphere="southern")



print(q, split = c(1, 1, 2, 1))
print(w, split = c(2, 1, 2, 1), newpage = FALSE)

e <- polarPlot(avgger, pollutant = "PM10", type="season", main="Polar Plot - Geraldine", hemisphere="southern")
r <- polarPlot(avgwash, pollutant = "PM10", type="season", main="Polar Plot - Washdyke", hemisphere="southern")



a <- trendLevel(pmanzac, "PM10", type = "weekend", main = "Anzac Square")
b <- trendLevel(pmash, "PM10", type = "weekend", main = "Ashburton")
c <- trendLevel(pmger, "PM10", type = "weekend", main = "Geraldine")
d <- trendLevel(pmwash, "PM10", type = "weekend", main = "Washdyke")

print(a, split = c(1, 1, 2, 1))
print(b, split = c(2, 1, 2, 1), newpage = FALSE)

print(c, split = c(1, 1, 2, 1))
print(d, split = c(2, 1, 2, 1), newpage = FALSE)










