#import libraries
library(dplyr)
library(sp)
library(tidyr)
library(spacetime)
library(SpatioTemporal)
library(gstat)

AnzacSquare <- read.csv("*********************")
Ashburton <- read.csv("*********************************")
Geraldine <- read.csv("*************************************")
Washdyke <- read.csv("************************************")

#extract the july dataset
splitAnzac <- AnzacSquare %>% separate(Date, c('Year','Month','Day'), sep = '-')
julyAnzac <- dplyr::filter(splitAnzac, Month == '07', Year == '2016')
splitAshburton <- Ashburton %>% separate(Date, c('Year','Month','Day'), sep = '-')
julyAshburton <- dplyr::filter(splitAshburton, Month == '07', Year == '2016')
splitGeraldine <- Geraldine %>% separate(Date, c('Year','Month','Day'), sep = '-')
julyGeraldine <- dplyr::filter(splitGeraldine, Month == '07', Year == '2016')
splitWashdyke <- Washdyke %>% separate(Date, c('Year','Month','Day'), sep = '-')
julyWashdyke <- dplyr::filter(splitWashdyke, Month == '07', Year == '2016')

Anzac <- subset(julyAnzac, select=c(Year, Month, Day, PM10))
Ashburton <- subset(julyAshburton, select=c(Year, Month, Day, PM10))
Geraldine <- subset(julyGeraldine, select=c(Year, Month, Day, PM10))
Washdyke <- subset(julyWashdyke, select=c(Year, Month, Day, PM10))
combineData <- Anzac
combineData$Ashburton <- Ashburton$PM10
combineData$Geraldine <- Geraldine$PM10
combineData$Washdyke <- Washdyke$PM10
colnames(combineData)[4] <- "Anzac"

#set the spatial location of the regions
Data.loc<-data.frame(Station=c("Ashburton","Geraldine","Anzac Square","Washdyke"),
                     Code = c("Ashburton", "Geraldine", "Anzac", "Washdyke"),
                     Lat=c(-43.9122,-44.1002,-44.4045,-44.3567),
                     Lon=c(171.7552,171.2415,171.2497,171.2363))

coordinates(Data.loc) = ~Lon+Lat
proj4string(Data.loc) = "+proj=longlat +datum=WGS84"

stations = 4:7
Data.loc = Data.loc[match(names(combineData[stations]), Data.loc$Code),]
row.names(Data.loc) = Data.loc$Station

combineData$time = ISOdate(combineData$Year, combineData$Month, combineData$Day, 0)

dataObj = STFDF(Data.loc, combineData$time, 
                data.frame(values = as.vector(t(combineData[stations]))))


# 1. Empirical Semivariogram Creation

{r }
vv <- variogram(object = values ~ 1, # fixed effect component
                data = dataObj, # July data
                width = 10, # spatial bin (10 km)
                cutoff = 10, # consider pts < 100 km apart
                tlags = 0.01:3.01) # 0 days to 3 days
plot(vv)


# 2. Spatio-temporal Semivariogram Construction 

## Separable Covariance Function

{r }
sepVgm <- vgmST(stModel = "separable",
                space = vgm(0.86, "Exp", 200, nugget = 0.1),
                time = vgm(1, "Sph", 200, nugget = 0.1), sill = 20)
sepVgm <- fit.StVariogram(vv, sepVgm)


## Metric Covariance Function

{r }
metricVgm <- vgmST(stModel = "metric",
                   joint = vgm(100, "Exp", 400, nugget = 0.1), sill = 10, stAni = 400)
metricVgm <- fit.StVariogram(vv, metricVgm)


## Best Fitted Model and Visualisation


metricMSE <- attr(metricVgm, "optim")$value
sepMSE <- attr(sepVgm, "optim")$value



plot(vv, list(sepVgm, metricVgm), main = "Semi-variance")


# Spatio Temporal Kriging Prediction

## Create space-time prediction grid


spat_pred_grid <- expand.grid( lon = seq(169, 171, length = 20),
                               lat = seq(-43, -45, length = 20)) %>%
  SpatialPoints(proj4string = CRS(proj4string(dataObj)))

gridded(spat_pred_grid) <- TRUE


## Create temporal grid based on the lag days


temp_pred_grid <- as.POSIXct("2016-07-01") + seq(3, 28, length = 3)
ST_pred <- STF(spat_pred_grid, temp_pred_grid) 


## Kriging Prediction 


pred_kriged <- krigeST(values ~ 1,
                       data = dataObj, # data set
                       newdata = ST_pred, # prediction grid
                       modelList = sepVgm, # best fitted semivariogram
                       computeVar = TRUE) # compute variances


## Plot the Prediction Surface


color_pal <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(16))
stplot(pred_kriged) 


## Plot the prediction standard error


pred_kriged$se <- sqrt(pred_kriged$var1.var)
stplot(pred_kriged[, , "se"], main = "Prediction std. errors (millimetre)", 
       layout = c(3, 2), col.regions = color_pal)


## December 2016. tommorow..






