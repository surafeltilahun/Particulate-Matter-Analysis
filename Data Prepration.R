library(tidyr)
library(plyr)
library(openair)

#counting missing values
is.missing <- function(x) 
{
  num = 0
  for (i in 1:length(x))
  {
    if (is.na(x[i])==TRUE){
      num = num+1
    }
  }
  return(num)
}
#counting negative values
is.neg <- function(x) 
{
  num = 0
  for (i in 1:length(x))
  {
    if (x[i] < 0){
      num = num+1
    }
  }
  return(num)
}


path = "***************************"
#importing the data into R from path
Anzac.Square <- read.csv(paste(path,"Anzac Square.csv", sep = ""))
Ashburton <- read.csv(paste(path,"Ashburton.csv", sep = ""))
Geraldine <- read.csv(paste(path,"Geraldine.csv", sep = ""))
Washdyke <- read.csv(paste(path,"Washdyke.csv", sep = ""))
namesAnzac <- c("date","StationName","WindMax","Temp2m","ws","CO","TempGround","SO2","PM10","Temp6m","PMCoarse","RelativeHumidity","wd","PM2.5")
namesAsh <- c("date","StationName","WindMax","Temp2m","ws","CO","TempGround","PM10","Temp6m","PMCoarse","RelativeHumidity","wd","PM2.5")
namesGerald <- c("date","StationName","WindMax","Temp2m","PMcoarse","ws","CO","TempGround","PM10","Temp6m","RelativeHumidity","wd","PM2.5")
namesWashdyke <- c("date","StationName","WindMax","Temp2m","PMcoarse","ws","CO","TempGround","SO2","PM10","Temp6m","wd","PM2.5")

#renaming the column names
names(Anzac.Square) <- namesAnzac
names(Ashburton) <- namesAsh
names(Geraldine) <- namesGerald
names(Washdyke) <- namesWashdyke

#replacing NAs with zeros
Anzac.Square[,-c(1,2)][Anzac.Square[,-c(1,2)] < 0] <- NA
Ashburton[,-c(1,2)][Ashburton[,-c(1,2)] < 0] <- NA
Geraldine[,-c(1,2)][Geraldine[,-c(1,2)] < 0] <- NA
Washdyke[,-c(1,2)][Washdyke[,-c(1,2)] < 0] <- NA

#removing NAs
Anzac.Square <- na.omit(Anzac.Square)
Ashburton <- na.omit(Ashburton)
Geraldine <- Geraldine[,-c(11)]
Geraldine <- na.omit(Geraldine)
Washdyke <- Washdyke[,-c(7,9)]
Washdyke <- na.omit(Washdyke)

#Putting date into the right format
Anzac.Square$date <- gsub("[.]", "\\1", Anzac.Square$date)
Anzac.Square$date <- strptime(Anzac.Square$date, "%d/%m/%Y %I:%M:%S %p")
Anzac.Square$date <- as.POSIXct(Anzac.Square$date)

#Putting date into the right format
Ashburton$date <- gsub("[.]", "\\1", Ashburton$date)
Ashburton$date <- strptime(Ashburton$date, "%d/%m/%Y %I:%M:%S %p")
Ashburton$date <- as.POSIXct(Ashburton$date)

#Putting date into the right format
Geraldine$date <- gsub("[.]", "\\1", Geraldine$date)
Geraldine$date <- strptime(Geraldine$date, "%d/%m/%Y %I:%M:%S %p")
Geraldine$date <- as.POSIXct(Geraldine$date)

#Putting date into the right format
Washdyke$date <- gsub("[.]", "\\1", Washdyke$date)
Washdyke$date <- strptime(Washdyke$date, "%d/%m/%Y %I:%M:%S %p")
Washdyke$date <- as.POSIXct(Washdyke$date)

#exporting thd cleaned version of data into parh2
path2 = "**************************"
write.csv(Anzac.Square, file = paste(path2,"avganzac.csv", sep = ""))
write.csv(Ashburton, file = paste(path2,"avgash.csv", sep = ""))
write.csv(Geraldine, file = paste(path2,"avgger.csv", sep = ""))
write.csv(Washdyke, file = paste(path2,"avgwash.csv", sep = ""))


