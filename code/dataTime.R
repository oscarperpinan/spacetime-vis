
##################################################################
## Source code for the book: "Displaying time series, spatial and
## space-time data with R: stories of space and time"

## Copyright (C) 2012 Oscar Perpiñán Lamigueiro

## This program is free software you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published
## by the Free Software Foundation; either version 2 of the License,
## or (at your option) any later version.
 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.
####################################################################

setwd('~/Dropbox/chapman/book/')

prov=28 ##madrid
est=3 ##aranjuez
start='01/01/2004'
end='31/12/2011'

URL = paste("http://www.marm.es/siar/exportador.asp?T=DD&P=", 
  prov, "&E=", est, "&I=", start, "&F=", end, sep = "")

library(zoo)

aranjuez <- read.zoo(URL, index.column = 1,
                     format = "%d/%m/%Y", 
                     header = TRUE, skip = 1, fill = TRUE,
                     dec = ",", as.is = TRUE)

aranjuezClean <- aranjuez[, c(1, 2, 4, 6, 7, 11, 13, 16, 17, 18)]

names(aranjuezClean) <- c('TempAvg', 'TempMax', 'TempMin',
                          'HumidAvg', 'HumidMax',
                          'WindAvg', 'WindMax',
                          'Rain', 'Radiation', 'ET')

summary(aranjuezClean)

aranjuezClean <- within(as.data.frame(aranjuezClean),{
  TempMin[TempMin>40] <- NA
  HumidMax[HumidMax>100] <- NA
  WindAvg[WindAvg>10] <- NA
  WindMax[WindMax>10] <- NA
})

aranjuez <- zoo(aranjuezClean, index(aranjuez))

save(aranjuez, file='data/aranjuez.RData')

library(zoo)
  
SIAR <- read.csv('http://solar.r-forge.r-project.org/data/SIAR.csv')
table(SIAR$Provincia)
  
prov=31 ##navarra
navarraSIAR <- subset(SIAR, Provincia=='Navarra')
start='01/01/2011'
end='31/12/2011'

navarra <- lapply(navarraSIAR$N_Estacion, FUN=function(i){
  URL = paste("http://www.marm.es/siar/exportador.asp?T=DD&P=", 
    prov, "&E=", i, "&I=", start, "&F=", end, sep = "")
  dat <- try(read.zoo(URL, index.column = 1,
                      format = "%d/%m/%Y", 
                      header = TRUE, skip = 1, fill = TRUE,
                      dec = ",", as.is = TRUE))
  if (class(dat)=='try-error') NULL else dat$Radiacion
})

names(navarra) <- make.names(abbreviate(navarraSIAR$Estacion))
  
## Which stations are not accesible?
err <- sapply(navarra, is.null)

navarra <- do.call(cbind, navarra[!err])

save(navarra, file='data/navarra.RData')

unemployUSA <- read.csv('data/unemployUSA.csv')
nms <- unemployUSA$Series.ID
annualCols <- 14 + 13*(0:12) ##columns of annual summaries
unemployUSA <- as.data.frame(t(unemployUSA[,-c(1, annualCols)]))
names(unemployUSA) <- nms
head(unemployUSA)

library(zoo)

Sys.setlocale("LC_TIME", 'C')
idx <- as.yearmon(row.names(unemployUSA), format='%b.%Y')
unemployUSA <- zoo(unemployUSA, idx)

isNA <- apply(is.na(unemployUSA), 1, any)
unemployUSA <- unemployUSA[!isNA,]

save(unemployUSA, file='data/unemployUSA.RData')

CO2 <- read.csv('data/CO2_GNI_BM.csv')
head(CO2)

CO2data <- reshape(CO2, varying=list(names(CO2)[5:16]),
                      timevar='Year', v.names='Value',
                      times=2000:2011,
                      direction='long')
head(CO2data)

CO2data <- CO2data[, c(1, 3, 5, 6)]
CO2data <- reshape(CO2data, 
                   idvar=c('Country.Name','Year'),
                   timevar='Indicator.Name', direction='wide')
  
names(CO2data)[3:6] <- c('CO2.PPP', 'CO2.capita', 'GNI.PPP', 'GNI.capita')

isNA <- apply(is.na(CO2data), 1, any)
CO2data <- CO2data[!isNA, ]

head(CO2data)

save(CO2data, CO2, file='data/CO2.RData')
