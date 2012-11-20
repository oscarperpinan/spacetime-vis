
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

##################################################################
## Spatial Data
##################################################################

##################################################################
## Air Quality in Madrid
##################################################################

## codeStations.csv is extracted from the document
## http://www.mambiente.munimadrid.es/opencms/export/sites/default/calaire/Anexos/INTPHORA-DIA.pdf,
## table of page 3.

codEstaciones <- read.csv2('data/codeStations.csv')
codURL <- as.numeric(substr(codEstaciones$Codigo, 7, 8))

## The information of each measuring station is available at its own webpage, defined by codURL
URLs <- paste('http://www.mambiente.munimadrid.es/opencms/opencms/calaire/contenidos/estaciones/estacion', codURL, '.html', sep='')

library(XML)
library(sp)

## Access each webpage, retrieve tables and extract long/lat data
coords <- lapply(URLs, function(est){
  tables <- readHTMLTable(est)
  location <- tables[[2]]
  ## Clean the table content and convert to dms format
  ub2dms <- function(x){
    ch <- as.character(x)
    ch <- sub(',', '.', ch) 
    ch <- sub('O', 'W', ch) ## Some stations use "O" instead of "W"
    as.numeric(char2dms(ch, "º", "'", "'' "))
  }
  long <- ub2dms(location[2,1])
  lat <- ub2dms(location[2,2])
  alt <- as.numeric(sub(' m.', '', location[2, 3]))

  coords <- data.frame(long=long, lat=lat, alt=alt)

  coords
})

airStations <- cbind(codEstaciones, do.call(rbind, coords))

## The longitude of "El Pardo" station is wrong (positive instead of negative)
airStations$long[22] <- -airStations$long[22]

write.csv2(airStations, file='data/airStations.csv')

## Fill in the form at
## http://www.mambiente.munimadrid.es/opencms/opencms/calaire/consulta/descarga.html
## to receive the Diarios11.zip file.
unzip('data/Diarios11.zip')
rawData <- readLines('data/Datos11.txt')
## This loop reads each line and extracts fields as defined by the
## INTPHORA file:
## http://www.mambiente.munimadrid.es/opencms/export/sites/default/calaire/Anexos/INTPHORA-DIA.pdf
datos11 <- lapply(rawData, function(x){
  codEst <- substr(x, 1, 8)
  codParam <- substr(x, 9, 10)
  codTec <- substr(x, 11, 12)
  codPeriod <- substr(x, 13, 14)
  month <- substr(x, 17, 18)
  dat <- substr(x, 19, nchar(x))
  ## "N" used for impossible days (31st April)
  idxN <- gregexpr('N', dat)[[1]]
  nZeroDays <- length(idxN)
  day <- seq(1, 31-nZeroDays)
  ## Substitute V and N with ";" to split data from different days
  dat <- gsub('[VN]+', ';', dat)
  dat <- as.numeric(strsplit(dat, ';')[[1]])
  ## Only data from valid days
  dat <- dat[day]
  res <- data.frame(codEst, codParam, ##codTec, codPeriod,
                    month, day, year=2011,
                    dat)
  })
datos11 <- do.call(rbind, datos11)

write.csv2(datos11, 'data/airQuality.csv')

##################################################################
## Spanish General Elections
##################################################################

dat2011 <- read.csv('data/GeneralSpanishElections2011.gz')

census <- dat2011$Total.censo.electoral
validVotes <- dat2011$Votos.válidos
## Election results per political party and municipality
votesData <- dat2011[, 12:1023]
## Abstention as an additional party
votesData$ABS <- census - validVotes
## Winner party at each municipality
whichMax <- apply(votesData,  1, function(x)names(votesData)[which.max(x)])
## Results of the winner party at each municipality
Max <- apply(votesData, 1, max)
## OTH for everything but PP, PSOE and ABS
whichMax[!(whichMax %in% c('PP',  'PSOE', 'ABS'))] <- 'OTH'
## Percentage of votes with the electoral census
pcMax <- Max/census * 100

## Province-Municipality code. sprintf formats a number with leading zeros.
PROVMUN <- with(dat2011, paste(sprintf('%02d', Código.de.Provincia),
                                            sprintf('%03d', Código.de.Municipio),
                                            sep=""))

votes2011 <- data.frame(PROVMUN, whichMax, Max, pcMax)
write.csv(votes2011, 'data/votes2011.csv', row.names=FALSE)

##################################################################
## CM SAF
##################################################################

library(raster)

tmp <- tempdir()
unzip('data/SISmm2008_CMSAF.zip', exdir=tmp)
filesCMSAF <- dir(tmp, pattern='SISmm')
SISmm <- stack(paste(tmp, filesCMSAF, sep='/'))
## CM-SAF data is average daily irradiance (W/m2). Multiply by 24
## hours to obtain daily irradiation (Wh/m2)
SISmm <- SISmm * 24

## Monthly irradiation: each month by the correspondent number of days
daysMonth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
SISm <- SISmm * daysMonth / 1000 ## kWh/m2
## Annual average
SISav <- sum(SISm)/sum(daysMonth)
writeRaster(SISav, file='SISav')

library(raster)
pop <- raster('~/Datos/Nasa/875430rgb-167772161.0.FLOAT.TIFF')
landClass <- raster('~/Datos/Nasa/241243rgb-167772161.0.TIFF')

library(raster)
## http://neo.sci.gsfc.nasa.gov/Search.html?group=64
pop <- raster('875430rgb-167772161.0.FLOAT.TIFF')
## http://neo.sci.gsfc.nasa.gov/Search.html?group=20
landClass <- raster('241243rgb-167772161.0.TIFF')
