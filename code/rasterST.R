
##################################################################
## Source code for the book: "Displaying time series, spatial and
## space-time data with R: stories of space and time"

## Copyright (C) 2013-2012 Oscar Perpiñán Lamigueiro

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

## Set folder to where the local copy of github repository can be found
setwd('~/Dropbox/chapman/book/')

library(lattice)
library(latticeExtra)

myTheme <- custom.theme.2(pch=19, cex=0.7,
                          region=rev(brewer.pal(9, 'YlOrRd')),
                          symbol = brewer.pal(n=8, name = "Dark2"))
myTheme$strip.background$col='transparent'
myTheme$strip.shingle$col='transparent'
myTheme$strip.border$col='transparent'

xscale.components.custom <- function(...){
    ans <- xscale.components.default(...)
    ans$top=FALSE
    ans}
yscale.components.custom <- function(...){
    ans <- yscale.components.default(...)
    ans$right=FALSE
    ans}
myArgs <- list(as.table=TRUE,
               between=list(x=0.5, y=0.2),
               xscale.components = xscale.components.custom,
               yscale.components = yscale.components.custom)
defaultArgs <- lattice.options()$default.args

lattice.options(default.theme = myTheme,
                default.args = modifyList(defaultArgs, myArgs))

##################################################################
## Introduction
##################################################################

library(raster)
library(zoo)

SISdm <- brick('data/SISgal')

timeIndex <- seq(as.Date('2011-01-01'), by='day', length=365)
SISdm <- setZ(SISdm, timeIndex)
names(SISdm) <- format(timeIndex, '%a_%Y%m%d')

##################################################################
## Levelplot
##################################################################

pdf(file="figs/SISdm.pdf")
library(rasterVis)
levelplot(SISdm, layers=1:12, panel=panel.levelplot.raster)
dev.off()

SISmm <- zApply(SISdm, by=as.yearmon, fun='mean')

pdf(file="figs/SISmm.pdf")
levelplot(SISmm, panel=panel.levelplot.raster)
dev.off()

##################################################################
## Exploratory graphics
##################################################################

pdf(file="figs/SISdm_histogram.pdf")
histogram(SISdm, FUN=as.yearmon)
dev.off()

pdf(file="figs/SISdm_den.pdf")
densityplot(SISdm, FUN=as.yearmon)
dev.off()

pdf(file="figs/SISdm_splom.pdf")
splom(SISmm, xlab='', plot.loess=TRUE)
dev.off()

##################################################################
## Time series
##################################################################

pdf(file="figs/SISmm_xyplot.pdf")
xyplot(SISdm)
dev.off()

SISav <- mean(SISdm)
SISdif <- setZ(SISdm - SISav, getZ(SISdm))

pdf(file="figs/SISdm_horizonplot.pdf")
horizonplot(SISdif, digits=1)
dev.off()

##################################################################
## Hovmoller
##################################################################

pdf(file="figs/SISdm_hovmoller_lat.pdf")
hovmoller(SISdif, par.settings=RdBuTheme())

dev.off()

pdf(file="figs/SISdm_hovmoller_lon.pdf")
hovmoller(SISdif, dirXY=x, par.settings=RdBuTheme())
dev.off()

##################################################################
## Animation
##################################################################

cft <- brick('data/cft_20130417_0000.nc')
projLCC2d <- "+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84"
projection(cft) <- projLCC2d

timeIndex <- seq(as.POSIXct('2013-04-17 01:00:00', tz='UTC'), length=96, by='hour')
cft <- setZ(cft, timeIndex)
names(cft) <- format(timeIndex, 'D%d_H%H')

library(maptools)
library(rgdal)
projLL <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
spainBoundaries <- readShapePoly('~/Datos/ESP_adm/ESP_adm1',
                                 proj4string=projLL)
spainBoundaries <- spTransform(spainBoundaries, CRS(projection(cft)))

tmp <- tempdir()
trellis.device(png, file=paste0(tmp, '/Rplot%02d.png'),
                      res=300, width=1500, height=1500)
cloudTheme <- rasterTheme(region=brewer.pal(n=9, 'Blues'))
levelplot(cft, layout=c(1, 1), par.settings=cloudTheme) +
    layer(sp.polygons(spainBoundaries, lwd=0.6, fill='transparent'))
dev.off()

old <- setwd(tmp)
##convertCMD <- 'convert Rplot*.png -morph 3 morph%05d.jpg'
##system(convertCMD)
##movieCMD <- 'ffmpeg -r 10 -b 500000 -i morph%05d.jpg output.mp4'
movieCMD <- 'ffmpeg -r 6 -b 300k -i Rplot%02d.png output.mp4'
system(movieCMD)
##file.remove(dir(pattern='morph'))
file.remove(dir(pattern='Rplot'))
file.copy('output.mp4', paste0(old, 'figs/cft.mp4'), overwrite=TRUE)
setwd(old)
