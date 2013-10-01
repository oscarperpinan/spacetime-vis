##################################################################
## Source code for the book: "Displaying time series, spatial and
## space-time data with R"

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

##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

Sys.setlocale("LC_TIME", 'C')

##################################################################
## CMSAF Data
##################################################################

library(raster)
library(zoo)
library(rasterVis)

SISdm <- brick('data/SISgal')

timeIndex <- seq(as.Date('2011-01-01'), by='day', length=365)
SISdm <- setZ(SISdm, timeIndex)
names(SISdm) <- format(timeIndex, '%a_%Y%m%d')

##################################################################
## Levelplot
##################################################################

pdf(file="figs/SISdm.pdf")
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

pdf(file="figs/SISdm_boxplot.pdf")
bwplot(SISdm, FUN=as.yearmon)
dev.off()

png(filename="figs/SISmm_splom.png",res=600,height=4000,width=4000)
splom(SISmm, xlab='', plot.loess=TRUE)
dev.off()

##################################################################
## Space-time and time series plots
##################################################################

pdf(file="figs/SISdm_hovmoller_lat.pdf")
hovmoller(SISdm, par.settings=BTCTheme())
dev.off()

png(filename="figs/SISmm_xyplot.png",res=300,height=2000,width=2000)
xyplot(SISdm, digits=1, col='black', lwd=0.2, alpha=0.6)
dev.off()

pdf(file="figs/SISdm_horizonplot.pdf")
horizonplot(SISdm, digits=1,
            col.regions=rev(brewer.pal(n=6, 'PuOr')),
            xlab='', ylab='Latitude')
dev.off()

##################################################################
## Animation
##################################################################

##################################################################
## Data
##################################################################

cft <- brick('data/cft_20130417_0000.nc')
## use memory instead of file
cft[] <- getValues(cft)
## set projection
projLCC2d <- "+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84"
projection(cft) <- projLCC2d
#set time index
timeIndex <- seq(as.POSIXct('2013-04-17 01:00:00', tz='UTC'), length=96, by='hour')
cft <- setZ(cft, timeIndex)
names(cft) <- format(timeIndex, 'D%d_H%H')

##################################################################
## Spatial context: administrative boundaries
##################################################################

library(maptools)
library(rgdal)
library(maps)
library(mapdata)


projLL <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
cftLL <- projectExtent(cft, projLL)
cftExt <- as.vector(bbox(cftLL))
boundaries <- map('worldHires',
                  xlim=cftExt[c(1,3)], ylim=cftExt[c(2,4)],
                  plot=FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=projLL)
boundaries <- spTransform(boundaries, CRS(projLCC2d))

##################################################################
## Producing frames and movie
##################################################################

cloudTheme <- rasterTheme(region=brewer.pal(n=9, 'Blues'))

tmp <- tempdir()
trellis.device(png, file=paste0(tmp, '/Rplot%02d.png'),
                      res=300, width=1500, height=1500)
levelplot(cft, layout=c(1, 1), par.settings=cloudTheme) +
    layer(sp.lines(boundaries, lwd=0.6))
dev.off()

old <- setwd(tmp)
## Create a movie with ffmpeg using 6 frames per second a bitrate of 300kbs
movieCMD <- 'ffmpeg -r 6 -b 300k -i Rplot%02d.png output.mp4'
system(movieCMD)
file.remove(dir(pattern='Rplot'))
file.copy('output.mp4', paste0(old, '/figs/cft.mp4'), overwrite=TRUE)
setwd(old)

##################################################################
## Static image
##################################################################

pdf(file="figs/cft.pdf")
levelplot(cft, layers=25:48, layout=c(6, 4),
          par.settings=cloudTheme,
          names.attr=paste0(sprintf('%02d', 1:24), 'h'),
          panel=panel.levelplot.raster) +
    layer(sp.lines(boundaries, lwd=0.6))
dev.off()
