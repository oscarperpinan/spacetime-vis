
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
library(ggplot2)
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

library(raster)
library(rasterVis)

old <- setwd(tempdir())
download.file('ftp://tgftp.nws.noaa.gov/SL.us008001/ST.expr/DF.gr2/DC.ndfd/AR.conus/VP.001/ds.wdir.bin', 'windDir.bin')
download.file('ftp://tgftp.nws.noaa.gov/SL.us008001/ST.expr/DF.gr2/DC.ndfd/AR.conus/VP.001/ds.wspd.bin', 'windSpeed.bin')
  
wDir <- raster('windDir.bin')/180*pi
wSpeed <- raster('windSpeed.bin')

setwd(old)

idxNA <- (wDir == 9999) | (wSpeed == 9999) | (wSpeed <= 0)

wDir[idxNA] <- NA
wSpeed[idxNA] <- NA

wind <- stack(wSpeed, wDir)

levelplot(wind, layers='windSpeed',
          par.settings=BTCTheme(),
          scales=list(draw=FALSE))

library(car)

boxCox <- function(x){
  lambda <- powerTransform(x~1)
  res <- bcPower(x, coef(lambda))
  }

wSpeed[] <- boxCox(wSpeed[])

windField <- stack(wSpeed, wDir)
names(windField) <- c('slope', 'aspect')

pdf(file="figs/vectorplot.pdf")
vectorplot(windField, isField=TRUE, par.settings=BTCTheme(),
           colorkey=FALSE, scales=list(draw=FALSE))

dev.off()

pdf(file="figs/streamplot.pdf")
streamplot(windField, isField=TRUE,
           droplet=list(pc=.2), streamlet=list(L=25),
           scales=list(draw=FALSE))
dev.off()

altUSA <- raster('~/Datos/USA_msk_alt/USA1_msk_alt')

pdf(file="figs/altUSA.pdf")
levelplot(altUSA, par.settings=BTCTheme())
dev.off()
