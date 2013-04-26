
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

library(sp)

airStations <- read.csv2('data/airStations.csv')

coordinates(airStations) <- ~ long + lat
proj4string(airStations) <- CRS("+proj=longlat +ellps=WGS84")

airQuality <- read.csv2('data/airQuality.csv')

NO2 <- airQuality[airQuality$codParam==8, ]

splitNO2 <- split(NO2, NO2$codEst)
listNO2 <- lapply(splitNO2, FUN=function(l){
  tt <- with(l, as.POSIXct(paste(year, month, day, sep='-')))
  zoo(l$dat, tt)
  })
## cbind.zoo merges the list of zoo's including all the time
## indexes in the output (all=TRUE)
NO2zoo <- do.call(cbind, listNO2)

xyplot(NO2zoo,
       lwd=0.4, col='black', alpha=0.3,
       superpose=TRUE,
       auto.key=FALSE)

library(spacetime)

NO2st <- STFDF(airStations, index(NO2zoo),
               data.frame(vals=c(t(NO2zoo))))

stplot(NO2st[, 1:10], cuts=5, col.regions=airPal)

library(gridSVG)
## Initial display
start <- NO2st[,1]
startVals <- start$vals/5000

nStations <- nrow(airStations)
days <- index(NO2zoo)
nDays <- length(days)
## Duration in seconds of the animation
duration <- nDays*.3

## Auxiliar panel function to display circles
panel.circlesplot <- function(x, y, cex, col='gray',
                              name='stationsCircles', ...){
  grid.circle(x, y, r=cex,
              gp=gpar(fill=col, alpha=0.5),
              default.units='native',
              name=name)
  }

spplot(start, panel=panel.circlesplot,
       cex=startVals,
       scales=list(draw=TRUE), auto.key=FALSE)
 
## Progress bar
prettyDays <- pretty(days, 12)
## Width of the progress bar
pbWidth <- .95
## Background
grid.rect(.5, 0.01, width=pbWidth, height=.01,
          just=c('center', 'bottom'),
          name='bgbar', gp=gpar(fill='gray'))

## Width of the progress bar for each day
dayWidth <- pbWidth/nDays
ticks <- c(0, cumsum(as.numeric(diff(prettyDays)))*dayWidth) + .025
grid.segments(ticks, .01, ticks, .02)
grid.text(format(prettyDays, '%d-%b'),
                 ticks, .03, gp=gpar(cex=.5))
## Initial display of the progress bar
grid.rect(.025, .01, width=0,
          height=.01, just=c('left', 'bottom'),
          name='pbar', gp=gpar(fill='blue', alpha='.3'))
## ...and its animation
grid.animate('pbar', duration=duration,
             width=seq(0, pbWidth, length=duration),
             rep=TRUE)


## Color to distinguish between weekdays ('green')
## and weekend ('blue')
isWeekend <- function(x) {format(x, '%w') %in% c(0, 6)}
color <- ifelse(isWeekend(days), 'blue', 'green')
colorAnim <- animValue(rep(color, each=nStations),
                       id=rep(seq_len(nStations), nDays))

## Intermediate sizes of the circles
vals <- NO2st$vals/5000
vals[is.na(vals)] <- 0
radius <- animUnit(unit(vals, 'native'),
                       id=rep(seq_len(nStations), nDays))                     

## Animation of circles including sizes and colors
grid.animate('stationsCircles',
             duration=duration,
             r=radius,
             fill=colorAnim,
             rep=TRUE)

## Pause animations when mouse is over the progress bar
grid.garnish('bgbar',
             onmouseover='document.rootElement.pauseAnimations()',
             onmouseout='document.rootElement.unpauseAnimations()')

gridToSVG('figs/NO2pb.svg')

p1 <- spplot(start, panel=panel.circlesplot,
            cex=startVals,
            scales=list(draw=TRUE), auto.key=FALSE)


NO2mean <- zoo(rowMeans(NO2zoo, na.rm=TRUE), index(NO2zoo))

p2 <- xyplot(NO2mean, xlab='', identifier='timePlot') +
  layer({
    grid.points(0, .5, size=unit(.5, 'char'),
                    default.units='npc',
                    gp=gpar(fill='gray'),
                    name='locator')
    grid.segments(0, 0, 0, 1, name='vLine')
    })

print(p1, position=c(0, .2, 1, 1), more=TRUE)
print(p2, position=c(.1, 0, .9, .25))

grid.animate('locator',
             x=unit(as.numeric(index(NO2zoo)), 'native'),
             y=unit(as.numeric(NO2mean), 'native'),
             duration=duration, rep=TRUE)

xLine <- unit(index(NO2zoo), 'native')

grid.animate('vLine',
             x0=xLine, x1=xLine,
             duration=duration, rep=TRUE)

grid.animate('stationsCircles',
             duration=duration,
             r=radius,
             fill=colorAnim,
             rep=TRUE)

## Pause animations when mouse is over the progress bar
grid.garnish('timePlot', grep=TRUE,
             onmouseover='document.rootElement.pauseAnimations()',
             onmouseout='document.rootElement.unpauseAnimations()')

gridToSVG('figs/vLine.svg')
