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
## Data and spatial information
##################################################################

library(sp)

## Spatial location of stations
airStations <- read.csv2('data/airStations.csv')
## rownames are used as the ID of the Spatial object
rownames(airStations) <- substring(airStations$Codigo, 7)
coordinates(airStations) <- ~ long + lat
proj4string(airStations) <- CRS("+proj=longlat +ellps=WGS84")
## Measurements data
airQuality <- read.csv2('data/airQuality.csv')
## Only interested in NO2 
NO2 <- airQuality[airQuality$codParam==8, ]

library(zoo)
library(spacetime)

NO2$time <- with(NO2, ISOdate(year, month, day))
NO2wide <- reshape(NO2[,c('codEst', 'dat', 'time')],
                   idvar='time', timevar='codEst',
                   direction='wide')
NO2zoo <- zoo(NO2wide[,-1], NO2wide$time)

dats <- data.frame(vals=as.vector(t(NO2zoo)))
NO2st <- STFDF(airStations, index(NO2zoo), dats)

##################################################################
## Graphics with spacetime
##################################################################

pdf(file="figs/NO2STxy.pdf")
airPal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(5)

stplot(NO2st[, 1:12], cuts=5, col.regions=airPal, edge.col='black')
dev.off()

pdf(file="figs/NO2hovmoller.pdf")
stplot(NO2st, mode='xt', col.regions=colorRampPalette(airPal)(15),
       scales=list(x=list(rot=45)), ylab='', xlab='')
dev.off()

png(filename="figs/NO2zoo.png",res=300,height=2000,width=2000)
stplot(NO2st, mode='ts', xlab='',
       lwd=0.1, col='black', alpha=0.6,
       auto.key=FALSE)
dev.off()

##################################################################
## Animation
##################################################################

##################################################################
## Initial snapshot
##################################################################

library(gridSVG)
## Initial parameters
start <- NO2st[,1]
## values will be encoded as size of circles,
## so we need to scale them
startVals <- start$vals/5000

nStations <- nrow(airStations)
days <- index(NO2zoo)
nDays <- length(days)
## Duration in seconds of the animation
duration <- nDays*.3

library(grid)

## Auxiliar panel function to display circles
panel.circlesplot <- function(x, y, cex, col='gray',
                              name='stationsCircles', ...){
  grid.circle(x, y, r=cex,
              gp=gpar(fill=col, alpha=0.5),
              default.units='native',
              name=name)
  }

pStart <- spplot(start, panel=panel.circlesplot,
                 cex=startVals,
                 scales=list(draw=TRUE), auto.key=FALSE)
pStart

##################################################################
## Intermediate states to create the animation
##################################################################

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

##################################################################
## Time reference: progress bar
##################################################################

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
## Pause animations when mouse is over the progress bar
grid.garnish('bgbar',
             onmouseover='document.rootElement.pauseAnimations()',
             onmouseout='document.rootElement.unpauseAnimations()')

grid.export('figs/NO2pb.svg')

##################################################################
## Time reference: a time series plot
##################################################################

## Time series with average value of the set of stations
NO2mean <- zoo(rowMeans(NO2zoo, na.rm=TRUE), index(NO2zoo))
## Time series plot with position highlighted
pTimeSeries <- xyplot(NO2mean, xlab='', identifier='timePlot') +
    layer({
        grid.points(0, .5, size=unit(.5, 'char'),
                    default.units='npc',
                    gp=gpar(fill='gray'),
                    name='locator')
        grid.segments(0, 0, 0, 1, name='vLine')
    })

print(pStart, position=c(0, .2, 1, 1), more=TRUE)
print(pTimeSeries, position=c(.1, 0, .9, .25))

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

## Pause animations when mouse is over the time series plot
grid.garnish('timePlot', grep=TRUE,
             onmouseover='document.rootElement.pauseAnimations()',
             onmouseout='document.rootElement.unpauseAnimations()')

grid.export('figs/vLine.svg')
