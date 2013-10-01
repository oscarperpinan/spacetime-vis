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

##################################################################
## Vector fields
##################################################################

library(raster)
library(rasterVis)

wDir <- raster('data/wDir')/180*pi
wSpeed <- raster('data/wSpeed')
windField <- stack(wSpeed, wDir)
names(windField) <- c('magnitude', 'direction')

##################################################################
## Arrow plot
##################################################################

pdf(file="figs/vectorplot.pdf")
vectorplot(windField, isField=TRUE, par.settings=BTCTheme(),
           colorkey=FALSE, scales=list(draw=FALSE))
dev.off()

##################################################################
## Stream lines
##################################################################

pdf(file="figs/streamplot.pdf")
myTheme <- streamTheme(region=rev(brewer.pal(n=4, name='Greys')),
                                    symbol=BTC(n=9, beg=20))
streamplot(windField, isField=TRUE,
           par.settings=myTheme,
           droplet=list(pc=12),
           streamlet=list(L=5, h=5),
           scales=list(draw=FALSE),
           panel=panel.levelplot.raster)
dev.off()
