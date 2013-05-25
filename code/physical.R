
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

##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

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
library(maptools)
library(latticeExtra)
library(colorspace)

old <- setwd(tempdir())
  
## Longitude-Latitude projection
proj <- CRS(' +proj=longlat +ellps=WGS84')
  
## Administrative boundaries (GADM)
download.file('http://www.gadm.org/data/shp/BRA_adm.zip', 'BRA_adm.zip')
unzip('BRA_adm.zip')
brazilAdm <- readShapePoly('BRA_adm1.shp', proj4string=proj)
Encoding(levels(brazilAdm$NAME_1)) <- 'latin1'

## Altitude raster (DIVA-GIS)
download.file('http://www.diva-gis.org/data/alt/BRA_alt.zip', 'BRA_alt.zip')
unzip('BRA_alt.zip')
brazilAlt <- raster('BRA_alt')

## World Water lines (Natural Earth)
download.file('http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_rivers_lake_centerlines.zip', 'neRivers.zip')
unzip('neRivers.zip')
worldlRiv <- readShapeLines('ne_10m_rivers_lake_centerlines', proj4string = proj)

## Sea depth (Natural Earth)
download.file('http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/OB_LR.zip', 'neSea.zip')
unzip('neSea.zip')
worldSea <- raster('OB_LR.tif')
brazilSea <- crop(worldSea, brazilAlt)

setwd(old)

## only those features labelled as "River" are needed
worlRiv<- worlRiv[worlRiv$featurecla=='River',]

## Helper function that calculates the extent of a line to crop it
## with an extent object using intersect
extentLine <- function(line){
  bb <- bbox(line)
  e <- extent(c(bb[1,1], bb[1,2], bb[2,1], bb[2,2]))
  e
}

extBrazil <- extent(brazilAlt)
## For each line test if it is inside Brazil
intersectBrazil <- lapply(worlRiv@lines, function(line){
  extLine <- extentLine(line)
  int <- intersect(extentLine(line), extBrazil)
})

inBrazil <- which(sapply(intersectBrazil, function(x)!is.null(x)))
## Extract rivers from the world database
brazilRiv <- worlRiv[inBrazil,]
## and specially the famous Amazonas River
amazonas <- brazilRiv[brazilRiv$name=='Amazonas',]

## Locations of labels of each polygon
centroids <- coordinates(brazilAdm)
## Location of the "Brazil" label (average of the set of polygons centroids)
xyBrazil <- apply(centroids, 2, mean)

admNames <- strsplit(as.character(brazilAdm$NAME_1), ' ')

admNames <- sapply(admNames,
                 FUN=function(s){
                   sep=if (length(s)>2) '\n' else  ' '
                   paste(s, collapse=sep)
                   })

blueTheme <- rasterTheme(region=brewer.pal(n=9, 'Blues'))

terrainTheme <- rasterTheme(region=terrain_hcl(15))

pdf(file="figs/brazil.pdf")
labs <- label(amazonas, 'Amazonas')

seaPlot <- levelplot(brazilSea, par.settings=blueTheme,
                     maxpixels=1e6, panel=panel.levelplot.raster,
                     margin=FALSE, colorkey=FALSE)

altPlot <- levelplot(brazilAlt, par.settings=terrainTheme,
                     maxpixels=1e6, panel=panel.levelplot.raster,
                     margin=FALSE, colorkey=FALSE)

seaPlot + altPlot + layer({
  sp.lines(brazilRiv, col='darkblue', lwd=0.3)
    
  sp.lineLabel(amazonas, labs, 
               lwd=1.3, col='darkblue', col.line='darkblue',
               cex=0.5, fontfamily='Palatino')
               
  
  sp.polygons(brazilAdm, col='black', lwd=0.5)
  panel.text(coordinates(brazilAdm), labels=admNames,
             cex=0.5, fontfamily='Courier', lineheight=.8)
  
  panel.text(xyBrazil[1], xyBrazil[2], labels='B R A Z I L',
             cex=1.5, fontfamily = 'Palatino', fontface=2)
  })
dev.off()
