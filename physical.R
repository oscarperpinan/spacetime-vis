##################################################################
## Source code for the book: "Displaying time series, spatial and
## space-time data with R"
##
## Copyright (C) 2013-2012 Oscar Perpiñán Lamigueiro
##
## This program is free software you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published
## by the Free Software Foundation; either version 2 of the License,
## or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
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
## Physical maps
##################################################################

library(raster)
library(rasterVis)
library(maptools)
library(rgeos)
library(latticeExtra)
library(colorspace)

## Longitude-Latitude projection
proj <- CRS(' +proj=longlat +ellps=WGS84')

##################################################################
## Retrieving data from DIVA-GIS, GADM and Natural Earth Data
##################################################################

old <- setwd(tempdir())

download.file('http://biogeo.ucdavis.edu/data/gadm2/shp/BRA_adm.zip',
              'BRA_adm.zip')
unzip('BRA_adm.zip')
brazilAdm <- readShapePoly('BRA_adm1.shp', proj4string=proj)
Encoding(levels(brazilAdm$NAME_1)) <- 'latin1'

download.file('http://biogeo.ucdavis.edu/data/diva/alt/BRA_alt.zip',
              'BRA_alt.zip')
unzip('BRA_alt.zip')
brazilDEM <- raster('BRA_alt')

## World Water lines (Natural Earth)
download.file('http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_rivers_lake_centerlines.zip', 'neRivers.zip')
unzip('neRivers.zip')
worldRiv <- readShapeLines('ne_10m_rivers_lake_centerlines', proj4string = proj)

download.file('http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/OB_LR.zip', 'neSea.zip')
unzip('neSea.zip')
worldSea <- raster('OB_LR.tif')
brazilSea <- crop(worldSea, brazilDEM)
setwd(old)

##################################################################
## Intersection of shapefiles and elevation model
##################################################################

## only those features labelled as "River" are needed
worldRiv<- worldRiv[worldRiv$featurecla=='River',]

## Define the extent of Brazil as an SpatialPolygons
extBrazil <- as(extent(brazilDEM), 'SpatialPolygons')
proj4string(extBrazil) <- proj

## and intersect it with worldRiv to extract brazilian rivers
## from the world database
brazilRiv <- gIntersection(worldRiv, extBrazil, byid=TRUE)
## and specially the famous Amazonas River
amazonas <- worldRiv[worldRiv$name=='Amazonas',]

##################################################################
## Labels
##################################################################

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

##################################################################
## Overlaying layers of information
##################################################################

blueTheme <- rasterTheme(region=brewer.pal(n=9, 'Blues'))

seaPlot <- levelplot(brazilSea, par.settings=blueTheme,
                     maxpixels=1e6, panel=panel.levelplot.raster,
                     margin=FALSE, colorkey=FALSE)

terrainTheme <- rasterTheme(region=terrain_hcl(15))

altPlot <- levelplot(brazilDEM, par.settings=terrainTheme,
                     maxpixels=1e6, panel=panel.levelplot.raster,
                     margin=FALSE, colorkey=FALSE)

png(filename="figs/rastersBrazil.png",res=300,height=2000,width=2000)
print(seaPlot, split=c(1, 1, 2, 1), more=TRUE)
print(altPlot, split=c(2, 1, 2, 1))
dev.off()

amazonasLab <- label(amazonas, 'Amazonas')

png(filename="figs/brazil.png",res=300,height=2000,width=2000)
seaPlot + altPlot + layer({
    ## Rivers
    sp.lines(brazilRiv, col='darkblue', lwd=0.2)
    ## Amazonas
    sp.lineLabel(amazonas, amazonasLab,
                 lwd=1, col='darkblue', col.line='darkblue',
                 cex=0.5, fontfamily='Palatino')
    ## Administrative boundaries
    sp.polygons(brazilAdm, col='black', lwd=0.2)
    ## Centroids of administrative boundaries ...
    panel.points(centroids, col='black')
    ## ... with their labels
    panel.pointLabel(centroids, labels=admNames,
                     cex=0.7, fontfamily='Palatino', lineheight=.8)
    ## Country name
    panel.text(xyBrazil[1], xyBrazil[2], labels='B R A Z I L',
               cex=1.5, fontfamily = 'Palatino', fontface=2)
})
dev.off()
