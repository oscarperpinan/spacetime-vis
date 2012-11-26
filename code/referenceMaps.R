
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

## Set folder to where the local copy of github repository can be found
setwd('~/Dropbox/chapman/book/')

##################################################################
## Reference and Physical Maps
##################################################################

##################################################################
## OpenStreetMap with Hill Shade layers
##################################################################

library('osmar')

api <- osmsource_api()
ymax <- 43.7031
ymin <- 43.6181
xmax <- -8.0224
xmin <- -8.0808
box <- corner_bbox(xmin, ymin, xmax, ymax)
cedeira <- get_osm(box, source=api, full=TRUE)
summary(cedeira)

summary(cedeira$nodes)

idxHighways <- find(cedeira, way(tags(k=='highway')))
highways <- subset(cedeira, way_ids=idxHighways)
idxStreets <- find(highways, way(tags(v=='residential')))
idxPrimary <- find(highways, way(tags(v=='primary')))
idxSecondary <- find(highways, way(tags(v=='secondary')))
idxTertiary <- find(highways, way(tags(v=='tertiary')))
idxOther <- find(highways,
                 way(tags(v=='unclassified' |
                          v=='footway' |
                          v=='steps')))

spFromOSM <- function(source, index, type='lines'){
  idx <- find_down(source, index)
  obj <- subset(source, ids=idx)
  objSP <- as_sp(obj, type)
  }

streets <- spFromOSM(cedeira, way(idxStreets))
primary <- spFromOSM(cedeira, way(idxPrimary))
secondary <- spFromOSM(cedeira, way(idxSecondary))
tertiary <- spFromOSM(cedeira, way(idxTertiary))
other <- spFromOSM(cedeira, way(idxOther))

idxPlaces <- find(cedeira, node(tags(k=='name')))
places <- spFromOSM(cedeira, node(idxPlaces), 'points')

nms <- subset(cedeira$nodes$tags, subset=(k=='name'), select=c('id', 'v'))
ord <- match(idxPlaces, nms$id)
nms <- nms[ord,]
places$name <- nms$v[ord]

## Cedeira town will be printed differently
idxCedeira <- which(nms$v=='Cedeira') ##Main town
cedeiraCoords <- coordinates(places[idxCedeira,])
places <- places[-idxCedeira,]

library(raster)
## Galicia DEM
## http://ide.unex.es/geonetwork/srv/es/main.search?any=MDE_Galicia
## http://ide.unex.es:8180/geonetwork/srv/es/resources.get?id=21&fname=dem_gal.7z&access=private

old <- tempdir()
download.file('http://ide.unex.es:8180/geonetwork/srv/es/resources.get?id=21&fname=dem_gal.7z&access=private', 'dem_gal.7z')
unzip('dem_gal.7z')
demGalicia <- raster('dem_gal.asc')
setwd(old)

cedeiraSP <- as_sp(cedeira, 'points')
projCedeira <- projection(cedeiraSP)
##extCedeira <- bbox(cedeiraSP) 
## or summary(cedeira$nodes)$bbox
extCedeira <- extent(-8.15, -7.95, 43.6, 43.75)
demCedeira <- crop(demGalicia, extCedeira)
projection(demCedeira) <- projCedeira
demCedeira[demCedeira <= 0] <- NA

slope <- terrain(demCedeira, 'slope')
aspect <- terrain(demCedeira, 'aspect')
hsCedeira <- hillShade(slope=slope, aspect=aspect,
                       angle=20, direction=30)

pdf(file="figs/cedeiraOsmar.pdf")
library(maptools) ##0.8-21 install.packages("maptools", repos="http://R-Forge.R-project.org")
library(latticeExtra)
library(colorspace)
library(rasterVis)

##Auxiliary function to display the roads. A thicker black line in
##the background and a thinner one with an appropiate color.
sp.road <- function(line, lwd=5, blwd=7,
                    col='indianred1', bcol='black'){
  sp.lines(line, lwd=blwd, col=bcol)
  sp.lines(line, lwd=lwd, col=col)
}

## The background color of the panel is set to blue to represent the sea
hsTheme <- modifyList(GrTheme(), list(panel.background=list(col='skyblue3')))
## DEM with terrain colors and semitransparency
terrainTheme <- modifyList(rasterTheme(region=terrain_hcl(n=15)),
                                list(regions=list(alpha=0.6)))
## Hill shade and DEM overlaid
levelplot(hsCedeira, maxpixels=ncell(hsCedeira),
          par.settings=hsTheme, margin=FALSE, colorkey=FALSE) +
  levelplot(demCedeira, maxpixels=ncell(demCedeira),
            par.settings=terrainTheme) +
  ## Roads and places
  layer({
    ## Street and roads
    sp.road(streets, lwd=1, blwd=2, col='white')
    sp.road(other, lwd=2, blwd=3, col='white')
    sp.road(tertiary, lwd=3, blwd=4, col='palegreen')
    sp.road(secondary, lwd=4, blwd=6, col='midnightblue')
    sp.road(primary, col='indianred1')
    ## Places except Cedeira town
    sp.points(places, pch=19, col='black', cex=0.4, alpha=0.8)
    sp.pointLabel(places, labels=places$name,
                      fontfamily = 'Palatino', 
                      cex=0.6, col='black')
    ## Cedeira town
    panel.points(cedeiraCoords, pch=18, col='black', cex=1)
    panel.text(cedeiraCoords, labels='Cedeira', pos=2, offset=1)
    })
dev.off()

##################################################################
## Physical maps
##################################################################

library(raster)
library(rasterVis)
library(maptools) ## 0.8-21 install.packages("maptools", repos="http://R-Forge.R-project.org")
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
