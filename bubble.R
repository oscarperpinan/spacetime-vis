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

##################################################################
## Proportional symbol mapping
##################################################################

##################################################################
## Introduction
##################################################################

##################################################################
## Proportional symbol with spplot
##################################################################

library(sp)

load('data/NO2sp.RData')

pdf(file="figs/airMadrid_spplot.pdf")
airPal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(5)

spplot(NO2sp["mean"], col.regions=airPal, cex=sqrt(1:5),
       edge.col='black', scales=list(draw=TRUE),
       key.space='right')
dev.off()

NO2df <- data.frame(NO2sp)
NO2df$Mean <- cut(NO2sp$mean, 5)

ggplot(data=NO2df, aes(long, lat, size=Mean, fill=Mean)) +
    geom_point(pch=21, col='black') + theme_bw() +
    scale_fill_manual(values=airPal)

##################################################################
## Optimal classification and sizes to improve discrimination
##################################################################

library(classInt)
## The number of classes is chosen between the Sturges and the
## Scott rules.
nClasses <- 5
intervals <- classIntervals(NO2sp$mean, n=nClasses, style='fisher')
## Number of classes is not always the same as the proposed number
nClasses <- length(intervals$brks) - 1

op <- options(digits=4)
tab <- print(intervals)
options(op)

## Complete Dent set of circle radii (mm)
dent <- c(0.64, 1.14, 1.65, 2.79, 4.32, 6.22, 9.65, 12.95, 15.11)
## Subset for our dataset
dentAQ <- dent[seq_len(nClasses)]
## Link Size and Class: findCols returns the class number of each
## point; cex is the vector of sizes for each data point
idx <- findCols(intervals)
cexNO2 <- dentAQ[idx]

NO2sp$classNO2 <- factor(names(tab)[idx])

## ggplot2 version
NO2df <- data.frame(NO2sp)

ggplot(data=NO2df, aes(long, lat, size=classNO2, fill=classNO2)) +
    geom_point(pch=21, col='black') + theme_bw() +
    scale_fill_manual(values=airPal) +
    scale_size_manual(values=dentAQ*2)

pdf(file="figs/airMadrid_classes.pdf")
## spplot version

## Definition of an improved key with title and background
NO2key <- list(x=0.98, y=0.02, corner=c(1, 0),
              title=expression(NO[2]~~(paste(mu, plain(g))/m^3)),
              cex.title=.75, cex=0.7,
              background='gray92')

pNO2 <- spplot(NO2sp["classNO2"],
               col.regions=airPal,  cex=dentAQ,
               edge.col='black',
               scales=list(draw=TRUE),
               key.space=NO2key)
pNO2
dev.off()

##################################################################
## Spatial context with underlying layers and labels
##################################################################

##################################################################
## Static image
##################################################################

madridBox <- bbox(NO2sp)

## ggmap solution
library(ggmap)
madridGG <- get_map(c(madridBox), maptype='watercolor', source='stamen')

## OpenStreetMap solution
library(OpenStreetMap)
ul <- madridBox[c(4, 1)]
lr <- madridBox[c(2, 3)]
madridOM <- openmap(ul, lr, type='stamen-watercolor')
madridOM <- openproj(madridOM)

NO2df <- data.frame(NO2sp)

## ggmap
ggmap(madridGG) +
    geom_point(data=NO2df,
               aes(long, lat, size=classNO2, fill=classNO2),
               pch=21, col='black') +
       scale_fill_manual(values=airPal) +
       scale_size_manual(values=dentAQ*2)

##OpenStreetMap
autoplot(madridOM) + 
    geom_point(data=NO2df,
               aes(long, lat, size=classNO2, fill=classNO2),
               pch=21, col='black') +
    scale_fill_manual(values=airPal) +
    scale_size_manual(values=dentAQ*2)

pdf(file="figs/airMadrid_stamen.pdf")
## the 'bb' attribute stores the bounding box of the get_map result
bbMap <- attr(madridGG, 'bb')
## This information is needed to resize the image with grid.raster
height <- with(bbMap, ur.lat - ll.lat)
width <- with(bbMap, ur.lon - ll.lon)

pNO2 + layer(grid.raster(madridGG,
                          width=width, height=height,
                          default.units='native'),
             under=TRUE)
dev.off()

tile <- madridOM$tile[[1]]

height <- with(tile$bbox, p1[2] - p2[2])
width <- with(tile$bbox, p2[1] - p1[1])

colors <- as.raster(matrix(tile$colorData,
                           ncol=tile$yres,
                           nrow=tile$xres,
                           byrow=TRUE))

pNO2 + layer(grid.raster(colors,
                         width=width,
                         height=height,
                         default.units='native'),
             under=TRUE)

##################################################################
## Vector data
##################################################################

library(maptools)
library(rgdal)
  
## nomecalles http://www.madrid.org/nomecalles/Callejero_madrid.icm
## Form at http://www.madrid.org/nomecalles/DescargaBDTCorte.icm

## Madrid districts
unzip('Distritos de Madrid.zip')
distritosMadrid <- readShapePoly('Distritos de Madrid/200001331')
proj4string(distritosMadrid) <- CRS("+proj=utm +zone=30")
distritosMadrid <- spTransform(distritosMadrid, CRS=CRS("+proj=longlat +ellps=WGS84"))

## Madrid streets
unzip('Callejero_ Ejes de viales.zip')
streets <- readShapeLines('Callejero_ Ejes de viales/call2011.shp')
streetsMadrid <- streets[streets$CMUN=='079',]
proj4string(streetsMadrid) <- CRS("+proj=utm +zone=30")
streetsMadrid <- spTransform(streetsMadrid, CRS=CRS("+proj=longlat +ellps=WGS84"))

spDistricts <- list('sp.polygons', distritosMadrid, fill='gray97', lwd=0.3)
spStreets <- list('sp.lines', streetsMadrid, lwd=0.05)
spNames <- list(sp.pointLabel, NO2sp,
                labels=substring(NO2sp$codEst, 7),
                cex=0.6, fontfamily='Palatino')

spplot(NO2sp["classNO2"], col.regions=airPal, cex=dentAQ,
       edge.col='black', alpha=0.8,
       sp.layout=list(spDistricts, spStreets, spNames),
       scales=list(draw=TRUE),
       key.space=NO2key)

png(filename="figs/airMadrid.png",res=600,height=4000,width=4000)
pNO2 +
    layer(sp.pointLabel(NO2sp,
                        labels=substring(NO2sp$codEst, 7),
                        cex=0.8, fontfamily='Palatino')
          ) +
    layer_({
        sp.polygons(distritosMadrid, fill='gray97', lwd=0.3)
        sp.lines(streetsMadrid, lwd=0.05)
    })
dev.off()

##################################################################
## Spatial interpolation
##################################################################

library(gstat)

airGrid <- spsample(NO2sp, type='regular', n=1e5)
gridded(airGrid) <- TRUE
airKrige <- krige(mean ~ 1, NO2sp, airGrid)

png(filename="figs/airMadrid_krige.png",res=600,height=4000,width=4000)
spplot(airKrige["var1.pred"],
       col.regions=colorRampPalette(airPal)) +
  layer({
    sp.polygons(distritosMadrid, fill='transparent', lwd=0.3)
    sp.lines(streetsMadrid, lwd=0.07)
    sp.points(NO2sp, pch=21, alpha=0.8, fill='gray50', col='black')
    })
dev.off()

##################################################################
## GeoJSON and OpenStreepMap
##################################################################

library(rgdal)
writeOGR(NO2sp, 'data/NO2.geojson', 'NO2sp', driver='GeoJSON')

library(rgdal)
writeOGR(NO2sp, dsn='NO2_mean.kml', layer='mean', driver='KML')

library(plotKML)
plotKML(NO2sp["mean"], points_names=NO2sp$codEst)

##################################################################
## Additional information with tooltips and hyperlinks
##################################################################

library(XML)

old <- setwd('images')
for (i in 1:nrow(NO2df)){
  codEst <- NO2df[i, "codEst"]
  ## Webpage of each station
  codURL <- as.numeric(substr(codEst, 7, 8))
  rootURL <- 'http://www.mambiente.munimadrid.es'
  stationURL <- paste(rootURL,
                      '/opencms/opencms/calaire/contenidos/estaciones/estacion',
                      codURL, '.html', sep='')
  content <- htmlParse(stationURL, encoding='utf8')
  ## Extracted with http://www.selectorgadget.com/
  xPath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "imagen_1", " " ))]'
  imageStation <- getNodeSet(content, xPath)[[1]]
  imageURL <- xmlAttrs(imageStation)[1]
  imageURL <- paste(rootURL, imageURL, sep='')
  download.file(imageURL, destfile=paste(codEst, '.jpg', sep=''))
}
setwd(old)

print(pNO2 + layer_(sp.polygons(distritosMadrid, fill='gray97', lwd=0.3)))

library(gridSVG)

NO2df <- as.data.frame(NO2sp)

tooltips <- sapply(seq_len(nrow(NO2df)), function(i){
  codEst <- NO2df[i, "codEst"]
  ## Information to be attached to each line
  stats <- paste(c('Mean', 'Median', 'SD'),
                 signif(NO2df[i, c('mean', 'median', 'sd')], 4),
                 sep=' = ', collapse='<br />')
  ## Station photograph 
  imageURL <- paste('images/', codEst, '.jpg', sep='')
  imageInfo <- paste("<img src=", imageURL,
                     " width='100' height='100' />", sep='')
  ## Text to be included in the tooltip
  nameStation <- paste('<b>', 
                       as.character(NO2df[i, "Nombre"]),
                       '</b>', sep='')
  info <- paste(nameStation, stats, sep='<br />')
  ## Tooltip includes the image and the text
  paste(imageInfo, info, sep='<br />')
})
grid.garnish('points.panel', title=tooltips,  grep=TRUE, group=FALSE)

## Webpage of each station
rootURL <- 'http://www.mambiente.munimadrid.es'
urlList <- sapply(seq_len(nrow(NO2df)), function(i){
  codEst <- NO2df[i, "codEst"]
  codURL <- as.numeric(substr(codEst, 7, 8))
  stationURL <- paste(rootURL,
                      '/opencms/opencms/calaire/contenidos/estaciones/estacion',
                      codURL, '.html', sep='')
  })

grid.hyperlink('points.panel', urlList, grep=TRUE, group=FALSE)

## Add jQuery and jQuery UI scripts
grid.script(file='http://code.jquery.com/jquery-1.8.3.js')
grid.script(file='http://code.jquery.com/ui/1.9.2/jquery-ui.js')
## Simple JavaScript code to initialize the tooltip
grid.script(file='js/myTooltip.js')
## Produce the SVG graphic: the results of grid.garnish,
## grid.hyperlink and grid.script are converted to SVG code
grid.export('figs/airMadrid.svg')

htmlBegin <- '<!DOCTYPE html>
<html>
<head>
<title>Tooltips with jQuery and gridSVG</title>
<link rel="stylesheet" type="text/css" href="http://code.jquery.com/ui/1.9.2/themes/smoothness/jquery-ui.css" />
<meta charset="utf-8">
</head>
<body>'

htmlEnd <- '</body> </html>'
  
svgText <- paste(readLines('figs/airMadrid.svg'), collapse='\n')
  
writeLines(paste(htmlBegin, svgText, htmlEnd, sep='\n'),
           'airMadrid.html')
