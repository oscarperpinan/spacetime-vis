
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

setwd('~/Dropbox/chapman/book/')

##################################################################
## Thematic maps
##################################################################

##################################################################
## Proportional symbol mapping
##################################################################

library(colorspace)
library(classInt)
library(sp)
library(maptools)
library(rgdal)

airStations <- read.csv2('data/airStations.csv')
coordinates(airStations) <- ~ long + lat
proj4string(airStations) <- CRS("+proj=longlat +ellps=WGS84")

airQuality <- read.csv2('data/airQuality.csv')

NO2 <- airQuality[airQuality$codParam==8, ]

summarize <- function(formula, data,
                      FUN=function(x)c(mean=mean(x), median=median(x), sd=sd(x)),
                      ...){
  agg <- aggregate(formula, data, FUN=FUN, ...)
  data.frame(do.call(cbind, agg))
}
  
NO2agg <- summarize(dat ~ codEst, data=NO2)

## Codigo and codEst are the stations codes
idxNO2 <- match(airStations$Codigo, NO2agg$codEst)
airStations <- spCbind(airStations, NO2agg[idxNO2, ])

nClasses <- 5
intervals <- classIntervals(NO2agg$mean, n=nClasses, style='fisher')
## Number of classes is not always the same as the proposed number
nClasses <- length(intervals$brks) - 1

idx <- findCols(intervals)
size <- c(0.6, 1.5)
## Dent set of circle radii (mm)
dent <- c(0.64, 1.14, 1.65, 2.79, 4.32, 6.22, 9.65, 12.95, 15.11)
## Sizes for the legend
cex.key <- dent[seq_len(nClasses)]
## Sizes for each data point
cex <- cex.key[idx]

## Colours order is reversed to link larger values with darker
## colors. Palette tails are suppressed.
pal <- rev(LinOCS(n=nClasses, beg=50, end=200))
## Colours of the circles
col <- findColours(intervals, pal)
## Colours for the legend
col.key <- pal

panel.circles <- function(x, y, col, radius, identifier,...){
  radius <- unit(radius, 'mm')
  ## grid.circle is vectorized, it is able to plot the set of
  ## circle without the need of a for loop. However, it assigns an
  ## unique identifier for the set of circles. Since we will later
  ## add XML attributes to the circles with gridSVG, we need
  ## different identifiers for each circle.
  for (i in seq_along(x)){
  grid.circle(x[i], y[i], radius[i],
              default.units='native',
              name = paste('Station', identifier[i], sep='.'),
              gp=gpar(col=col[i],
                fill=adjustcolor(col[i], alpha=.85),
                lwd=1, ...)
              )
  }}

## Labels for each group.
tabInt <- getFromNamespace('tableClassIntervals', 'classInt')
## Only two significative digits
op <- options(digits=2)
## Arguments of this function can be understood inspecting the
## code of classInt:::print.classIntervals
tab <- tabInt(cols = idx, brks = intervals$brks,
              under = "under", over = "over", between = "-", 
              cutlabels = TRUE,
              intervalClosure = "left",
              dataPrecision = NULL)
options(op)
## Key list for the xyplot function
key <- list(x=0.02, y=0.02, corner = c(0, 0),
            title=expression(NO[2]~~(paste(mu, plain(g))/m^3)),
            cex.title=.95,
            background='gray92', 
            text=list(labels=names(tab), cex=.9),
            points=list(col=col.key, pch=19, cex=cex.key*.6, alpha=0.9))

longlatScales <- getFromNamespace('longlat.scales', 'sp')
bbExpand <- getFromNamespace('bbexpand', 'sp')
scales <- longlatScales(airStations,
                        scales=list(draw = TRUE, cex=0.7),
                        xlim = bbExpand(bbox(airStations)[1, ], 0.2),
                        ylim = bbExpand(bbox(airStations)[2, ], 0.1))

pCircles <- xyplot(lat ~ long, data=as.data.frame(airStations),
            xlab='', ylab='', main='',
            cex=cex, col=col, radius=cex,
            key=key, identifier = airStations$codEst,
            asp=mapasp(airStations), scales=scales,
            panel=panel.circles)

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

distritosMadrid <- readShapePoly('~/Datos/nomecalles/Distritos de Madrid/200001331')
proj4string(distritosMadrid) <- CRS("+proj=utm +zone=30")
distritosMadrid <- spTransform(distritosMadrid, CRS=CRS("+proj=longlat +ellps=WGS84"))

## streets <- readShapeLines('~/Datos/nomecalles/Callejero_ Ejes de viales/call2011.shp')
## streetsMadrid <- streets[streets$CMUN=='079',]
## proj4string(streetsMadrid) <- CRS("+proj=utm +zone=30")
## streetsMadrid <- spTransform(streetsMadrid, CRS=CRS("+proj=longlat +ellps=WGS84"))
## writeLinesShape(streetsMadrid, '~/Datos/nomecalles/Callejero_ Ejes de viales/streetsMadrid')

streetsMadrid <- readShapeLines('~/Datos/nomecalles/Callejero_ Ejes de viales/streetsMadrid.shp')
proj4string(streetsMadrid) <- CRS("+proj=longlat +ellps=WGS84")

png(filename="figs/airMadrid.png",res=600,height=4000,width=4000)
p <- pCircles +
  layer_({
    sp.polygons(distritosMadrid, fill='gray97', lwd=0.3)
    sp.lines(streetsMadrid, lwd=0.05)
  }) +
  layer(sp.pointLabel(airStations, labels=airStations$Nombre,
                        cex=0.6, fontfamily='Palatino'))
print(p)
dev.off()

library(gridSVG)

print(pCircles)

dat <- as.data.frame(airStations)

for (i in 1:nrow(airStations)){
  ## Information to be attached to each line
  stats <- paste(c('Mean', 'Median', 'SD'),
                 signif(dat[i, c('mean', 'median', 'sd')], 4),
                 sep=' = ', collapse='; ')
  
  nameStation <- as.character(dat[i, "Nombre"])
  info <- paste(nameStation, stats, sep=': ')
  idStation <- paste('Station', dat[i, "codEst"], sep='.')
  ## attach SVG attributes
  grid.garnish(idStation, title=info)

  codURL <- as.numeric(substr(dat[i, "codEst"], 7, 8))
  URL <- paste('http://www.mambiente.munimadrid.es/opencms/opencms/calaire/contenidos/estaciones/estacion', codURL, '.html', sep='')
  grid.hyperlink(idStation, URL)
}

gridToSVG('figs/airMadrid.svg')

##################################################################
## Multivariate choropleth maps
##################################################################

votes2011 <- read.csv('data/votes2011.csv',
                      colClasses=c('factor', 'factor', 'numeric', 'numeric'))

##################################################################
## Administrative boundaries
##################################################################

library(maps)
library(maptools)
library(sp)
library(lattice)
library(latticeExtra)
library(colorspace)

old <- setwd(tempdir())
download.file('http://goo.gl/TIvr4', 'mapas_completo_municipal.rar')
system2('unrar', c('e', 'mapas_completo_municipal.rar'))
espMap <- readShapePoly(fn="esp_muni_0109")
Encoding(levels(espMap$NOMBRE)) <- "latin1"

provinces <- readShapePoly(fn="spain_provinces_ag_2")
setwd(old)

espMap <- readShapePoly(fn="~/Datos/mapas_completo_municipal/esp_muni_0109")
Encoding(levels(espMap$NOMBRE)) <- "latin1"

provinces <- readShapePoly(fn="~/Datos/mapas_completo_municipal/spain_provinces_ag_2")

espPols <- unionSpatialPolygons(espMap, espMap$PROVMUN) ##disolve repeated polygons

## Extract Canarias islands from the SpatialPolygons object
canarias <-  sapply(espPols@polygons, function(x)substr(x@ID, 1, 2) %in% c("35",  "38"))
peninsulaPols <- espPols[!canarias]
islandPols <- espPols[canarias]

## Shift the island extent box to position them at the bottom right corner
dy <- bbox(peninsulaPols)[2,1] - bbox(islandPols)[2,1]
dx <- bbox(peninsulaPols)[1,2] - bbox(islandPols)[1,2]
islandPols2 <- elide(islandPols, shift=c(dx, dy))
bbIslands <- bbox(islandPols2)
## Bind Peninsula (without islands) with shifted islands
espPols <- rbind(peninsulaPols, islandPols2)

## Match polygons and data using ID slot and PROVMUN column
IDs <- sapply(espPols@polygons, function(x)x@ID)
idx <- match(IDs, votes2011$PROVMUN)

##Places without information
idxNA <- which(is.na(idx))

##Information to be added to the SpatialPolygons object
dat2add <- votes2011[idx, ]

## SpatialPolygonsDataFrame uses row names to match polygons with data
row.names(dat2add) <- IDs
espMapVotes <- SpatialPolygonsDataFrame(espPols, dat2add)

## Drop those places without information
espMapVotes <- espMapVotes[-idxNA, ]

##################################################################
## Map
##################################################################

classes <- levels(factor(espMapVotes$whichMax))
nClasses <- length(classes)

pList <- lapply(1:nClasses, function(i){
  mapClass <- espMapVotes[espMapVotes$whichMax==classes[i],]
  step <- 360/nClasses ## distance between hues
  pal <- rev(sequential_hcl(16, h = (30 + step*(i-1))%%360)) ## hues equally spaced
  pClass <- spplot(mapClass['pcMax'], col.regions=pal, col='transparent',
                   at = seq(0, 100, by=20))
  })

p <- Reduce('+', pList)

##################################################################
## Legend
##################################################################

## Function to add a title to a legend
addTitle <- function(legend, title){
  titleGrob <- textGrob(title, gp=gpar(fontsize=8), hjust=1, vjust=1)
  ## retrieve the legend from the trellis object
  legendGrob <- eval(as.call(c(as.symbol(legend$fun), legend$args)))
  ## Layout of the legend WITH the title
  ly <- grid.layout(ncol=1, nrow=2,
                    widths=unit(0.9, 'grobwidth', data=legendGrob))
  ## Create a frame to host the original legend and the title
  fg <- frameGrob(ly, name=paste('legendTitle', title, sep='_'))
  ## Add the grobs to the frame
  pg <- packGrob(fg, titleGrob, row=2)
  pg <- packGrob(pg, legendGrob, row=1)
  }

## Access each trellis object from pList...
for (i in seq_along(classes)){
  ## extract the legend (automatically created by spplot)...
  lg <- pList[[i]]$legend$right
  ## ...supress labels except from the last legend...
  lg$args$key$labels$cex=ifelse(i==nClasses, 0.8, 0) 
  ## ... and add the addTitle function to the legend component of each trellis object
  pList[[i]]$legend$right <- list(fun='addTitle',
                                  args=list(legend=lg, title=classes[i]))
}

## List of legends
legendList <- lapply(pList, function(x){
  lg <- x$legend$right
  clKey <- eval(as.call(c(as.symbol(lg$fun), lg$args)))
  clKey
})

## Function to pack the list of legends in a unique legend
## Adapted from latticeExtra::: mergedTrellisLegendGrob
packLegend <- function(legendList){
  N <- length(legendList)
  ly <- grid.layout(nrow = 1,  ncol = N)
  g <- frameGrob(layout = ly, name = "mergedLegend")
  for (i in 1:N) g <- packGrob(g, legendList[[i]], col = i)
  g
}

## The legend of p will include all the legends
p$legend$right <- list(fun = 'packLegend',  args = list(legendList = legendList))

pdf(file="figs/mapLegends.pdf")
canarias <- provinces$PROV %in% c(35, 38)
peninsulaLines <- provinces[!canarias,]

p +
  layer(sp.polygons(peninsulaLines,  lwd = 0.1)) +
  layer(grid.rect(x=bbIslands[1,1], y=bbIslands[2,1],
                  width=diff(bbIslands[1,]),
                  height=diff(bbIslands[2,]),
                  default.units='native', just=c('left', 'bottom'),
                  gp=gpar(lwd=0.5, fill='transparent')))
dev.off()

##################################################################
## Raster maps
##################################################################

##################################################################
## Diverging palettes
##################################################################

pdf(file="figs/leveplotSISavOrig.pdf")
library(raster)
library(rasterVis)
SISav <- raster('data/SISav')
levelplot(SISav)
dev.off()

meanRad <- cellStats(SISav, 'mean')
SISav <- SISav - meanRad

pdf(file="figs/xyplotSISav.pdf")
xyplot(layer ~ y, data = SISav,
       groups=cut(x, 5),
       par.settings=rasterTheme(symbol=plinrain(n=5, end=200)),
       xlab = 'Latitude', ylab = 'Solar radiation (scaled)',  
       auto.key=list(space='right', title='Longitude', cex.title=1.3))
dev.off()

pdf(file="figs/showDivPal.pdf")
divPal <- brewer.pal(n=9, 'PuOr')
divPal[5] <- "#FFFFFF"

showPal <- function(pal, labs=pal, cex=0.6, ...){
  barplot(rep(1, length(pal)), col=pal,
          names.arg=labs, cex.names=cex,
          axes=FALSE, ...)
}

showPal(divPal)
dev.off()

pdf(file="figs/divPal_SISav_naive.pdf")
divTheme <- rasterTheme(region=divPal)

levelplot(SISav, contour=TRUE, par.settings=divTheme)
dev.off()

rng <- range(SISav[])
## Number of desired intervals
nInt <- 15
## Increment correspondent to the range and nInt
inc0 <- diff(rng)/nInt
## Number of intervals from the negative extreme to zero
n0 <- floor(abs(rng[1])/inc0)
## Update the increment adding 1/2 to position zero in the center of an interval
inc <- abs(rng[1])/(n0 + 1/2)
## Number of intervals from zero to the positive extreme
n1 <- ceiling((rng[2]/inc - 1/2) + 1)
## Collection of breaks
breaks <- seq(rng[1], by=inc, length= n0 + 1 + n1)

## Midpoints computed with the median of each interval
idx <- findInterval(SISav[], breaks, rightmost.closed=TRUE)
mids <- tapply(SISav[], idx, median)
## Maximum of the absolute value both limits
mx <- max(abs(breaks))
mids

pdf(file="figs/showBreak2Pal.pdf")
break2pal <- function(x, mx, pal){
  ## x = mx gives y = 1
  ## x = 0 gives y = 0.5
  y <- 1/2*(x/mx + 1)
  rgb(pal(y), maxColorValue=255)
}

## Interpolating function that maps colors with [0, 1]
## rgb(divRamp(0.5), maxColorValue=255) gives "#FFFFFF" (white)
divRamp <- colorRamp(divPal)
## Diverging palette where white is associated with the interval
## containing the zero
pal <- break2pal(mids, mx, divRamp)
showPal(pal, round(mids, 1))
dev.off()

pdf(file="figs/divPalSISav.pdf")
levelplot(SISav, par.settings=rasterTheme(region=pal),
          at=breaks, contour=TRUE)
dev.off()

pdf(file="figs/divPalSISav_regions.pdf")
divTheme <- rasterTheme()

divTheme$regions$col <- pal
levelplot(SISav, par.settings=divTheme, at=breaks, contour=TRUE)
dev.off()

library(classInt)

cl <- classIntervals(SISav[],
                     ## n=15, style='equal')
                     ## style='hclust')
                     ## style='sd')
                     style='kmeans')
                     ## style='quantile')
cl
breaks <- cl$brks

pdf(file="figs/divPalSISav_classInt.pdf")
idx <- findInterval(SISav[], breaks, rightmost.closed=TRUE)
mids <- tapply(SISav[], idx, median)
mids
mx <- max(abs(breaks))
pal <- break2pal(mids, mx, divRamp)
divTheme$regions$col <- pal
levelplot(SISav, par.settings=divTheme, at=breaks, contour=TRUE)
dev.off()

##################################################################
## Categorical data
##################################################################

library(raster)
## China and India  
ext <- extent(65, 135, 5, 55)

pop <- raster('875430rgb-167772161.0.FLOAT.TIFF')
pop <- crop(pop, ext)
pop[pop==99999] <- NA

landClass <- raster('241243rgb-167772161.0.TIFF')
landClass <- crop(landClass, ext)

library(raster)


ext <- extent(65, 135, 5, 55)

pop <- raster('~/Datos/Nasa/875430rgb-167772161.0.FLOAT.TIFF')
pop <- crop(pop, ext)
pop[pop==99999] <- NA

landClass <- raster('~/Datos/Nasa/241243rgb-167772161.0.TIFF')
landClass <- crop(landClass, ext)

landClass[landClass %in% c(0, 254)] <- NA
## Only four groups are needed:
## Forests: 1:5
## Shublands, etc: 6:11
## Agricultural/Urban: 12:14
## Snow: 15:16
landClass <- cut(landClass, c(0, 5, 11, 14, 16))
## Add a Raster Atribute Table and define the raster as categorical data
landClass <- ratify(landClass)
## Configure the RAT: first create a RAT data.frame using the
## levels method; second, set the values for each class (to be
## used by levelplot); third, assign this RAT to the raster
## using again levels
rat <- levels(landClass)[[1]]
rat$classes <- c('Forest', 'Land', 'Urban', 'Snow')
levels(landClass) <- rat

pdf(file="figs/landClass.pdf")
library(rasterVis)

pal <- c('palegreen4', # Forest
         'lightgoldenrod', # Land
         'indianred4', # Urban
         'snow3')      # Snow

catTheme <- modifyList(rasterTheme(),
                       list(panel.background = list(col='lightskyblue1'),
                            regions = list(col= pal)))

levelplot(landClass, maxpixels=3.5e5, par.settings=catTheme,
          panel=panel.levelplot.raster)
dev.off()

pdf(file="figs/populationNASA.pdf")
pPop <- levelplot(pop, zscaleLog=10, par.settings=BTCTheme,
                  maxpixels=3.5e5, panel=panel.levelplot.raster)
pPop
dev.off()

pdf(file="figs/histogramLandClass.pdf")
s <- stack(pop, landClass)
names(s) <- c('pop', 'landClass')
histogram(~log10(pop)|landClass, data=s,
          scales=list(relation='free'),
          strip=strip.custom(strip.levels=TRUE))
dev.off()

## at for each sub-levelplot is obtained from the global levelplot
at <- pPop$legend$bottom$args$key$at
classes <- rat$classes
nClasses <- length(classes)

pList <- lapply(1:nClasses, function(i){
  landSub <- landClass
  ## Those cells from a different land class are set to NA...
  landSub[!(landClass==i)] <- NA
  ## ... and the resulting raster mask the population raster
  popSub <- mask(pop, landSub)
  ## The HCL color wheel is divided in nClasses
  step <- 360/nClasses
  ## and a sequential palette is constructed with a hue from one
  ## the color wheel parts
  cols <- rev(sequential_hcl(16, h = (30 + step*(i-1))%%360))

  pClass <- levelplot(popSub, zscaleLog=10, at=at, maxpixels=3.5e5,
                      col.regions=cols, margin=FALSE)
})

png(filename="figs/popLandClass.png",res=300,height=2000,width=2000)
p <- Reduce('+', pList)
## Function to add a title to a legend
addTitle <- function(legend, title){
  titleGrob <- textGrob(title, gp=gpar(fontsize=8), hjust=1, vjust=1)
  ## retrieve the legend from the trellis object
  legendGrob <- eval(as.call(c(as.symbol(legend$fun), legend$args)))
  ## Layout of the legend WITH the title
  ly <- grid.layout(ncol=1, nrow=2,
                    widths=unit(0.9, 'grobwidth', data=legendGrob))
  ## Create a frame to host the original legend and the title
  fg <- frameGrob(ly, name=paste('legendTitle', title, sep='_'))
  ## Add the grobs to the frame
  pg <- packGrob(fg, titleGrob, row=2)
  pg <- packGrob(pg, legendGrob, row=1)
  }

## Access each trellis object from pList...
for (i in seq_len(nClasses)){
  ## extract the legend (automatically created by spplot)...
  lg <- pList[[i]]$legend$right
  ## ...supress labels except from the last legend...
  lg$args$key$labels$cex=ifelse(i==nClasses, 0.8, 0) 
  ## ... and add the addTitle function to the legend component of each trellis object
  pList[[i]]$legend$right <- list(fun='addTitle',
                                  args=list(legend=lg, title=classes[i]))
}

## List of legends
legendList <- lapply(pList, function(x){
  lg <- x$legend$right
  clKey <- eval(as.call(c(as.symbol(lg$fun), lg$args)))
  clKey
})

## Function to pack the list of legends in a unique legend
## Adapted from latticeExtra::: mergedTrellisLegendGrob
packLegend <- function(legendList){
  N <- length(legendList)
  ly <- grid.layout(nrow = 1,  ncol = N)
  g <- frameGrob(layout = ly, name = "mergedLegend")
  for (i in 1:N) g <- packGrob(g, legendList[[i]], col = i)
  g
}

## The legend of p will include all the legends
p$legend$right <- list(fun = 'packLegend',  args = list(legendList = legendList))

p
dev.off()
