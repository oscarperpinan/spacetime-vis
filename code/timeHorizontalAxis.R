
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
load('data/aranjuez.RData')
load('data/navarra.RData')
load('data/unemployUSA.RData')

pdf(file="figs/aranjuez.pdf")
library(lattice)
xyplot(aranjuez, layout=c(1, ncol(aranjuez)))

dev.off()

pdf(file="figs/aranjuezXblocks.pdf")
Year <- function(x)format(x, "%Y")

xyplot(aranjuez, layout=c(1, ncol(aranjuez)), strip=FALSE,
       scales=list(y=list(cex=0.6, rot=0)),
       panel=function(x, y, ...){
         panel.xblocks(x, Year,
                       col = c("lightgray", "white"),
                       border = "darkgray")
         panel.xblocks(x, y<mean(y, na.rm=TRUE),
                       col = "indianred1",
                       height=unit(0.1, 'npc'))
         panel.lines(x, y, col='royalblue4', lwd=0.5, ...)
         panel.text(x[1], min(y, na.rm=TRUE),
                    names(aranjuez)[panel.number()],
                    cex=0.6, adj=c(0, 0), srt=90, ...)
         idxMax <- which.max(y)
         panel.points(x[idxMax], y[idxMax],
                      col='black', fill='lightblue', pch=24)
         idxMin <- which.min(y)
         panel.points(x[idxMin], y[idxMin],
                      col='black', fill='lightblue', pch=25)
       })

dev.off()

pdf(file="figs/navarra.pdf")
avRad <- zoo(rowMeans(navarra, na.rm=1), index(navarra))
pNavarra <- xyplot(navarra - avRad,
                   superpose=TRUE, auto.key=FALSE,
                   lwd=0.5, alpha=0.3, col='midnightblue') 
pNavarra
dev.off()

pdf(file="figs/navarraBanking.pdf")
xyplot(navarra - avRad,
       aspect='xy', cut=list(n=3, overlap=0.1),
       strip=FALSE,
       superpose=TRUE, auto.key=FALSE,
       lwd=0.5, alpha=0.3, col='midnightblue')
dev.off()

pdf(file="figs/navarraHorizonplot.pdf")
library(latticeExtra)

horizonplot(navarra-avRad,
            layout=c(1, ncol(navarra)),
            origin=0, colorkey=TRUE)
dev.off()

library(gridSVG)
## grobs in the graphical output
pNavarra
grobs <- grid.ls(print=FALSE)
## only interested in some of them
nms <- grobs$name[grobs$type == "grobListing"]
idxNames <- grep('lines', nms)
IDs <- nms[idxNames]

for (id in unique(IDs)){
  ## extract information from the data
  ## according to the ID value
  i <- strsplit(id, '\\.')
  i <- sapply(i, function(x)as.numeric(x[5]))
  ## Information to be attached to each line: annual mean of daily
  ## radiation and abbreviated name of the station
  dat <- round(mean(navarra[,i], na.rm=TRUE), 2)
  info <- paste(names(navarra)[i], paste(dat, collapse=','),
                sep=': ')
  ## attach SVG attributes
  grid.garnish(id,
               onmouseover="highlight(evt)",
               onmouseout="hide(evt)",
               title=info)
}

grid.script(filename="highlight.js")

gridToSVG('figs/navarraRadiation.svg')

pdf(file="~/Dropbox/chapman/book/figs/unemployUSAxyplot.pdf")
xyplot(unemployUSA, superpose=TRUE, par.settings=custom.theme,
       auto.key=list(space='right'))

dev.off()

panel.flow <- function(x, y, groups, origin, ...){
  dat <- data.frame(x=x, y=y, groups=groups)
  nVars <- nlevels(groups)
  groupLevels <- levels(groups)

  ## From long to wide
  yWide <- unstack(dat, y~groups)
  ## Where are the maxima of each variable located? We will use
  ## them to position labels.
  idxMaxes <- apply(yWide, 2, which.max)

  ##Origin calculated following Havre.Hetzler.ea2002
  if (origin=='themeRiver') origin= -1/2*rowSums(yWide)
  else origin=0 
  yWide <- cbind(origin=origin, yWide)
  ## Cumulative sums to define the polygon
  yCumSum <- t(apply(yWide, 1, cumsum))
  Y <- as.data.frame(sapply(seq_len(nVars),
                            function(iCol)c(yCumSum[,iCol+1],
                                            rev(yCumSum[,iCol]))))
  names(Y) <- levels(groups)
  ## Back to long format, since xyplot works that way
  y <- stack(Y)$values

  ## Similar but easier for x
  xWide <- unstack(dat, x~groups)
  x <- rep(c(xWide[,1], rev(xWide[,1])), nVars)
  ## Groups repeated twice (upper and lower limits of the polygon)
  groups <- rep(groups, each=2)
  
  ## Graphical parameters
  superpose.polygon <- trellis.par.get("superpose.polygon")
  col = superpose.polygon$col
  border = superpose.polygon$border 
  lwd = superpose.polygon$lwd 

  ## Draw polygons
  for (i in seq_len(nVars)){
    xi <- x[groups==groupLevels[i]]
    yi <- y[groups==groupLevels[i]]
    panel.polygon(xi, yi, border=border,
                  lwd=lwd, col=col[i])
  }

  ## Print labels
  for (i in seq_len(nVars)){
    xi <- x[groups==groupLevels[i]]
    yi <- y[groups==groupLevels[i]]
    N <- length(xi)/2
    ## Height available for the label
    h <- unit(yi[idxMaxes[i]], 'native') -
      unit(yi[idxMaxes[i] + 2*(N-idxMaxes[i]) +1], 'native')
    ##...converted to "char" units
    hChar <- convertHeight(h, 'char', TRUE)
    ## If there is enough space and we are not at the first or
    ## last variable, then the label is printed inside the polygon.
    if((hChar >= 1) && !(i %in% c(1, nVars))){
      grid.text(groupLevels[i],
                xi[idxMaxes[i]],
                (yi[idxMaxes[i]] +
                 yi[idxMaxes[i] + 2*(N-idxMaxes[i]) +1])/2,
                gp = gpar(col='white', alpha=0.7, cex=0.7),
                default.units='native')
    } else {
      ## Elsewhere, the label is printed outside

      grid.text(groupLevels[i],
                xi[N],
                (yi[N] + yi[N+1])/2,
                gp=gpar(col=col[i], cex=0.7),
                just='left', default.units='native')
    }          
  }
}

library(colorspace)

nCols <- ncol(unemployUSA)
pal <- rainbow_hcl(nCols, c=70, l=75, start=30, end=300)
myTheme <- custom.theme(fill=pal, lwd=0.2)

pdf(file="figs/ThemeRiverError.pdf")
xyplot(unemployUSA, superpose=TRUE, auto.key=FALSE,
       panel=panel.flow, origin='themeRiver',
       par.settings=myTheme, cex=0.4, offset=0,
       scales=list(y=list(draw=FALSE)))
dev.off()

prepanel.flow <- function(x, y, groups, origin,...){
  dat <- data.frame(x=x, y=y, groups=groups)
  nVars <- nlevels(groups)
  groupLevels <- levels(groups)
  yWide <- unstack(dat, y~groups)
  if (origin=='themeRiver') origin= -1/2*rowSums(yWide)
  else origin=0
  yWide <- cbind(origin=origin, yWide)
  yCumSum <- t(apply(yWide, 1, cumsum))

  list(xlim=range(x),
       ylim=c(min(yCumSum[,1]), max(yCumSum[,nVars+1])),
       dx=diff(x),
       dy=diff(c(yCumSum[,-1])))
}

pdf(file="figs/unemployUSAThemeRiver.pdf")
sep2008 <- as.numeric(as.yearmon('2008-09'))

xyplot(unemployUSA, superpose=TRUE, auto.key=FALSE,
       panel=panel.flow, prepanel=prepanel.flow,
       origin='themeRiver', scales=list(y=list(draw=FALSE)),
       par.settings=myTheme) +
  layer(panel.abline(v=sep2008, col='gray', lwd=0.7))
dev.off()
