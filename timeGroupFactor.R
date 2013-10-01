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
library(zoo)

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
## Scatterplot matrix: time as a grouping variable 
##################################################################

png(filename="figs/aranjuezSplom.png",res=600,height=4000,width=4000)
load('data/aranjuez.RData')

## Red-Blue palette with black added (12 colors)
colors <- c(brewer.pal(n=11, 'RdBu'), '#000000')
## Rearrange according to months (darkest for summer)
colors <- colors[c(6:1, 12:7)]

splom(~as.data.frame(aranjuez),
        groups=format(index(aranjuez), '%m'),
      auto.key=list(space='right', 
          title='Month', cex.title=1),
      pscale=0, varname.cex=0.7, xlab='',
        par.settings=custom.theme(symbol=colors,
            pch=19), cex=0.3, alpha=0.1)
dev.off()

trellis.focus('panel', 1, 1)
idx <- panel.link.splom(pch=13, cex=0.6, col='green')
aranjuez[idx,]

##################################################################
## Hexagonal binning
##################################################################

pdf(file="figs/aranjuezSplomHexbin.pdf")
library(hexbin)

splom(~as.data.frame(aranjuez),
           panel=panel.hexbinplot, xlab='',
           colramp=BTC,
           diag.panel = function(x, ...){
             yrng <- current.panel.limits()$ylim
             d <- density(x, na.rm=TRUE)
             d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y))
             panel.lines(d)
             diag.panel.splom(x, ...)
           },
           lower.panel = function(x, y, ...){
             panel.hexbinplot(x, y, ...)
             panel.loess(x, y, ..., col = 'red')
           },
           pscale=0, varname.cex=0.7
           )

dev.off()

aranjuezDF <- data.frame(aranjuez,
                         month=format(index(aranjuez), '%m'))
aranjuezRshp <- reshape(aranjuezDF, direction='long',
                        varying=list(names(aranjuez)[1:3]),
                        v.names='Temperature',
                        times=names(aranjuez)[1:3],
                        timevar='Statistic')

head(aranjuezRshp)

pdf(file="figs/aranjuezHexbinplot.pdf")
hexbinplot(Radiation~Temperature|Statistic, data=aranjuezRshp,
           layout=c(1, 3), colramp=BTC) +
    layer(panel.loess(..., col = 'red'))
dev.off()

pdf(file="figs/aranjuezGGhexbin.pdf")
ggplot(data=aranjuezRshp, aes(Temperature, Radiation)) +
    stat_binhex(ncol=1) + 
    stat_smooth(se=FALSE, method='loess', col='red') +
    facet_wrap(~Statistic, ncol=1) +
    theme_bw()
dev.off()

##################################################################
## Scatterplot with time as a conditioning variable
##################################################################

pdf(file="figs/aranjuezFacetGrid.pdf")
ggplot(data=aranjuezRshp, aes(Radiation, Temperature)) +
    facet_grid(Statistic ~ month) +
    geom_point(col='skyblue4', pch=19, cex=0.5, alpha=0.3) +
    geom_rug() +
    stat_smooth(se=FALSE, method='loess', col='indianred1', lwd=1.2) +
    theme_bw()
dev.off()

pdf(file="figs/aranjuezOuterStrips.pdf")
useOuterStrips(xyplot(Temperature ~ Radiation | month * Statistic,
                      data=aranjuezRshp,
                      between=list(x=0),
                      col='skyblue4', pch=19,
                      cex=0.5, alpha=0.3)) +
    layer({
        panel.rug(..., col.line='indianred1', end=0.05, alpha=0.6)
        panel.loess(..., col='indianred1', lwd=1.5, alpha=1)
    })
dev.off()
