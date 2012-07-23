
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

pdf(file="figs/aranjuezSplom.pdf")
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
           layout=c(1, 3), colramp=BTC, 
           panel=function(x, y, xlim,...){
             panel.hexbinplot(x, y, ...)
             panel.loess(x, y, ..., col = 'red')
           }
           )
dev.off()

pdf(file="figs/aranjuezOuterStrips.pdf")
library(latticeExtra)

useOuterStrips(xyplot(Temperature~Radiation|month*Statistic,
                      data=aranjuezRshp,
                      between=list(x=0), 
                      panel=function(...){
                        panel.xyplot(...,
                                     col='skyblue4', pch=19,
                                     cex=0.5, alpha=0.3)
                        panel.rug(..., col.line='indianred1',
                                  end=0.05, alpha=0.6)
                        panel.loess(..., col='indianred1', lwd=1.5)
                        }))
  
dev.off()
