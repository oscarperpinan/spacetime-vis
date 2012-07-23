
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
load('data/CO2.RData')

library(googleVis)
pgvis <- gvisMotionChart(CO2data, idvar='Country.Name', timevar='Year')

print(pgvis, 'html', file='figs/googleVis.html')

##################################################################
## Choosing colors
##################################################################

library(RColorBrewer)

nCountries <- nlevels(CO2data$Country.Name)
pal <- brewer.pal(n=5, 'Set1')
pal <- rep(pal, length = nCountries)

## Rank of average values of CO2 per capita
CO2mean <- aggregate(CO2.capita ~ Country.Name, data=CO2data, FUN=mean)
palOrdered <- pal[rank(CO2mean$CO2.capita)]

pdf(file="figs/hclust.pdf")
CO2capita <- subset(CO2, Indicator.Code=='EN.ATM.CO2E.PC')
hCO2 <- hclust(dist(CO2capita[, -c(1:4)]))

oldpar <- par(mar=c(0, 2, 0, 0) + .1)
plot(hCO2, labels=CO2capita$Country.Name,
     xlab='', ylab='', sub='', main='')
par(oldpar)
dev.off()

idx <- match(levels(CO2data$Country.Name), 
             CO2capita$Country.Name[hCO2$order])
palOrdered <- pal[idx]

myTheme <- custom.theme(pch=19, cex=0.6, symbol=palOrdered)

pCO2.capita <- xyplot(GNI.capita  ~ CO2.capita,
                      xlab="CO2 emissions (metric tons per capita)",
                      ylab="GNI per capita, PPP (current international $)",
                      groups=Country.Name, data=CO2data,
                      par.settings=myTheme,
                      type='b')

xyplot(GNI.capita  ~ CO2.capita,
       xlab="CO2 emissions (metric tons per capita)",
       ylab="GNI per capita, PPP (current international $)",
       groups=Country.Name, data=CO2data,
       par.settings=myTheme,
       type='b', 
       panel=function(x, y, ..., subscripts, groups){
         panel.text(x, y, ...,
                    labels=CO2data$Year[subscripts],
                    pos=2, cex=0.5, col='gray')
         panel.superpose(x, y, subscripts, groups,...)
       }
       )

library(latticeExtra)

pCO2.capita <- pCO2.capita +
    glayer_(panel.text(..., labels=CO2data$Year[subscripts],
                       pos=2, cex=0.5, col='gray'))

##################################################################
## Positioning labels
##################################################################

pdf(file="figs/CO2_capita.pdf")
pCO2.capita +
  glayer(panel.text(x[9], y[9],
                    labels= group.value,
                    col=palOrdered[group.number],
                    pos=4, offset=0.7, cex=0.7))
dev.off()

pdf(file="figs/CO2_capitaDL.pdf")
library(directlabels)
direct.label(pCO2.capita, method='extreme.grid')
dev.off()

##################################################################
## A panel for each year and bubbles with variable radius
##################################################################

library(classInt)
z <- CO2data$CO2.PPP
intervals <- classIntervals(z, n=4, style='fisher')

nInt <- length(intervals$brks) - 1
cex.key <- seq(0.5, 1.8, length=nInt)

idx <- findCols(intervals)
CO2data$cexPoints <- cex.key[idx]

op <- options(digits=2)
## Use whatever pal you wish. It will not be used in the following code
intervalTab <- findColours(intervals, pal=pal)
## The names attribute stores the intervals strings
tab <- names(attr(intervalTab, 'table'))
options(op)

key <- list(space='right',
            title='CO2.PPP', cex.title=1,
            ## Labels of the key are the intervals strings
            text=list(labels=tab, cex=0.85),
            ## Points sizes are defined with cex.key
            points=list(col='black', pch=19,
              cex=cex.key, alpha=0.7))

pdf(file="figs/CO2points.pdf")
xyplot(GNI.capita~CO2.capita|Year, data=CO2data,
       groups=Country.Name, key=key, alpha=0.7,
       strip=strip.custom(strip.levels=c(TRUE, TRUE)),
       panel=function(x, y, cex.values,..., subscripts, groups){
         panel.text(x, y, ...,
                    labels=groups[subscripts],
                    col=palOrdered[groups[subscripts]],
                    pos=3, cex=0.8*sqrt(CO2data$cexPoints[subscripts]))
         panel.points(x, y,
                      col=palOrdered[groups[subscripts]],
                      cex=CO2data$cexPoints[subscripts])
       })  
dev.off()

##################################################################
## Travelling bubbles
##################################################################

library(gridSVG)

xyplot(GNI.capita ~ CO2.capita, data=CO2data,
       subset=Year==2000, groups=Country.Name,
       ## The limits of the graphic are defined
       ## with the entire dataset
       xlim=extendrange(CO2data$CO2.capita),
       ylim=extendrange(CO2data$GNI.capita),
       panel=function(x, y, ..., subscripts, groups) {
         color <- palOrdered[groups[subscripts]]
         radius <- CO2data$CO2.PPP[subscripts]
         ## Size of labels
         cex <- 1.1*sqrt(radius)
         ## Bubbles
         grid.circle(x, y, default.units="native",
                     r=radius*unit(.25, "inch"),
                     name=trellis.grobname("points", type="panel"),
                     gp=gpar(col=color,
                       ## Fill color ligther than border
                       fill=adjustcolor(color, alpha=.5),
                       lwd=2))
         ## Country labels
         grid.text(label=groups[subscripts],
                   x=unit(x, 'native'),
                   ## Labels above each bubble
                   y=unit(y, 'native') + 1.5 * radius *unit(.25, 'inch'),
                   name=trellis.grobname('labels', type='panel'),
                   gp=gpar(col=color, cex=cex))
       })

## Duration in seconds of the animation
duration <- 20

nCountries <- nlevels(CO2data$Country.Name)
years <- unique(CO2data$Year)
nYears <- length(years)

## Intermediate positions of the bubbles
x_points <- animUnit(unit(CO2data$CO2.capita, 'native'),
                     id=rep(seq_len(nCountries), nYears))
y_points <- animUnit(unit(CO2data$GNI.capita, 'native'),
                     id=rep(seq_len(nCountries), nYears))
## Intermediate positions of the labels
y_labels <- animUnit(unit(CO2data$GNI.capita, 'native') +
                     1.5 * CO2data$CO2.PPP * unit(.25, 'inch'),
                     id=rep(seq_len(nCountries), nYears))
## Intermediate sizes of the bubbles
size <- animUnit(CO2data$CO2.PPP * unit(.25, 'inch'),
                     id=rep(seq_len(nCountries), nYears))

grid.animate(trellis.grobname("points", type="panel", row=1, col=1),
             duration=duration,
             x=x_points,
             y=y_points,
             r=size,
             rep=TRUE)

grid.animate(trellis.grobname("labels", type="panel", row=1, col=1),
             duration=duration,
             x=x_points,
             y=y_labels,
             rep=TRUE)

countries <- unique(CO2data$Country.Name)
URL <- paste('http://en.wikipedia.org/wiki/', countries, sep='')
grid.hyperlink(trellis.grobname('points', type='panel', row=1, col=1),
               URL, group=FALSE)

visibility <- matrix("hidden", nrow=nYears, ncol=nYears)
diag(visibility) <- "visible"
yearText <- animateGrob(garnishGrob(textGrob(years, .9, .15,
                                             name="year",
                                             gp=gpar(cex=2, col="grey")),
                                    visibility="hidden"),
                        duration=20,
                        visibility=visibility,
                        rep=TRUE)
grid.draw(yearText)

gridToSVG("figs/bubbles.svg")
