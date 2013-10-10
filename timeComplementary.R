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

##################################################################
## Polylines
##################################################################

load('data/CO2.RData')

library(googleVis)
pgvis <- gvisMotionChart(CO2data, idvar='Country.Name', timevar='Year')

print(pgvis, 'html', file='figs/googleVis.html')

pdf(file="figs/CO2_GNI.pdf")
## lattice version
xyplot(GNI.capita  ~ CO2.capita, data=CO2data,
       xlab="CO2 emissions (metric tons per capita)",
       ylab="GNI per capita, PPP (current international $)",
       groups=Country.Name, type='b')
dev.off()

## ggplot2 version
ggplot(data=CO2data, aes(x=CO2.capita, y=GNI.capita,
           color=Country.Name)) +
    xlab("CO2 emissions (metric tons per capita)") +
    ylab("GNI per capita, PPP (current international $)") +
    geom_point() + geom_path() + theme_bw()

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
CO2capita <- CO2data[, c('Country.Name', 'Year', 'CO2.capita')]
CO2capita <- reshape(CO2capita, idvar='Country.Name', timevar='Year', direction='wide')
hCO2 <- hclust(dist(CO2capita[, -1]))

oldpar <- par(mar=c(0, 2, 0, 0) + .1)
plot(hCO2, labels=CO2capita$Country.Name,
     xlab='', ylab='', sub='', main='')
par(oldpar)
dev.off()

idx <- match(levels(CO2data$Country.Name), 
             CO2capita$Country.Name[hCO2$order])
palOrdered <- pal[idx]

## simpleTheme encapsulates the palette in a new theme for xyplot
myTheme <- simpleTheme(pch=19, cex=0.6, col=palOrdered)

pCO2.capita <- xyplot(GNI.capita  ~ CO2.capita,
                      xlab="CO2 emissions (metric tons per capita)",
                      ylab="GNI per capita, PPP (current international $)",
                      groups=Country.Name, data=CO2data,
                      par.settings=myTheme,
                      type='b')

gCO2.capita <- ggplot(data=CO2data, aes(x=CO2.capita, y=GNI.capita,
                      color=Country.Name)) +
    geom_point() + geom_path() +
    scale_color_manual(values=palOrdered, guide=FALSE) +
    xlab('CO2 emissions (metric tons per capita)') +
    ylab('GNI per capita, PPP (current international $)') +
    theme_bw()

##################################################################
## Labels to show time information
##################################################################

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

pCO2.capita <- pCO2.capita +
    glayer_(panel.text(..., labels=CO2data$Year[subscripts],
                       pos=2, cex=0.5, col='gray'))

gCO2.capita <- gCO2.capita + geom_text(aes(label=Year), colour='gray',
                                       size=2.5, hjust=0, vjust=0)

##################################################################
## Country names: positioning labels
##################################################################

pdf(file="figs/CO2_capita.pdf")
library(maptools)  
## group.value provides the country name; group.number is the
## index of each country to choose the color from the palette.
pCO2.capita +
    glayer(panel.pointLabel(mean(x), mean(y),
                            labels= group.value,
                            col=palOrdered[group.number],
                            cex=.8,
                            fontface=2, fontfamily='Palatino'))
dev.off()

pdf(file="figs/CO2_capitaDL.pdf")
library(directlabels)
direct.label(pCO2.capita, method='extreme.grid')
dev.off()

direct.label(gCO2.capita, method='extreme.grid')

##################################################################
## A panel for each year
##################################################################

pdf(file="figs/CO2_capita_panel.pdf")
xyplot(GNI.capita  ~ CO2.capita | factor(Year), data=CO2data,
       xlab="CO2 emissions (metric tons per capita)",
       ylab="GNI per capita, PPP (current international $)",
       groups=Country.Name, type='b',
       auto.key=list(space='right'))
dev.off()

ggplot(data=CO2data, aes(x=CO2.capita, y=GNI.capita, colour=Country.Name)) +
    facet_wrap(~ Year) + geom_point(pch=19) + 
    xlab('CO2 emissions (metric tons per capita)') +
    ylab('GNI per capita, PPP (current international $)') +
    theme_bw()

pdf(file="figs/CO2_capita_panel_labels.pdf")
xyplot(GNI.capita  ~ CO2.capita | factor(Year), data=CO2data,
       xlab="CO2 emissions (metric tons per capita)",
       ylab="GNI per capita, PPP (current international $)",
       groups=Country.Name, type='b',
       par.settings=myTheme) + 
    glayer(panel.pointLabel(x, y, labels=group.value,
                            col=palOrdered[group.number], cex=0.7))
dev.off()

##################################################################
## Using variable size to encode an additional variable
##################################################################

library(classInt)
z <- CO2data$CO2.PPP
intervals <- classIntervals(z, n=4, style='fisher')

nInt <- length(intervals$brks) - 1
cex.key <- seq(0.5, 1.8, length=nInt)

idx <- findCols(intervals)
CO2data$cexPoints <- cex.key[idx]

pdf(file="figs/CO2pointsGG.pdf")
ggplot(data=CO2data, aes(x=CO2.capita, y=GNI.capita, colour=Country.Name)) +
    facet_wrap(~ Year) + geom_point(aes(size=cexPoints), pch=19) +
    xlab('CO2 emissions (metric tons per capita)') +
    ylab('GNI per capita, PPP (current international $)') +
    theme_bw()
dev.off()

pdf(file="figs/CO2points.pdf")
op <- options(digits=2)
tab <- print(intervals)
options(op)

key <- list(space='right',
            title='CO2.PPP', cex.title=1,
            ## Labels of the key are the intervals strings
            text=list(labels=names(tab), cex=0.85),
            ## Points sizes are defined with cex.key
            points=list(col='black', pch=19,
              cex=cex.key, alpha=0.7))


xyplot(GNI.capita~CO2.capita|factor(Year), data=CO2data,
       groups=Country.Name, key=key, alpha=0.7,
       col=palOrdered, cex=CO2data$cexPoints) +
    glayer(panel.pointLabel(x, y, labels=group.value,
                            col=palOrdered[group.number], cex=0.7))
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
                     id=rep(seq_len(nCountries), each=nYears))
y_points <- animUnit(unit(CO2data$GNI.capita, 'native'),
                     id=rep(seq_len(nCountries), each=nYears))
## Intermediate positions of the labels
y_labels <- animUnit(unit(CO2data$GNI.capita, 'native') +
                     1.5 * CO2data$CO2.PPP * unit(.25, 'inch'),
                     id=rep(seq_len(nCountries), each=nYears))
## Intermediate sizes of the bubbles
size <- animUnit(CO2data$CO2.PPP * unit(.25, 'inch'),
                     id=rep(seq_len(nCountries), each=nYears))

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

grid.export("figs/bubbles.svg")
