
## #+PROPERTY:  session *R*
## #+PROPERTY:  tangle yes
## #+PROPERTY:  comments org

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

## Time graph of different meteorological variables
##    :PROPERTIES:
##    :ID:       9845d111-ce84-4d95-bb2f-59e9f5ff635c
##    :END:

## There is a variety of scientific researches interested in the
## relationship between several meteorological variables. A suitable
## approach is to display the time evolution of all of them either using
## a panel for each of the variables. The superposition of variables with
## different characteristics is not very useful (unless their values were
## previously rescaled), so this option is postponed for the next
## section.

## For this example we will use eight years of daily data from the SIAR
## meteorological station located at Aranjuez (Madrid) as described in
## the section [[id:e534aa8b-3cec-412f-9fea-70c5d41a39d2][Multiple time series with different scales]].  This
## multivariate time series can be displayed with the =xyplot= method of
## =lattice= with a panel for each variable. The =layout= argument is
## used to display the variables with parallel panels arranged in rows.

## #+CAPTION: Time plot of the collection of meteorological time series of the Aranjuez station.
## #+LABEL: fig:aranjuezNaive

pdf(file="figs/aranjuez.pdf")
xyplot(aranjuez, layout=c(1, ncol(aranjuez)))
dev.off()

## This first attempt can be improved with a custom panel
## function. This function will generate the content of each panel
## using the information processed by =xyplot=. Since these functions
## are executed consecutively, the order of the functions determines
## the superposition of graphical layers:
## - The label of each time series is displayed with text inside each
##   panel instead of using the strips mechanism. The =panel.text=
##   prints the name of each variable with the aid of =panel.number=.
## - The alternation of years is displayed with blocks of gray and
##   white color using the =panel.xblocks= function from
##   =latticeExtra=. The year is extracted (as character) from the
##   time index of the =zoo= object with =format.POSIXlt=.
## - Those values below the mean of each variable are highlighted
##   with short red color blocks at the bottom of each panel, again
##   with the =panel.xblocks= function.
## - The maxima and minima are hihglighted with blue small triangles.

## \index{Panel function}
## \index{panel.xblocks@\texttt{panel.xblocks}}
## \index{panel.text@\texttt{panel.text}}
## \index{panel.number\texttt{panel.number}}
## \index{panel.points\texttt{panel.points}}
## #+CAPTION: Enhanced time plot of the collection of meteorological time series of the Aranjuez station.
## #+LABEL: fig:aranjuezEnhanced

pdf(file="figs/aranjuezXblocks.pdf")
Year <- function(x)format(x, "%Y")

xyplot(aranjuez, layout=c(1, ncol(aranjuez)), strip=FALSE,
       scales=list(y=list(cex=0.6, rot=0)),
       panel=function(x, y, ...){
         panel.xblocks(x, Year, col = c("lightgray", "white"),
                       border = "darkgray")
         panel.xblocks(x, y<mean(y, na.rm=TRUE), col = "indianred1",
                       height=unit(0.1, 'npc'))
         panel.xyplot(x, y, ...)
         panel.text(x[1], min(y, na.rm=TRUE),
                    names(aranjuez)[panel.number()],
                    cex=0.6, adj=c(0, 0), srt=90, ...)
         idxMax <- which.max(y)
         panel.points(x[idxMax], y[idxMax], col='black', fill='lightblue', pch=24)
         idxMin <- which.min(y)
         panel.points(x[idxMin], y[idxMin], col='black', fill='lightblue', pch=25)
       })

dev.off()

## Time series of variables with the same scale

## As an example of time series of variables with the same scale we will
## use measurements of solar radiation from different meteorological
## stations, as described in the section [[id:d820caf0-b02d-4906-a9a6-ca14a80f776b][Time series of variables with
## the same scale]].

## The first attempt to display this multivariate time series makes
## use of the =xyplot.zoo= method. The objective of this graphic is
## to display the behaviour of the collection as a whole: the series
## are superposed in the same panel (=superpose=TRUE=) without legend
## (=auto.key=TRUE=), using thin lines (=lwd=0.3=) and partial
## transparency (=alpha=0.3=). Transparency softens overplotting
## problems and reveals density clusters since regions with more
## overlapping lines are darker. The figure \ref{fig:navarraNaive}
## displays the variations around the time average (=avRad=).

## \index{zoo@\texttt{zoo}} 
## \index{xyplot.zoo@\texttt{xyplot.zoo}}
## #+CAPTION: Time plot of the variations around time average of solar radiation measurements from the meteorological stations of Navarra.
## #+LABEL: fig:navarraNaive

pdf(file="figs/navarra.pdf")
avRad <- zoo(rowMeans(navarra, na.rm=1), index(navarra))
pNavarra <- xyplot(navarra - avRad, superpose=TRUE, auto.key=FALSE,
                   lwd=0.3, alpha=0.2, col='black') 
pNavarra
dev.off()

## The horizon graph

## The horizon graph\index{Horizon graph} is useful to examine how a
## large number of series changes through time, and to do so in a way
## that allows both comparisons between the individual time series and
## and independent analysis of each series. Moreover,
## extraordinary behaviours and predominant patterns are easily
## distinguished.

## This graph displays several stacked series collapsing the y-axis
## to free vertical space:
## - Positive and negative values share the same vertical
##   space. Negative values are inverted and placed above the
##   reference line. Sign is encoded using different hues (positive
##   values in blue and negative values in red).
## - Differences in magnitude are displayed as differences in color
##   intensity (darker colors for greater differences).
## - The color bands share the same baseline and are superposed with
##   darker bands in front of the ligther ones.

## Since the panels share the same design structure, once this
## technique is understood, it is easy to establish comparisons or
## spot extraordinary events.  This method is what Tufte described as
## small multiples\index{Small multiples}.

## Figure \ref{fig:navarraHorizonplot} displays the variations of
## solar radiation around the time average with an horizon graph
## using a row for each time series.
## \index{Packages!latticeExtra@\texttt{latticeExtra}}
## \index{horizonplot@\texttt{horizonplot}}
## #+CAPTION: Horizonplot of variations around time average of solar radiation measurements from the meteorological stations of Navarra.
## #+LABEL: fig:navarraHorizonplot

pdf(file="figs/navarraHorizonplot.pdf")
library(latticeExtra)

horizonplot(navarra-avRad, layout=c(1, ncol(navarra)),
            origin=0, colorkey=TRUE)
dev.off()

## Interaction with =gridSVG=

## The =gridSVG= package provides functions to convert =grid=-
## based =R= graphics to an SVG format. It provides several functions
## to add dynamic and interactive capabilities to =R= graphics. In
## this section we will use =grid.script=, a function to add
## JavaScript code to a plot.

## The first step is to specify which component of the scene
## will run the JavaScript code. The =grid.ls= function  returns a
## listing of the names of grobs or viewports included in the graphic
## output: only the lines will be connected with the JavaScript
## code. 

## \index{Packages!gridSVG@\texttt{gridSVG}}
## \index{grid.ls@\texttt{grid.ls}}

library(gridSVG)
## grobs in the graphical output
pNavarra
grobs <- grid.ls(print=FALSE)
## only interested in some of them
nms <- grobs$name[grobs$type == "grobListing"]
idxNames <- grep('lines', nms)
IDs <- nms[idxNames]

## The second step is to modify each =grob= (graphical object) to add
## attributes that specify when it will call JavaScript code. For
## each line identified with the elements of the =IDs= vector and
## associated to a meteorological station, the =navarra= object is
## accessed to extract the annual mean value of the daily radiation
## and the abbreviated name of the corresponding station.  The
## =grid.garnish= function adds attributes to the =grob= of each line
## so that when the mouse moves over a grob a tooltip is displayed to
## show the information defined by =info= and the line is highlighted
## and colored in red, and to hide the tooltip and to set the default
## parameters of the line when the mouse hovers out of the =grob=.

## \index{grid.garnish@\texttt{grid.garnish}}

for (id in unique(IDs)){
  ## extract information from the data
  ## according to the ID value
  i <- strsplit(id, '\\.')
  i <- sapply(i, function(x)as.numeric(x[5]))

  ## Information to be attached to each line: annual mean of daily
  ## radiation and abbreviated name of the station
  dat <- round(mean(navarra[,i], na.rm=TRUE), 2)
  info <- paste(names(navarra)[i], paste(dat, collapse=','),
                sep=':')
  ## attach SVG attributes
  grid.garnish(id,
               onmouseover=paste("showTooltip(evt,'", info, "')"),
               onmouseout="hideTooltip(evt)")
}

## Finally, =grid.script= adds a script file that contains the
## JavaScript code to draw the tooltips and =gridToSVG= exports
## the whole scene to SVG. 

## \index{grid.script@\texttt{grid.script}}
## \index{gridToSVG@\texttt{gridToSVG}}
## \index{Javascript}

grid.script(filename="tooltip.js")
gridToSVG('figs/navarraRadiation.svg')

## Stacked graphs

## If the variables of a multivariate time series can be summed to
## produce a meaningful global variable, they may be better displayed
## with stacked graphs. For example, the information of unemployment
## rates in the USA ([[id:fed0f8b8-5ab0-4e25-a04d-23289d0acf77][Unemployment rates in the United States]]) provides
## data of unemployed persons by industry and class of workers, and can
## be summed to give a total unemployment rate.

## The time series of unemployment rates can be directly displayed with the =xyplot.zoo= method.

pdf(file="~/Dropbox/chapman/book/figs/unemployUSAxyplot.pdf")
xyplot(unemployUSA, superpose=TRUE,
       par.settings=custom.theme.2,
       auto.key=list(space='right'))
dev.off()

## This graphical output is not very useful: the legend is confusing with
## too many items; the vertical scale is dominated by two series with the
## bulk of the series buried in the lower part of the scale; the trend,
## variations and structure of the total and the individual contributions
## cannot be deduced from this graph. A suitable improvement is to
## display the multivariate time series as a set of stacked colored
## polygons with a time line at the bottom.

## The =xyplot= function displays information according to the class
## of its first argument (methods) and to the =panel= function. We
## will use the =xyplot.zoo= method (equivalent to the =xyplot.ts=
## method) with a new custom =panel= function.
## This new function has four main arguments, three of them calculated by
## =xyplot= (=x=, =y= and =groups=) and a new one, =origin=. Of
## course, it includes the =...= argument to provide additional
## arguments.

## \index{Panel function}
## \index{superpose.polygon@\texttt{superpose.polygon}}
## \index{trellis.par.get@\texttt{trellis.par.get}}
## \index{apply@\texttt{apply}}
## \index{sapply@\texttt{sapply}}
## \index{unstack@\texttt{unstack}}
## \index{panel.text@\texttt{panel.text}}
## \index{panel.polygon@\texttt{panel.polygon}}

panel.flow <- function(x, y, groups, origin, ..., pos=4){
  dat <- data.frame(x=x, y=y, groups=groups)
  nVars <- nlevels(groups)
  groupLevels <- levels(groups)

  yWide <- unstack(dat, y~groups)

  ##Havre.Hetzler.ea2002
  if (origin=='themeRiver') origin= -1/2*rowSums(yWide)
  else origin=0 

  yWide <- cbind(origin=origin, yWide)
  yCumSum <- t(apply(yWide, 1, cumsum))
  Y <- as.data.frame(sapply(seq_len(nVars),
                            function(iCol)c(yCumSum[,iCol+1],
                                            rev(yCumSum[,iCol]))))
  names(Y) <- levels(groups)
  y <- stack(Y)$values

  xWide <- unstack(dat, x~groups)
  x <- rep(c(xWide[,1], rev(xWide[,1])), nVars)

  groups <- rep(groups, each=2)
  
  superpose.polygon <- trellis.par.get("superpose.polygon")

  col = superpose.polygon$col
  border = superpose.polygon$border 
  lwd = superpose.polygon$lwd 

  for (i in seq_len(nVars)){
    xi <- x[groups==groupLevels[i]]
    yi <- y[groups==groupLevels[i]]
    panel.polygon(xi, yi, border=border,
                  lwd=lwd, col=col[i])
    N <- length(xi)/2
    panel.text(xi[N], (yi[N]+yi[N+1])/2,
               labels=groupLevels[i],
               col=col[i], pos=pos, ...)
  }
}

## The first step is to create a =data.frame= with the coordinates
## and with the =groups= factor. The value and number of its levels
## will be used in the main step of this =panel= function.With this
## =data.frame= we have to calculate the =y= and =x= coordinates for
## each group to get an stacked set of polygons.

## This =data.frame= is in the /long/ format, with a row for each
## observation where the =group= column identifies the
## variable. Thus, it has to transformed to the /wide/ format, with a
## column for each variable. With the =unstack= function a new
## =data.frame= is produced, whose columns are defined according to
## the formula =y ~ groups= and with a row for each time
## position. The stack of polygons is the result of the cumulative
## sum of each row (=apply(yWide, 1, cumsum)=). The origin of this sum
## is defined with the corresponding =origin= argument: with =origin
## = 'themeRiver'= the polygons are arranged in a symmetric way.

## Each column of this matrix of cumulative sums defines the =y=
## coordinate of each variable (where =origin= is now the first
## variable). The polygon of each variable is comprised between this
## curve (=iCol+1=) and the one of the previous variable (=iCol=). In
## order to get a closed polygon, the coordinates of the inferior
## limit are in reverse order. This new =data.frame= (=Y=) is in the
## /wide/ format, but =xyplot= requires the information in the /long/
## format: the =y= coordinates of the polygons are extracted from the
## =values= column of the /long/ version of this =data.frame=.

## The =x= coordinates are produced in easier way. Again, =unstack=
## produces a =data.frame= with column for each variable and a row
## for each time position but now, since the =x= coordinates are the same
## for the set of polygons, the corresponding vector is constructed
## directly with a combination of concatenation a repetition.

## Finally, the =groups= vector is produced repeating each element of
## the column of the original =data.frame= (=dat$groups=) twice to
## account for the forward and reverse curves of the corresponding
## polygon.

## The last step before displaying the polygons is to acquire the
## graphical settings. The information retrieved with
## =trellis.par.get= is transferred to the corresponding arguments of
## =panel.polygon=.

## Everything is ready for constructing the polygons. With a =for=
## loop the coordinates of the corresponding group are extracted from
## the =x= and =y= vectors. A polygon is displayed with
## =panel.polygon= and labelled with =panel.text= (where the labels
## are the =levels= of the original =groups= variable,
## =groupLevels=). Both the polygon and its label share the same
## color (=col[i]=).

## With this panel function, =xyplot= will display a set of stacked
## polygons corresponding to the multivariate time series. However,
## the graphical window is not large enough and part of the polygons
## fall out of it. Why?

pdf(file="~/Dropbox/chapman/book/figs/ThemeRiverError.pdf")
library(colorspace)

nCols <- ncol(unemployUSA)
pal <- rainbow_hcl(nCols, c=70, l=75, start=30, end=300)
myTheme <- custom.theme(fill=pal, lwd=0.4)

xyplot(unemployUSA, superpose=TRUE, auto.key=FALSE,
       panel=panel.flow, origin='themeRiver',
       par.settings=myTheme, cex=0.4, offset=0,
       scales=list(y=list(draw=FALSE)))
dev.off()

## The problem is that =lattice= makes a preliminary estimate of the
## window size using a default =prepanel= function which is unaware
## of the internal calculations of our new =panel.flow= function. The
## solution is to define a new =prepanel.flow= function. The input
## arguments and first lines are exactly the same as in
## =panel.flow=. The output is a list whose elements are the limits
## for each axis (=xlim= and =ylim=), and the sequence of differences
## (=dx= and =dy=) which can be used for the aspect and banking
## calculations. The limits of the x-axis are defined with the range
## of the time index, while the limits of the y-axis are calculated
## with the minimum of the first column of =yyy= (the origin line)
## and with the maximum of its last column (the upper line of the
## cumulative sum).

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

## The output of =xyplot= using both the panel and prepanel functions
## is displayed in the figure fig:unemployUSAThemeRiver.

## #+CAPTION: Theme River of unemployment at USA
## #+LABEL: fig:unemployUSAThemeRiver

pdf(file="/home/oscar/Dropbox/chapman/book/figs/unemployUSAThemeRiver.pdf")
xyplot(unemployUSA, superpose=TRUE, auto.key=FALSE,
       panel=panel.flow, prepanel=prepanel.flow,
       origin='themeRiver', scales=list(y=list(draw=FALSE)),
       par.settings=myTheme, cex=0.4, offset=0)
dev.off()
