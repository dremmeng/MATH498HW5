library( fields)
setwd("~/Dropbox/Home/Teaching/FDA/theCourse/Mod05Regularization")
data(NorthAmericanRainfall)
s<- cbind( NorthAmericanRainfall$longitude, 
           NorthAmericanRainfall$latitude )
# this the average summer rainfall (in 1/10 of mm)
#recorded at ~~ 1700 locations. 

y<- NorthAmericanRainfall$precip


y<- y/254 # convert to inches in the summer

bubblePlot( s, y)

bubblePlot( s, y , highlight = FALSE,
            col=tim.colors(256), size=.7)
# thin plate spline surface with 
# lambda found by minimizing GCV
# lower left plot is the GCV function 
# plotted against eff df. 
obj<- Tps(s,y)
set.panel(2,2)
plot( obj)
set.panel()

# default plot of the surface
# many other options (see help files)
# but this is quick -- especially if one
# only has 10 minutes to get ready for class!

surface( obj)

# example of using the predict option 
# a horizontal slice of surface at 40 degrees
# (through Boulder, CO) to see transition at
# around 100 degrees longitude from dry to wet
#
s1<- seq( -125, -70,length.out=50)
sGrid<- cbind( s1, 40.0)

look<- predict( obj, sGrid)


system.time(
look1<- predictSE( obj, sGrid)
)

# this takes a while
# -- time it just to see -- 
# Yikes! takes 3 minutes. Slow because nmber of 
# locations is large and the coding is 
# conservative opting for clarity rather than speed. 


fields.style()
BF<- qnorm( 1- .05/50)
plot( s1, look, type="l",
      xlab="longitude", ylab="rainfall (in)")
envelopePlot(s1,look + BF*look1,s1, 
             look - BF*look1 )
lines( s1, look, col="blue")



