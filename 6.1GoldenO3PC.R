setwd("~/Dropbox/Home/Teaching/FDA/theCourse/Modules/Mod06PrincipleComponents")
suppressMessages(library( fields))
suppressMessages(library( fda))
suppressMessages(library( lubridate))


# load the Golden data
load("GOzone2021.rda")

# reshape as day and hour

O3Full<- matrix(NA, ncol=24, nrow=365 )
O3Full[ cbind(GOzone2021$day, GOzone2021$hour+1)] <- GOzone2021$O3

# here is a useful view of these data 
imagePlot(1:365, 0:23, O3Full, col=tim.colors(256),
          xlab="day of year ",
          ylab="hour")
title("Golden,CO hourly ozone in PPB for 2021")

# some summaries
tDay<- 1:365
# number of missing in each day
ind<- rowSums( is.na( O3Full))
table( ind)

# Note this pruning of the NAs is severe as 50+ days 
# are only missing one value

O3<- O3Full[ind==0,]
tDay<- tDay[ind==0]

dim( O3)
length( tDay)

obj<- svd( O3)

names( obj)



# check dimensions
#U
dim( obj$u)
# singular values
dim( obj$d) 
length( obj$d)
# create as a matrix
Dsvd<- diag(obj$d )
dim( Dsvd)
# V
dim( obj$v)

# sanity check we know what is going on
testMatrix<- obj$u%*% Dsvd%*%t( obj$v)
test.for.zero( testMatrix, O3)

# plot of singluar values
plot( 1:24, obj$d, log="y")

# variance explained 

plot( 1:24,  obj$d^2/ sum(obj$d^2 ), log="y",
      ylab="variance explained")
yline( 1, lty=2)
# cumulative variance

 cumsum( obj$d^2)/ sum(obj$d^2 )

# percent of variance left over 
  round( 100*(1- cumsum( obj$d^2)/ sum(obj$d^2 )),2)
  
# subjectively let's stop at 3 components. 

# svd is only unique up to sign in v and u
# will flip the first columns so this is
# more easily interpreted.
  
# MAKE SURE YOU FLIP BOTH U and V !!!  
V<- obj$v
V[,1]<- -1*V[,1]
coef<- obj$u%*%Dsvd
coef[,1]<- -1* coef[,1]


# the basis functions over 24 hours
fields.style()  
matplot( 0:23, V[,1:3], 
         type="l", lty=1, lwd=2, xlab="hour",
         ylab="PC")
title(" first 3 PCs 
      orange-1, green-2, blue-3")
  
# first three coefficents over time 

matplot(tDay, coef[,1:3], pch=16, cex=.5)

# how do coefficients work together?
pairs( coef[,1:3])

# building a particular  days ozone
mDay<- 30
#
O3PC<- V[,1]*coef[mDay,1] + 
  V[,2]*coef[mDay,2] +
  V[,3]*coef[mDay,3]
#
plot( 0:23, O3[mDay,],pch=16)
lines( 0:23, O3PC)

# redo as matrix/vector multplications
mDay<- 85
O3PC<- V[,1:3]%*%coef[mDay,1:3]  
plot( 0:23, O3[mDay,],pch=16)
lines( 0:23, O3PC)
#




# how do they vary over time?
N<- nrow( coef)
coefPast<- coef[-N,]
coefCurrent<- coef[-1,]
set.panel( 2,2)
for( I in 1:3){
plot(coefPast[,I], coefCurrent[,I],
     xlab="lag1", ylab="current")
title( paste( "PC", I))
}

for( I in 1:3){
print(
  cor(coefPast[,I], coefCurrent[,1:3] )
)
}

# magnitude of SE for a correlation coefficient
1/sqrt(N-1)

# subjectively some predictabilty in PC1, 
# maybe some in PC2 not much in PC3










