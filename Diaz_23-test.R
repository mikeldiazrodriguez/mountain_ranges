# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("GGally", "readxl", "rgdal", "rgeos", "sp", "spatstat", "tidyverse", "dismo", "MASS", "ggplot2", "plyr", "maps", "maptools", "raster", "geostatsp", "patchwork")
ipak(packages)


# set your working directory
setwd("C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/shp")

# add the study areas
area_northern <- readOGR(dsn="area_northern.shp", layer="area_northern")
area_central <- readOGR(dsn="area_central.shp", layer="area_central")

# and check it
plot(area_northern)
plot(area_central)

# add the sites
sites_northern <- readOGR(dsn="sites_northern.shp", layer="sites_northern")
sites_central <- readOGR(dsn="sites_central.shp", layer="sites_central")

# add the random sites
random_sites_northern <- readOGR(dsn="random_sites_northern.shp", layer="random_sites_northern")
random_sites_central <- readOGR(dsn="random_sites_central.shp", layer="random_sites_central")

# plot each study area, sites and the random sites and check if everything looks OK
## check the Northern Mountain range area
plot(area_northern)
plot(sites_northern, col="blue", pch=20, add=T)
plot(random_sites_northern, col="grey", pch=20, add=T)

## check the Central Mountain range area
plot(area_central)
plot(sites_central, col="red", pch=20, add=T)
plot(random_sites_central, col="grey", pch=20, add=T)

# remove any sites with the same grid reference
sites_northern <- remove.duplicates(sites_northern)
sites_central <- remove.duplicates(sites_central)

# select the points inside each study area 
sitesSubN <- sites_northern[area_northern,]
sitesSubC <- sites_central[area_central,]



# check to see that the points have been removed in each area
## Northern Mountain ranges
plot(area_northern)
plot(sitesSubN, col="red", pch=20, add=T)

## Central Mountain ranges
plot(area_central)
plot(sitesSubC, col="red", pch=20, add=T)


# set a window as the Northern area
window1 <- as.owin(area_northern)
plot(window1)

# create a ppp object
sitesSubN.ppp <- ppp(x=sitesSubN$utmx,y=sitesSubN$utmy,window=window1)

# plot the sites and the Northern area
plot(sitesSubN.ppp,pch=16,cex=0.5, main="Northern Mountain range sites")


# set a window as the Central area
window2 <- as.owin(area_central)
plot(window2)

# create a ppp object
sitesSubC.ppp <- ppp(x=sitesSubC$utmx,y=sitesSubC$utmy,window=window2)

# plot the sites and the Northern area
plot(sitesSubC.ppp,pch=16,cex=0.5, main="Central Mountain range sites")



# CSR Analysis

## Northern Mountain ranges
### Normality test for sites
shapiro.test(sitesSubN$utmx)

### Normality test for random sites
shapiro.test(random_sites_northern$utmx)

### If the alternative hypothesis is accepted, the points do not follow a normal distribution. To verify this, the K-S Test is used.
### K-S is a non-parametric test to check if the points of both samples belong to the same population.
ks.test(sitesSubN$utmx,random_sites_northern$utmx)


## Central Mountain ranges
### Normality test for sites
shapiro.test(sitesSubC$utmx)

### Normality test for random sites
shapiro.test(random_sites_central$utmx)

### K-S Test
ks.test(sitesSubC$utmx,random_sites_central$utmx)


## K, L and G Functions
# set the simulations
sims <- 99
# to get 95% envelope
nrank <- round((sims + 1) / 100 * 2.5, 0)

# K, L and G functions for Northern Mountain ranges
KfunctionN <- envelope(sitesSubN.ppp,Kest, nsim=sims,rank=nrank, correction="best") 
KinhomFunctionN <- envelope(sitesSubN.ppp,Kinhom, nsim=sims,rank=nrank, correction="best")
LFunctionN <- envelope(sitesSubN.ppp,Lest, nsim=sims,rank=nrank, correction="best")
LinhomFunctionN <- envelope(sitesSubN.ppp,Linhom, nsim=sims,rank=nrank, correction="best")
GFunctionN <- envelope(sitesSubN.ppp,Gest, nsim=sims,rank=nrank, correction="best")
GinhomFunctionN <- envelope(sitesSubN.ppp,Ginhom, nsim=sims,rank=nrank, correction="best")

# K, L and G functions for Central Mountain ranges
KfunctionC <- envelope(sitesSubC.ppp,Kest, nsim=sims,rank=nrank, correction="best") 
KinhomFunctionC <- envelope(sitesSubC.ppp,Kinhom, nsim=sims,rank=nrank, correction="best")
LFunctionC <- envelope(sitesSubC.ppp,Lest, nsim=sims,rank=nrank, correction="best")
LinhomFunctionC <- envelope(sitesSubC.ppp,Linhom, nsim=sims,rank=nrank, correction="best")
GFunctionC <- envelope(sitesSubC.ppp,Gest, nsim=sims,rank=nrank, correction="best")
GinhomFunctionC <- envelope(sitesSubC.ppp,Ginhom, nsim=sims,rank=nrank, correction="best")

# create Figure 2
par(mar=c(3, 3, 3, 3), mfrow=c(3,2))
plot(KfunctionN,main="a. K Function for the Northern area (homogeneous)",legend=FALSE)
plot(KinhomFunctionN,main="b. K Function for the Northern area (inhomogeneous)",legend=FALSE)
plot(LFunctionN,main="c. L Function for the Northern area (homogeneous)",legend=FALSE)
plot(LinhomFunctionN,main="d. L Function for the Northern area (inhomogeneous)",legend=FALSE) 
plot(GFunctionN,main="e. G Function for the Northern area (homogeneous)",legend=FALSE) 
plot(GinhomFunctionN,main="f. G Function for the Northern area (inhomogeneous)",legend=FALSE)
par(mfrow=c(1,1))
png(file = "~/output/Figure2.png",width = 900,height = 1200)
dev.off()

# create Figure 3
par(mar=c(3, 3, 3, 3), mfrow=c(3,2))
plot(KfunctionC,main="a. K Function for the Central area (homogeneous)",legend=FALSE)
plot(KinhomFunctionC,main="b. K Function for the Central area (inhomogeneous)",legend=FALSE)
plot(LFunctionC,main="c. L Function for the Central area (homogeneous)",legend=FALSE)
plot(LinhomFunctionC,main="d. L Function for the Central area (inhomogeneous)",legend=FALSE) 
plot(GFunctionC,main="e. G Function for the Central area (homogeneous)",legend=FALSE) 
plot(GinhomFunctionC,main="f. G Function for the Central area (inhomogeneous)",legend=FALSE)
par(mfrow=c(1,1))
png(file = "~/output/Figure3.png",width = 900,height = 1200)
dev.off()
