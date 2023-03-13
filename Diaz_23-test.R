# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("GGally", "readxl", "rgdal", "rgeos", "sp", "spatstat", "tidyverse", "dismo", "MASS", "ggplot2", "plyr", "maps", "maptools", "raster", "geostatsp", "patchwork", "foreach")
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


# Resampling analysis

## set the working directory
setwd("C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/grids")

## Import variables and set the same extent for everyone
elevation <- raster("dem-northern.tif")
elevation_central <- raster("dem-central.tif")

## Extract cell site elevations from the Northern area
sites_northern_spdf <- SpatialPointsDataFrame(sites_northern, data.frame(sites_northern))


sites_northern$`Elevation (m)` <-  
  raster::extract(elevation, 
                  sites_northern_spdf)

## Calculate the sites densities for Northern area
sites_northern_densities <- sites_northern %$%
  elevation %>%
  density(from = 53, to = 1054, n = 1001) %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 1001) %>%
  dplyr::rename(Elevation = x,
                Frequency = y)

## Calculate possible densities across the study area using resampling
background_altitude_values <- elevation %>%
  values() %>%
  na.omit() # Drop all masked (NA) locations

# Draw 1000 random samples, and calculate their densities
background_altitude_densities <- foreach::foreach(n = 1:999, .combine = rbind) %do% {
  background_altitude_values %>%
    sample(nrow(sites_northern),
           replace = FALSE) %>%
    density(from = 53, to = 1054, n = 1001) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 1001)
} %>%
  dplyr::group_by(x) %>%
  purrrlyr::by_slice(function(x){
    quantile(x$y, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      broom::tidy()
  }, .collate = "cols") %>%
  magrittr::set_names(c("Elevation", "Lower CI", "Frequency", "Upper CI"))

# Plot distributions
ggplot() +
  geom_line(data = background_altitude_densities,
            mapping = aes(x = Elevation,
                          y = Frequency)) +
  geom_ribbon(data = background_altitude_densities,
              mapping = aes(x = Elevation,
                            ymin = `Lower CI`,
                            ymax = `Upper CI`),
              alpha = 0.3) +
  geom_line(data = sites_northern_densities,
            mapping = aes(x = Elevation,
                          y = Frequency),
            color = "red")




## Extract cell site elevations from the Central area
sites_central_spdf <- SpatialPointsDataFrame(sites_central, data.frame(sites_central))


sites_central$`Elevation (m)` <-  
  raster::extract(elevation_central, 
                  sites_central_spdf)

## Calculate the sites densities for Northern area
sites_central_densities <- sites_central %$%
  elevation_central %>%
  density(from = 245, to = 834, n = 589) %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 589) %>%
  dplyr::rename(Elevation = x,
                Frequency = y)

## Calculate possible densities across the study area using resampling
background_altitude_values <- elevation %>%
  values() %>%
  na.omit() # Drop all masked (NA) locations

# Draw 1000 random samples, and calculate their densities
background_altitude_densities <- foreach::foreach(n = 1:999, .combine = rbind) %do% {
  background_altitude_values %>%
    sample(nrow(sites_central),
           replace = FALSE) %>%
    density(from = 245, to = 834, n = 589) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 1001)
} %>%
  dplyr::group_by(x) %>%
  purrrlyr::by_slice(function(x){
    quantile(x$y, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      broom::tidy()
  }, .collate = "cols") %>%
  magrittr::set_names(c("Elevation", "Lower CI", "Frequency", "Upper CI"))

# Plot distributions
ggplot() +
  geom_line(data = background_altitude_densities,
            mapping = aes(x = Elevation,
                          y = Frequency)) +
  geom_ribbon(data = background_altitude_densities,
              mapping = aes(x = Elevation,
                            ymin = `Lower CI`,
                            ymax = `Upper CI`),
              alpha = 0.3) +
  geom_line(data = sites_central_densities,
            mapping = aes(x = Elevation,
                          y = Frequency),
            color = "red")



#Boxplot
## set the working directory
setwd("C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv")

## Import files
boxplot_variables_northern <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/boxplot_variables_northern.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")
boxplot_variables_central <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/boxplot_variables_central.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")


## Sites vs random sites in Northern Mountain ranges
## Change the order in x axis
levels(boxplot_variables_northern$Type)
boxplot_variables_northern$Type = factor(boxplot_variables_northern$Type, levels=c("Sites", "Random sites"))
levels(boxplot_variables_northern$Type)

## Create the ALTm boxplot
p <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=ALTm, fill=Type ))+geom_jitter(aes(x=Type, y=ALTm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ALTm (m.a.s.l.)")+xlab("Type")+ggtitle("a. ALTm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")

## Create the TPI100m boxplot
p1 <- ggplot(data = boxplot_variables_northern, col=c(123,234))+geom_boxplot(aes(x=Type, y=TPI100m, fill=Type ))+geom_jitter(aes(x=Type, y=TPI100m,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI100m")+xlab("Type")+ggtitle("b. TPI100m in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI500m boxplot
p2 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=TPI500m, fill=Type ))+geom_jitter(aes(x=Type, y=TPI500m,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI500m")+xlab("Type")+ggtitle("c. TPI500m in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI500m boxplot
p3 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=TPI1000m, fill=Type ))+geom_jitter(aes(x=Type, y=TPI500m,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI1000m")+xlab("Type")+ggtitle("d. TPI1000m in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLOm boxplot
p4 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=SLOm, fill=Type ))+geom_jitter(aes(x=Type, y=SLOm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLOm (degrees)")+xlab("Type")+ggtitle("e. SLOm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ASPm boxplot
p5 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=ASPm, fill=Type ))+geom_jitter(aes(x=Type, y=ASPm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ASPm")+xlab("Type")+ggtitle("f. ASPm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROEm boxplot
p6 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=HYDROEm, fill=Type ))+geom_jitter(aes(x=Type, y=HYDROEm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROEm (m.)")+xlab("Type")+ggtitle("g. HYDROEm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROCm boxplot
p7 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=HYDROCm, fill=Type ))+geom_jitter(aes(x=Type, y=HYDROCm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROCm (minutes)")+xlab("Type")+ggtitle("h. HYDROCm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WETm boxplot
p8 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=WETm, fill=Type ))+geom_jitter(aes(x=Type, y=WETm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WETm (minutes)")+xlab("Type")+ggtitle("i. WETm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none") 

## Create the GEOLEm boxplot
p9 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=GEOLEm, fill=Type ))+geom_jitter(aes(x=Type, y=GEOLEm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLEm (m.)")+xlab("Type")+ggtitle("a. GEOLEm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")

## Create the GEOLCm boxplot
p10 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=GEOLCm, fill=Type ))+geom_jitter(aes(x=Type, y=GEOLCm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLCm (minutes)")+xlab("Type")+ggtitle("b. GEOLCm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the VISPRm boxplot
p11 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=VISPRm, fill=Type ))+geom_jitter(aes(x=Type, y=VISPRm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("VISPRm")+xlab("Type")+ggtitle("c. VISPRm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none") 

## Create the LCPCm boxplot
p12 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=LCPCm, fill=Type ))+geom_jitter(aes(x=Type, y=LCPCm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("LCPCm (minutes)")+xlab("Type")+ggtitle("d. LCPCm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none") 

## Create the TOTINSm boxplot
p13 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=TOTINSm, fill=Type ))+geom_jitter(aes(x=Type, y=TOTINSm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TOTINSm")+xlab("Type")+ggtitle("e. TOTINSm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")

## Create the DIRINSm boxplot
p14 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=DIRINSm, fill=Type ))+geom_jitter(aes(x=Type, y=DIRINSm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIRINSm")+xlab("Type")+ggtitle("f. DIRINSm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")

## Create the DIFINSm boxplot
p15 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=DIFINSm, fill=Type ))+geom_jitter(aes(x=Type, y=DIFINSm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIFINSm")+xlab("Type")+ggtitle("g. DIRINSm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WINDm boxplot
p16 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=WINDm, fill=Type ))+geom_jitter(aes(x=Type, y=WINDm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WINDm")+xlab("Type")+ggtitle("h. WINDm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  


## create Figure X
(p | p1 | p2) /
(p3 | p4 | p5) /
(p6 | p7 | p8)

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)

## create Figure X
(p9 | p10 | p11) /
(p12 | p13 | p14)/
(p15 | p16 | guide_area())

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)




## Sites vs random sites in Central Mountain ranges
## Change the order in x axis
levels(boxplot_variables_central$Type)
boxplot_variables_central$Type = factor(boxplot_variables_central$Type, levels=c("Sites", "Random sites"))
levels(boxplot_variables_central$Type)

## Create the ALTm boxplot
p <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=ALTm, fill=Type ))+geom_jitter(aes(x=Type, y=ALTm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ALTm (m.a.s.l.)")+xlab("Type")+ggtitle("a. ALTm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")

## Create the TPI100m boxplot
p1 <- ggplot(data = boxplot_variables_central, col=c(123,234))+geom_boxplot(aes(x=Type, y=TPI100m, fill=Type ))+geom_jitter(aes(x=Type, y=TPI100m,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI100m")+xlab("Type")+ggtitle("b. TPI100m in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI500m boxplot
p2 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=TPI500m, fill=Type ))+geom_jitter(aes(x=Type, y=TPI500m,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI500m")+xlab("Type")+ggtitle("c. TPI500m in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI500m boxplot
p3 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=TPI1000m, fill=Type ))+geom_jitter(aes(x=Type, y=TPI500m,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI1000m")+xlab("Type")+ggtitle("d. TPI1000m in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLOm boxplot
p4 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=SLOm, fill=Type ))+geom_jitter(aes(x=Type, y=SLOm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLOm (degrees)")+xlab("Type")+ggtitle("e. SLOm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ASPm boxplot
p5 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=ASPm, fill=Type ))+geom_jitter(aes(x=Type, y=ASPm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ASPm")+xlab("Type")+ggtitle("f. ASPm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROEm boxplot
p6 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=HYDROEm, fill=Type ))+geom_jitter(aes(x=Type, y=HYDROEm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROEm (m.)")+xlab("Type")+ggtitle("g. HYDROEm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROCm boxplot
p7 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=HYDROCm, fill=Type ))+geom_jitter(aes(x=Type, y=HYDROCm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROCm (minutes)")+xlab("Type")+ggtitle("h. HYDROCm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WETm boxplot
p8 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=WETm, fill=Type ))+geom_jitter(aes(x=Type, y=WETm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WETm (minutes)")+xlab("Type")+ggtitle("i. WETm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLEm boxplot
p9 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=GEOLEm, fill=Type ))+geom_jitter(aes(x=Type, y=GEOLEm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLEm (m.)")+xlab("Type")+ggtitle("a. GEOLEm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLCm boxplot
p10 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=GEOLCm, fill=Type ))+geom_jitter(aes(x=Type, y=GEOLCm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLCm (minutes)")+xlab("Type")+ggtitle("b. GEOLCm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the VISPRm boxplot
p11 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=VISPRm, fill=Type ))+geom_jitter(aes(x=Type, y=VISPRm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("VISPRm")+xlab("Type")+ggtitle("c. VISPRm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the LCPCm boxplot
p12 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=LCPCm, fill=Type ))+geom_jitter(aes(x=Type, y=LCPCm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("LCPCm (minutes)")+xlab("Type")+ggtitle("d. LCPCm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TOTINSm boxplot
p13 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=TOTINSm, fill=Type ))+geom_jitter(aes(x=Type, y=TOTINSm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TOTINSm")+xlab("Type")+ggtitle("e. TOTINSm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the DIRINSm boxplot
p14 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=DIRINSm, fill=Type ))+geom_jitter(aes(x=Type, y=DIRINSm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIRINSm")+xlab("Type")+ggtitle("f. DIRINSm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the DIFINSm boxplot
p15 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=DIFINSm, fill=Type ))+geom_jitter(aes(x=Type, y=DIFINSm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIFINSm")+xlab("Type")+ggtitle("g. DIRINSm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WINDm boxplot
p16 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=WINDm, fill=Type ))+geom_jitter(aes(x=Type, y=WINDm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WINDm")+xlab("Type")+ggtitle("h. WINDm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  


## create Figure X
(p | p1 | p2) /
(p3 | p4 | p5) /
(p6 | p7 | p8)

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)

## create Figure X
(p9 | p10 | p11) /
(p12 | p13 | p14)/
(p15 | p16 | guide_area())

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)



## Northern Mountain ranges sites vs Central Mountain ranges sites
## Import files
boxplot_northern_vs_central <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/boxplot_northern_vs_central.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")

## Create the ALTA boxplot
p <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ALTA, fill=Area ))+geom_jitter(aes(x=Area, y=ALTA,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ALTA (m.a.s.l.)")+xlab("Area")+ggtitle("a. ALTA for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ALTm boxplot
p1 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ALTm, fill=Area ))+geom_jitter(aes(x=Area, y=ALTm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ALTm (m.a.s.l.)")+xlab("Area")+ggtitle("b. ALTm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")

## Create the TPI100 boxplot
p2 <- ggplot(data = boxplot_northern_vs_central, col=c(123,234))+geom_boxplot(aes(x=Area, y=TPI100, fill=Area ))+geom_jitter(aes(x=Area, y=TPI100,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI100")+xlab("Area")+ggtitle("c. TPI100 for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI100m boxplot
p3 <- ggplot(data = boxplot_northern_vs_central, col=c(123,234))+geom_boxplot(aes(x=Area, y=TPI100m, fill=Area ))+geom_jitter(aes(x=Area, y=TPI100m,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI100m")+xlab("Area")+ggtitle("d. TPI100m for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI500 boxplot
p4 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=TPI500, fill=Area ))+geom_jitter(aes(x=Area, y=TPI500,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI500")+xlab("Area")+ggtitle("e. TPI500 for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI500m boxplot
p5 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=TPI500m, fill=Area ))+geom_jitter(aes(x=Area, y=TPI500m,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI500m")+xlab("Area")+ggtitle("f. TPI500m for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI1000 boxplot
p6 <- ggplot(data = boxplot_northern_vs_central, col=c(123,234))+geom_boxplot(aes(x=Area, y=TPI1000, fill=Area ))+geom_jitter(aes(x=Area, y=TPI1000,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI1000")+xlab("Area")+ggtitle("g. TPI1000 for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TPI1000m boxplot
p7 <- ggplot(data = boxplot_northern_vs_central, col=c(123,234))+geom_boxplot(aes(x=Area, y=TPI1000m, fill=Area ))+geom_jitter(aes(x=Area, y=TPI1000m,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TPI1000m")+xlab("Area")+ggtitle("h. TPI1000m for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ALTrA boxplot
p8 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ALTrA, fill=Area ))+geom_jitter(aes(x=Area, y=ALTrA,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ALTrA")+xlab("Area")+ggtitle("i. ALTrA for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ALTrB boxplot
p9 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ALTrB, fill=Area ))+geom_jitter(aes(x=Area, y=ALTrB,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ALTrB")+xlab("Area")+ggtitle("j. ALTrB for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLO boxplot
p10 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=SLO, fill=Area ))+geom_jitter(aes(x=Area, y=SLO,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLO")+xlab("Area")+ggtitle("a. SLO for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLOm boxplot
p11 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=SLOm, fill=Area ))+geom_jitter(aes(x=Area, y=SLOm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLOm")+xlab("Area")+ggtitle("b. SLOm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLOga boxplot
p12 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=SLOga, fill=Area ))+geom_jitter(aes(x=Area, y=SLOga,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLOga")+xlab("Area")+ggtitle("c. SLOga for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLOt boxplot
p13 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=SLOt, fill=Area ))+geom_jitter(aes(x=Area, y=SLOt,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLOt")+xlab("Area")+ggtitle("d. SLOt for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLOst boxplot
p14 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=SLOst, fill=Area ))+geom_jitter(aes(x=Area, y=SLOst,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLOst")+xlab("Area")+ggtitle("e. SLOst for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLOpi boxplot
p15 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=SLOpi, fill=Area ))+geom_jitter(aes(x=Area, y=SLOpi,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLOpi")+xlab("Area")+ggtitle("f. SLOpi for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the INCr45-15 boxplot
p16 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=INCr45.15, fill=Area ))+geom_jitter(aes(x=Area, y=INCr45.15,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("INCr45-15")+xlab("Area")+ggtitle("g. INCr45-15 for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ASP boxplot
p17 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ASP, fill=Area ))+geom_jitter(aes(x=Area, y=ASP,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ASP")+xlab("Area")+ggtitle("a. ASP for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ASPm boxplot
p18 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ASPm, fill=Area ))+geom_jitter(aes(x=Area, y=ASPm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ASPm")+xlab("Area")+ggtitle("b. ASPm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROE boxplot
p19 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROE, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROE,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROE")+xlab("Area")+ggtitle("a. HYDROE for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROEm boxplot
p20 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROEm, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROEm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROEm")+xlab("Area")+ggtitle("b. HYDROEm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROC boxplot
p21 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROC, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROC,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROC")+xlab("Area")+ggtitle("c. HYDROC for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROCm boxplot
p22 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROCm, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROCm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROCm")+xlab("Area")+ggtitle("d. HYDROCm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROV boxplot
p23 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROV, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROV,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROV (m2)")+xlab("Area")+ggtitle("e. HYDROV for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WET boxplot
p24 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=WET, fill=Area ))+geom_jitter(aes(x=Area, y=WET,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WET")+xlab("Area")+ggtitle("f. WET for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WETm boxplot
p25 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=WETm, fill=Area ))+geom_jitter(aes(x=Area, y=WETm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WETm")+xlab("Area")+ggtitle("g. WETm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WETv boxplot
p26 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=WETv, fill=Area ))+geom_jitter(aes(x=Area, y=WETv,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WETv (m2)")+xlab("Area")+ggtitle("h. WETv for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLE boxplot
p27 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLE, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLE,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLE")+xlab("Area")+ggtitle("a. GEOLE for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLEm boxplot
p28 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLEm, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLEm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLEm")+xlab("Area")+ggtitle("b. GEOLEm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLC boxplot
p29 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLC, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLC,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLC")+xlab("Area")+ggtitle("c. GEOLC for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLCm boxplot
p30 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLCm, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLCm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLCm")+xlab("Area")+ggtitle("d. GEOLCm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLV boxplot
p31 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLV, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLV,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLV (m2)")+xlab("Area")+ggtitle("e. GEOLV for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the CPFPCGs boxplot
p32 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=CPFPCGs, fill=Area ))+geom_jitter(aes(x=Area, y=CPFPCGs,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("CPFPCGs (m2)")+xlab("Area")+ggtitle("a. CPFPCGs for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the CPFPCDs boxplot
p33 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=CPFPCDs, fill=Area ))+geom_jitter(aes(x=Area, y=CPFPCDs,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("CPFPCDs (m2)")+xlab("Area")+ggtitle("b. CPFPCDs for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the CPFPCGc boxplot
p34 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=CPFPCGc, fill=Area ))+geom_jitter(aes(x=Area, y=CPFPCGc,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("CPFPCGc (seconds)")+xlab("Area")+ggtitle("c. CPFPCGc for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the CPFPCDc boxplot
p35 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=CPFPCDc, fill=Area ))+geom_jitter(aes(x=Area, y=CPFPCDc,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("CPFPCDc (seconds)")+xlab("Area")+ggtitle("d. CPFPCDc for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  


## create Figure X
(p | p1 | p2) /
(p3 | p4 | p5) /
(p6 | p7 | p8)/
(p9 | guide_area()| guide_area())

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)

## create Figure X
(p10 | p11 | p12) /
(p13 | p14 | p15) /
(p16 | guide_area()| guide_area())

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)

## create Figure X
(p17 | p18 | guide_area())

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)

## create Figure X
(p19 | p20 | p21) /
(p22 | p23 | p24) /
(p25 | p26| guide_area())

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)

## create Figure X
(p27 | p28 | p29) /
(p30 | p31 | guide_area())

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)

## create Figure X
(p32 | p33 | p34) /
(p35 | guide_area() | guide_area())

png(file = "~/figures/FigureX.png",width = 2970,height = 2100)