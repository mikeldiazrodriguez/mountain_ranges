# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("GGally", "readxl", "rgdal", "rgeos", "sp", "spatstat", "tidyverse", "dismo", "MASS", "ggplot2", "plyr", "maps", "maptools", "raster", "geostatsp", "patchwork", "foreach", "purrrlyr")
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
elevation <- raster("ALTA_northern.tif")
elevation_central <- raster("ALTA_central.tif")

TPI100_central <- raster("TPI100_central.asc")

TPI500_central <- raster("TPI500_central.asc")

## Extract cell site elevations from the Northern area
sites_northern_spdf <- SpatialPointsDataFrame(sites_northern, data.frame(sites_northern))


sites_northern$`Elevation (m)` <-  
  raster::extract(elevation, 
                  sites_northern_spdf)

## Delete the grey theme
theme_set(theme_bw())

## Obtain the mean to used it in the next step
cellStats(elevation,mean)


## Calculate the sites densities for Northern area
sites_northern_densities <- sites_northern %$%
  elevation %>%
  density(from = 0, to = 1054, n = 34) %>% # n = number of individuals per sample
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 34) %>%
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
    density(from = 0, to = 1054, n = 34) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 34)
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
            color = "red") +ggtitle("a. ALTA in Northern Mountain ranges")



###### ELEVATION ######
## Extract cell site elevations from the Central area
sites_central_spdf <- SpatialPointsDataFrame(sites_central, data.frame(sites_central))


sites_central$`Elevation (m)` <-  
  raster::extract(elevation_central, 
                  sites_central_spdf)

## NO ### Obtain the mean to used it in the next step
cellStats(elevation_central,mean)


## Calculate the sites densities for Northern area
sites_central_densities <- sites_central %$%
  elevation_central %>%
  density(from = 0, to = 900, n = 34) %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 34) %>%
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
    density(from = 0, to = 900, n = 34) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 34)
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
            color = "red") +ggtitle("a. ALTA variable in Central Mountain ranges")


##### TPI100 #####

## Extract cell site elevations from the Central area
sites_central_spdf <- SpatialPointsDataFrame(sites_central, data.frame(sites_central))


sites_central$`TPI100` <-  
  raster::extract(TPI100_central, 
                  sites_central_spdf)

## Obtain the mean to used it in the next step
cellStats(TPI100_central,mean)


## Calculate the sites densities for Northern area
sites_central_densities <- sites_central %$%
  TPI100_central %>%
  density(from = -19.6531, to = 19.1837, n = 34) %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 34) %>%
  dplyr::rename(TPI100 = x,
                Frequency = y)

## Calculate possible densities across the study area using resampling
background_TPI100_values <- TPI100_central %>%
  values() %>%
  na.omit() # Drop all masked (NA) locations

# Draw 1000 random samples, and calculate their densities
background_TPI100_densities <- foreach::foreach(n = 1:999, .combine = rbind) %do% {
  background_TPI100_values %>%
    sample(nrow(sites_central),
           replace = FALSE) %>%
    density(from = -19.6531, to = 19.1837, n = 34) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 34)
} %>%
  dplyr::group_by(x) %>%
  purrrlyr::by_slice(function(x){
    quantile(x$y, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      broom::tidy()
  }, .collate = "cols") %>%
  magrittr::set_names(c("TPI100", "Lower CI", "Frequency", "Upper CI"))

# Plot distributions
ggplot() +
  geom_line(data = background_TPI100_densities,
            mapping = aes(x = TPI100,
                          y = Frequency)) +
  geom_ribbon(data = background_TPI100_densities,
              mapping = aes(x = TPI100,
                            ymin = `Lower CI`,
                            ymax = `Upper CI`),
              alpha = 0.3) +
  geom_line(data = sites_central_densities,
            mapping = aes(x = TPI100,
                          y = Frequency),
            color = "red") +ggtitle("b. TPI100 variable in Central Mountain ranges")



##### TPI500 #####

## Extract cell site elevations from the Central area
sites_central_spdf <- SpatialPointsDataFrame(sites_central, data.frame(sites_central))


sites_central$`TPI500` <-  
  raster::extract(TPI500_central, 
                  sites_central_spdf)

## Obtain the mean to used it in the next step
cellStats(TPI500_central,mean)


## Calculate the sites densities for Northern area
sites_central_densities <- sites_central %$%
  TPI500_central %>%
  density(from = -60.73590, to = 62.37790, n = 34) %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 34) %>%
  dplyr::rename(TPI500 = x,
                Frequency = y)

## Calculate possible densities across the study area using resampling
background_TPI500_values <- TPI500_central %>%
  values() %>%
  na.omit() # Drop all masked (NA) locations

# Draw 1000 random samples, and calculate their densities
background_TPI500_densities <- foreach::foreach(n = 1:999, .combine = rbind) %do% {
  background_TPI500_values %>%
    sample(nrow(sites_central),
           replace = FALSE) %>%
    density(from = -60.73590, to = 62.37790, n = 34) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 34)
} %>%
  dplyr::group_by(x) %>%
  purrrlyr::by_slice(function(x){
    quantile(x$y, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      broom::tidy()
  }, .collate = "cols") %>%
  magrittr::set_names(c("TPI500", "Lower CI", "Frequency", "Upper CI"))

# Plot distributions
ggplot() +
  geom_line(data = background_TPI500_densities,
            mapping = aes(x = TPI500,
                          y = Frequency)) +
  geom_ribbon(data = background_TPI500_densities,
              mapping = aes(x = TPI500,
                            ymin = `Lower CI`,
                            ymax = `Upper CI`),
              alpha = 0.3) +
  geom_line(data = sites_central_densities,
            mapping = aes(x = TPI500,
                          y = Frequency),
            color = "red") +ggtitle("c. TPI500 variable in Central Mountain ranges")

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

## Delete the grey theme
theme_set(theme_bw())

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
p15 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=DIFINSm, fill=Type ))+geom_jitter(aes(x=Type, y=DIFINSm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIFINSm")+xlab("Type")+ggtitle("g. DIFINSm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WINDm boxplot
p16 <- ggplot(data = boxplot_variables_northern,col=c(123,234))+geom_boxplot(aes(x=Type, y=WINDm, fill=Type ))+geom_jitter(aes(x=Type, y=WINDm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WINDm")+xlab("Type")+ggtitle("h. WINDm in Northern Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  


## create Figure X
(p | p1 | p2) /
(p3 | p4 | p5) /
(p6 | p7 | p8)

png(file = "~/figures/FigureX.png",width = 1506,height = 750)

## create Figure X
(p9 | p10 | p11) /
(p12 | p13 | p14)/
(p15 | p16 | guide_area())

png(file = "~/figures/FigureX.png",width = 1506,height = 750)




## Sites vs random sites in Central Mountain ranges
## Change the order in x axis
levels(boxplot_variables_central$Type)
boxplot_variables_central$Type = factor(boxplot_variables_central$Type, levels=c("Sites", "Random sites"))
levels(boxplot_variables_central$Type)

## Delete the grey theme
theme_set(theme_bw())

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
p15 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=DIFINSm, fill=Type ))+geom_jitter(aes(x=Type, y=DIFINSm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIFINSm")+xlab("Type")+ggtitle("g. DIFINSm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WINDm boxplot
p16 <- ggplot(data = boxplot_variables_central,col=c(123,234))+geom_boxplot(aes(x=Type, y=WINDm, fill=Type ))+geom_jitter(aes(x=Type, y=WINDm,fill=Type),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WINDm")+xlab("Type")+ggtitle("h. WINDm in Central Mountain ranges")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  


## create Figure X
(p | p1 | p2) /
(p3 | p4 | p5) /
(p6 | p7 | p8)

png(file = "~/figures/FigureX.png",width = 1506,height = 750)

## create Figure X
(p9 | p10 | p11) /
(p12 | p13 | p14)/
(p15 | p16 | guide_area())

png(file = "~/figures/FigureX.png",width = 1506,height = 750)



## Northern Mountain ranges sites vs Central Mountain ranges sites
## Import files
boxplot_northern_vs_central <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/boxplot_northern_vs_central.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")

## Delete the grey theme
theme_set(theme_bw())

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
p8 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ALTrA, fill=Area ))+geom_jitter(aes(x=Area, y=ALTrA,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ALTrA")+xlab("Area")+ggtitle("a. ALTrA for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ALTrB boxplot
p9 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ALTrB, fill=Area ))+geom_jitter(aes(x=Area, y=ALTrB,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ALTrB")+xlab("Area")+ggtitle("b. ALTrB for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLO boxplot
p10 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=SLO, fill=Area ))+geom_jitter(aes(x=Area, y=SLO,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLO")+xlab("Area")+ggtitle("i. SLO for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the SLOm boxplot
p11 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=SLOm, fill=Area ))+geom_jitter(aes(x=Area, y=SLOm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("SLOm")+xlab("Area")+ggtitle("a. SLOm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

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
p17 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ASP, fill=Area ))+geom_jitter(aes(x=Area, y=ASP,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ASP")+xlab("Area")+ggtitle("b. ASP for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the ASPm boxplot
p18 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=ASPm, fill=Area ))+geom_jitter(aes(x=Area, y=ASPm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("ASPm")+xlab("Area")+ggtitle("c. ASPm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROE boxplot
p19 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROE, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROE,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROE (m.)")+xlab("Area")+ggtitle("d. HYDROE for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROEm boxplot
p20 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROEm, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROEm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROEm (m.)")+xlab("Area")+ggtitle("e. HYDROEm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROC boxplot
p21 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROC, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROC,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROC (minutes)")+xlab("Area")+ggtitle("f. HYDROC for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROCm boxplot
p22 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROCm, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROCm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROCm (minutes)")+xlab("Area")+ggtitle("g. HYDROCm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the HYDROV boxplot
p23 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=HYDROV, fill=Area ))+geom_jitter(aes(x=Area, y=HYDROV,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("HYDROV (ha)")+xlab("Area")+ggtitle("h. HYDROV for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WET boxplot
p24 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=WET, fill=Area ))+geom_jitter(aes(x=Area, y=WET,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WET (minutes)")+xlab("Area")+ggtitle("h. WET for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WETm boxplot
p25 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=WETm, fill=Area ))+geom_jitter(aes(x=Area, y=WETm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WETm (minutes)")+xlab("Area")+ggtitle("i. WETm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WETv boxplot
p26 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=WETv, fill=Area ))+geom_jitter(aes(x=Area, y=WETv,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WETv (ha)")+xlab("Area")+ggtitle("i. WETv for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLE boxplot
p27 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLE, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLE,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLE (m.)")+xlab("Area")+ggtitle("a. GEOLE for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLEm boxplot
p28 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLEm, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLEm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLEm (m.)")+xlab("Area")+ggtitle("b. GEOLEm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLC boxplot
p29 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLC, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLC,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLC (minutes)")+xlab("Area")+ggtitle("c. GEOLC for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLCm boxplot
p30 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLCm, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLCm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLCm (minutes)")+xlab("Area")+ggtitle("d. GEOLCm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the GEOLV boxplot
p31 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=GEOLV, fill=Area ))+geom_jitter(aes(x=Area, y=GEOLV,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("GEOLV (ha)")+xlab("Area")+ggtitle("a. GEOLV for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the CPFPCGs boxplot
p32 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=CPFPCGs, fill=Area ))+geom_jitter(aes(x=Area, y=CPFPCGs,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("CPFPCGs (ha)")+xlab("Area")+ggtitle("b. CPFPCGs for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the CPFPCDs boxplot
p33 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=CPFPCDs, fill=Area ))+geom_jitter(aes(x=Area, y=CPFPCDs,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("CPFPCDs (ha)")+xlab("Area")+ggtitle("c. CPFPCDs for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the CPFPCGc boxplot
p34 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=CPFPCGc, fill=Area ))+geom_jitter(aes(x=Area, y=CPFPCGc,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("CPFPCGc (minutes)")+xlab("Area")+ggtitle("e. CPFPCGc for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the CPFPCDc boxplot
p35 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=CPFPCDc, fill=Area ))+geom_jitter(aes(x=Area, y=CPFPCDc,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("CPFPCDc (minutes)")+xlab("Area")+ggtitle("f. CPFPCDc for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the VISC boxplot
p36 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=VISC, fill=Area ))+geom_jitter(aes(x=Area, y=VISC,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("VISC (ha)")+xlab("Area")+ggtitle("d. VISC for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the VISZ boxplot
p37 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=VISZ, fill=Area ))+geom_jitter(aes(x=Area, y=VISZ,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("VISZ (ha)")+xlab("Area")+ggtitle("e. VISZ for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the VISPR boxplot
p38 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=VISPR, fill=Area ))+geom_jitter(aes(x=Area, y=VISPR,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("VISPR")+xlab("Area")+ggtitle("g. VISPR for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the VISPRm boxplot
p39 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=VISPRm, fill=Area ))+geom_jitter(aes(x=Area, y=VISPRm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("VISPRm")+xlab("Area")+ggtitle("h. VISPRm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the LCPC boxplot
p40 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=LCPC, fill=Area ))+geom_jitter(aes(x=Area, y=LCPC,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("LCPC (minutes)")+xlab("Area")+ggtitle("i. LCPC for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the LCPCm boxplot
p41 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=LCPCm, fill=Area ))+geom_jitter(aes(x=Area, y=LCPCm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("LCPCm (minutes)")+xlab("Area")+ggtitle("a. LCPCm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TOTINS boxplot
p42 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=TOTINS, fill=Area ))+geom_jitter(aes(x=Area, y=TOTINS,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TOTINS")+xlab("Area")+ggtitle("b. TOTINS for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the TOTINSm boxplot
p43 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=TOTINSm, fill=Area ))+geom_jitter(aes(x=Area, y=TOTINSm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("TOTINSm")+xlab("Area")+ggtitle("c. TOTINSm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the DIRINS boxplot
p44 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=DIRINS, fill=Area ))+geom_jitter(aes(x=Area, y=DIRINS,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIRINS")+xlab("Area")+ggtitle("d. DIRINS for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the DIRINSm boxplot
p45 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=DIRINSm, fill=Area ))+geom_jitter(aes(x=Area, y=DIRINSm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIRINSm")+xlab("Area")+ggtitle("e. DIRINSm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the DIFINS boxplot
p46 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=DIFINS, fill=Area ))+geom_jitter(aes(x=Area, y=DIFINS,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIFINS")+xlab("Area")+ggtitle("f. DIFINS for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the DIFINSm boxplot
p47 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=DIFINSm, fill=Area ))+geom_jitter(aes(x=Area, y=DIFINSm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("DIFINSm")+xlab("Area")+ggtitle("g. DIFINSm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WIND boxplot
p48 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=WIND, fill=Area ))+geom_jitter(aes(x=Area, y=WIND,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WIND")+xlab("Area")+ggtitle("h. WIND for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## Create the WINDm boxplot
p49 <- ggplot(data = boxplot_northern_vs_central,col=c(123,234))+geom_boxplot(aes(x=Area, y=WINDm, fill=Area ))+geom_jitter(aes(x=Area, y=WINDm,fill=Area),alpha=0.6)+scale_fill_brewer(palette="PuBu")+ylab("WINDm")+xlab("Area")+ggtitle("i. WINDm for the sites of each area")+ theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")  

## create Figure X SI
(p | p1 | p2) /
  (p3 | p4 | p5) /
  (p6 | p7 | p10)

png(file = "~/figures/FigureX.png",width = 1506,height = 750)

## create Figure X Text
(p8 | p9 | p12) /
  (p13 | p14 | p15) /
  (p16 | p23 | p26)


png(file = "~/figures/FigureX.png",width = 1506,height = 750)

## create Figure X SI
(p11 | p17 | p18) /
  (p19 | p20 | p21) /
  (p22 | p24| p25)

png(file = "~/figures/FigureX.png",width = 1506,height = 750)


## create Figure X Text
(p31 | p32 | p33) /
  (p36 | p37 | guide_area())

png(file = "~/figures/FigureX.png",width = 1506,height = 600)

## create Figure X SI
(p27 | p28 | p29) /
  (p30 | p34 | p35) /
  (p38 | p39 | p40)

png(file = "~/figures/FigureX.png",width = 1506,height = 750)

## create Figure X SI
(p41 | p42 | p43) /
  (p44 | p45 | p46) /
  (p47 | p48| p49)

png(file = "~/figures/FigureX.png",width = 1506,height = 750)



## Statistic tests
## Import files
variables_northern <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/table_variables_northern.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")

## Check normality in Northern area sites (Shapiro-Wilk Test)
### ALTA
shapiro.test(variables_northern$ALTA)

### ALTm
shapiro.test(variables_northern$ALTm)

### TPI100
shapiro.test(variables_northern$TPI100)

### TPI100m
shapiro.test(variables_northern$TPI100m)

### TPI500
shapiro.test(variables_northern$TPI500)

### TPI500m
shapiro.test(variables_northern$TPI500m)

### TPI1000
shapiro.test(variables_northern$TPI1000)

### TPI1000m
shapiro.test(variables_northern$TPI1000m)

## ALTrA
shapiro.test(variables_northern$ALTrA)

## ALTrB
shapiro.test(variables_northern$ALTrB)

## SLO
shapiro.test(variables_northern$SLO)

## SLOm
shapiro.test(variables_northern$SLOm)

## SLOga
shapiro.test(variables_northern$SLOga)

## SLOt
shapiro.test(variables_northern$SLOt)

## SLOst
shapiro.test(variables_northern$SLOst)

## SLOpi
shapiro.test(variables_northern$SLOpi)

## INCr45.15
shapiro.test(variables_northern$INCr45.15)

## ASP
shapiro.test(variables_northern$ASP)

## ASPm
shapiro.test(variables_northern$ASPm)

## HYDROE
shapiro.test(variables_northern$HYDROE)

## HYDROEm
shapiro.test(variables_northern$HYDROEm)

## HYDROC
shapiro.test(variables_northern$HYDROC)

## HYDROCm
shapiro.test(variables_northern$HYDROCm)

## HYDROV
shapiro.test(variables_northern$HYDROV)

## WET
shapiro.test(variables_northern$WET)

## WETm
shapiro.test(variables_northern$WETm)

## WETv
shapiro.test(variables_northern$WETv)

## GEOLE
shapiro.test(variables_northern$GEOLE)

## GEOLEm
shapiro.test(variables_northern$GEOLEm)

## GEOLC
shapiro.test(variables_northern$GEOLC)

## GEOLCm
shapiro.test(variables_northern$GEOLCm)

## GEOLV
shapiro.test(variables_northern$GEOLV)

## CPFPCGs
shapiro.test(variables_northern$CPFPCGs)

## CPFPCDs
shapiro.test(variables_northern$CPFPCDs)

## CPFPCGc
shapiro.test(variables_northern$CPFPCGc)

## CPFPCGDc
shapiro.test(variables_northern$CPFPCDc)

## VISC
shapiro.test(variables_northern$VISC)

## VISZ
shapiro.test(variables_northern$VISZ)

## VISPR
shapiro.test(variables_northern$VISPR)

## VISPRm
shapiro.test(variables_northern$VISPRm)

## LCPC
shapiro.test(variables_northern$LCPC)

## LCPCm
shapiro.test(variables_northern$LCPCm)

## TOTINS
shapiro.test(variables_northern$TOTINS)

## TOTINSm
shapiro.test(variables_northern$TOTINSm)

## DIRINS
shapiro.test(variables_northern$DIRINS)

## DIRINSm
shapiro.test(variables_northern$DIRINSm)

## DIFINS
shapiro.test(variables_northern$DIFINS)

## DIFINSm
shapiro.test(variables_northern$DIFINSm)

## WIND
shapiro.test(variables_northern$WIND)

## WINDm
shapiro.test(variables_northern$WINDm)


########### random sites ##################

## Import files
random_sites_northern <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/random_sites_variables_northern.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")

## Check normality in Northern area random sites (Shapiro-Wilk Test)
### ALTA
shapiro.test(random_sites_northern$ALT) #NO

### ALTm
shapiro.test(random_sites_northern$ALTm)

### TPI100
shapiro.test(random_sites_northern$TPI100)#NO

### TPI100m
shapiro.test(random_sites_northern$TPI100m)

### TPI500
shapiro.test(random_sites_northern$TPI500)#NO

### TPI500m
shapiro.test(random_sites_northern$TPI500m)

### TPI1000
shapiro.test(random_sites_northern$TPI1000)#NO

### TPI1000m
shapiro.test(random_sites_northern$TPI1000m)

## ALTrA
shapiro.test(random_sites_northern$ALTrA)#NO

## ALTrB
shapiro.test(random_sites_northern$ALTrB)#NO

## SLO
shapiro.test(random_sites_northern$SLO)#NO

## SLOm
shapiro.test(random_sites_northern$SLOm)

## SLOga
shapiro.test(random_sites_northern$SLOga)#NO

## SLOt
shapiro.test(random_sites_northern$SLOt)#NO

## SLOst
shapiro.test(random_sites_northern$SLOst)#NO

## SLOpi
shapiro.test(random_sites_northern$SLOpi)#NO

## INCr45.15
shapiro.test(random_sites_northern$INCr45.15)#NO

## ASP
shapiro.test(random_sites_northern$ASP)#NO

## ASPm
shapiro.test(random_sites_northern$ASPm)

## HYDROE
shapiro.test(random_sites_northern$HYDROE)#NO

## HYDROEm
shapiro.test(random_sites_northern$HYDROEm)

## HYDROC
shapiro.test(random_sites_northern$HYDROC)#NO

## HYDROCm
shapiro.test(random_sites_northern$HYDROCm)

## HYDROV
shapiro.test(random_sites_northern$HYDROV)#NO

## WET
shapiro.test(random_sites_northern$WET)#NO

## WETm
shapiro.test(random_sites_northern$WETm)

## WETv
shapiro.test(random_sites_northern$WETv)#NO

## GEOLE
shapiro.test(random_sites_northern$GEOLE)#NO

## GEOLEm
shapiro.test(random_sites_northern$GEOLEm)

## GEOLC
shapiro.test(random_sites_northern$GEOLC)#NO

## GEOLCm
shapiro.test(random_sites_northern$GEOLCm)

## GEOLV
shapiro.test(random_sites_northern$GEOLV)#NO

## CPFPCGs
shapiro.test(random_sites_northern$CPFPCGs)#NO

## CPFPCDs
shapiro.test(random_sites_northern$CPFPCDs)#NO

## CPFPCGc
shapiro.test(random_sites_northern$CPFPCGc)#NO

## CPFPCGDc
shapiro.test(random_sites_northern$CPFPCDc)#NO

## VISC
shapiro.test(random_sites_northern$VISC)#NO

## VISZ
shapiro.test(random_sites_northern$VISZ)#NO

## VISPR
shapiro.test(random_sites_northern$VISPR)#NO

## VISPRm
shapiro.test(random_sites_northern$VISPRm)

## LCPC
shapiro.test(random_sites_northern$LCPC)#NO

## LCPCm
shapiro.test(random_sites_northern$LCPCm)

## TOTINS
shapiro.test(random_sites_northern$TOTINS)#NO

## TOTINSm
shapiro.test(random_sites_northern$TOTINSm)

## DIRINS
shapiro.test(random_sites_northern$DIRINS)#NO

## DIRINSm
shapiro.test(random_sites_northern$DIRINSm)

## DIFINS
shapiro.test(random_sites_northern$DIFINS)#NO

## DIFINSm
shapiro.test(random_sites_northern$DIFINSm)

## WIND
shapiro.test(random_sites_northern$WIND)#NO

## WINDm
shapiro.test(random_sites_northern$WINDm)


## Import files
fisher_northern <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/boxplot_variables_northern.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")

## Check homoscedasticity (Fisher Test)
var.test(fisher_northern$ALTm ~ fisher_northern$Type, alternative='two.sided')

t.test(ALTm ~ Type, data = fisher_northern)



######   Check statistic tests ####

## Northern Area ##
## Import files
test_northern <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/boxplot_variables_northern.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")

## Install and load Rcmdr package
install.packages("Rcmdr")
library(Rcmdr)

# ALTm
## Shapiro-Wilk Test to check Normality 
normalityTest(~ALTm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(ALTm ~ Type, alternative="two.sided", data=test_northern) # Shows different variances p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(ALTm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # Shows different variances p-value < 0.05 -- Samples from different populations#
       data=test_northern)

# TPI100m
## Normality Test
normalityTest(~TPI100m, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(TPI100m ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(TPI100m~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from different populations#
       data=test_northern)


# TPI500m
## Normality Test
normalityTest(~TPI500m, test="shapiro.test", data=test_northern) # Shows normality p-value > 0.05 -- F test#

## F Test for check parametric homoscedasticity/variances (medians by group)
var.test(TPI500m ~ Type, alternative='two.sided', conf.level=.95, 
         data=test_northern) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(TPI500m~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# TPI1000m
## Normality Test
normalityTest(~TPI1000m, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(TPI1000m ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(TPI1000m~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# SLOm
## Shapiro-Wilk Test to check Normality 
normalityTest(~SLOm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(SLOm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(SLOm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# ASPm
## Shapiro-Wilk Test to check Normality 
normalityTest(~ASPm, test="shapiro.test", data=test_northern) # Shows normality p-value > 0.05 -- F test#

## F Test for check parametric homoscedasticity/variances (medians by group)
var.test(ASPm ~ Type, alternative='two.sided', conf.level=.95, 
         data=test_northern) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(ASPm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# HYDROEm
## Shapiro-Wilk Test to check Normality 
normalityTest(~HYDROEm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(HYDROEm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(HYDROEm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# HYDROCm
## Shapiro-Wilk Test to check Normality 
normalityTest(~HYDROCm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(HYDROCm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(HYDROCm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# WETm
## Shapiro-Wilk Test to check Normality 
normalityTest(~WETm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(WETm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(WETm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# GEOLEm
## Shapiro-Wilk Test to check Normality 
normalityTest(~GEOLEm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(GEOLEm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(GEOLEm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # Shows different variances p-value < 0.05 -- Samples from different populations#
       data=test_northern)


# GEOLCm
## Shapiro-Wilk Test to check Normality 
normalityTest(~GEOLCm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(GEOLCm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(GEOLCm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # Shows different variances p-value < 0.05 -- Samples from different populations#
       data=test_northern)


# VISPRm
## Shapiro-Wilk Test to check Normality 
normalityTest(~VISPRm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(VISPRm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T Student test#

## T Student Test for independent samples
t.test(VISPRm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# LCPCm
## Shapiro-Wilk Test to check Normality 
normalityTest(~LCPCm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(LCPCm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T Student test#

## T Student Test for independent samples
t.test(LCPCm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# TOTINSm
## Shapiro-Wilk Test to check Normality 
normalityTest(~TOTINSm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(TOTINSm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T Student test#

## T Student Test for independent samples
t.test(TOTINSm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# DIRINSm
## Shapiro-Wilk Test to check Normality 
normalityTest(~DIRINSm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(DIRINSm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T Student test#

## T Student Test for independent samples
t.test(DIRINSm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)


# DIFINSm
## Shapiro-Wilk Test to check Normality 
normalityTest(~DIFINSm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(DIFINSm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(DIFINSm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # Shows different variances p-value < 0.05 -- Samples from different populations#
       data=test_northern)


# WINDm
## Shapiro-Wilk Test to check Normality 
normalityTest(~WINDm, test="shapiro.test", data=test_northern) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(WINDm ~ Type, alternative="two.sided", data=test_northern) # Shows same variance p-value > 0.05 -- T Student test#

## T Student Test for independent samples
t.test(WINDm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_northern)





## Central Area ##
## Import files
test_central <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/boxplot_variables_central.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")

## Install and load Rcmdr package
install.packages("Rcmdr")
library(Rcmdr)

# ALTm
## Shapiro-Wilk Test to check Normality 
normalityTest(~ALTm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(ALTm ~ Type, alternative="two.sided", data=test_central) # Shows different variances p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(ALTm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value < 0.05 -- Samples from different populations#
       data=test_central)


# TPI100m
## Normality Test
normalityTest(~TPI100m, test="shapiro.test", data=test_central) # Shows normality p-value > 0.05 -- F test#

## F Test for check parametric homoscedasticity/variances (medians by group)
var.test(TPI100m ~ Type, alternative='two.sided', conf.level=.95, 
         data=test_central) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(TPI100m~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_central)


# TPI500m
## Normality Test
normalityTest(~TPI500m, test="shapiro.test", data=test_central) # Shows normality p-value > 0.05 -- F test#

## F Test for check parametric homoscedasticity/variances (medians by group)
var.test(TPI500m ~ Type, alternative='two.sided', conf.level=.95, 
         data=test_central) # Shows different variances p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(TPI500m~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value > 0.05 -- Samples from same population#
       data=test_central)


# TPI1000m
## Normality Test
normalityTest(~TPI1000m, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(TPI1000m ~ Type, alternative="two.sided", data=test_central) # Shows different variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(TPI1000m~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value < 0.05 -- Samples from different populations#
       data=test_central)


# SLOm
## Shapiro-Wilk Test to check Normality 
normalityTest(~SLOm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(SLOm ~ Type, alternative="two.sided", data=test_central) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(SLOm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_central)


# ASPm
## Shapiro-Wilk Test to check Normality 
normalityTest(~ASPm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(ASPm ~ Type, alternative="two.sided", data=test_central) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(ASPm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_central)


# HYDROEm
## Shapiro-Wilk Test to check Normality 
normalityTest(~HYDROEm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (meedians by group) 
wilcox.test(HYDROEm ~ Type, alternative="two.sided", data=test_central) # Shows different variances p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(HYDROEm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value < 0.05 -- Samples from different populations#
       data=test_central)


# HYDROCm
## Shapiro-Wilk Test to check Normality 
normalityTest(~HYDROCm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(HYDROCm ~ Type, alternative="two.sided", data=test_central) # Shows different variances p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(HYDROCm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value < 0.05 -- Samples from different populations#
       data=test_central)


# WETm
## Shapiro-Wilk Test to check Normality 
normalityTest(~WETm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(WETm ~ Type, alternative="two.sided", data=test_central) # Shows same variance p-value > 0.05 -- T student test#

## T Student Test for independent samples
t.test(WETm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_central)


# GEOLEm
## Shapiro-Wilk Test to check Normality 
normalityTest(~GEOLEm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(GEOLEm ~ Type, alternative="two.sided", data=test_central) # Shows different variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(GEOLEm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value < 0.05 -- Samples from different populations#
       data=test_central)


# GEOLCm
## Shapiro-Wilk Test to check Normality 
normalityTest(~GEOLCm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(GEOLCm ~ Type, alternative="two.sided", data=test_central) # Shows different variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(GEOLCm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value < 0.05 -- Samples from different populations#
       data=test_central)


# VISPRm
## Shapiro-Wilk Test to check Normality 
normalityTest(~VISPRm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(VISPRm ~ Type, alternative="two.sided", data=test_central) # Shows different variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(VISPRm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value < 0.05 -- Samples from different populations#
       data=test_central)


# LCPCm
## Shapiro-Wilk Test to check Normality 
normalityTest(~LCPCm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(LCPCm ~ Type, alternative="two.sided", data=test_central) # Shows same variance p-value > 0.05 -- T Student test#

## T Student Test for independent samples
t.test(LCPCm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_central)


# TOTINSm
## Shapiro-Wilk Test to check Normality 
normalityTest(~TOTINSm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(TOTINSm ~ Type, alternative="two.sided", data=test_central) # Shows same variance p-value > 0.05 -- T Student test#

## T Student Test for independent samples
t.test(TOTINSm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_central)


# DIRINSm
## Shapiro-Wilk Test to check Normality 
normalityTest(~DIRINSm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(DIRINSm ~ Type, alternative="two.sided", data=test_central) # Shows same variance p-value > 0.05 -- T Student test#

## T Student Test for independent samples
t.test(DIRINSm~Type, alternative='two.sided', conf.level=.95, var.equal=TRUE, # p-value > 0.05 -- Samples from same population#
       data=test_central)


# DIFINSm
## Shapiro-Wilk Test to check Normality 
normalityTest(~DIFINSm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(DIFINSm ~ Type, alternative="two.sided", data=test_central) # Shows different variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(DIFINSm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # p-value < 0.05 -- Samples from different populations#
       data=test_central)


# WINDm
## Shapiro-Wilk Test to check Normality 
normalityTest(~WINDm, test="shapiro.test", data=test_central) # Shows non normality p-value < 0.05 -- Wilcoxon test#

## Wilcoxon Test for check non parametric homoscedasticity/variances (medians by group)
wilcox.test(WINDm ~ Type, alternative="two.sided", data=test_central) # Shows different variance p-value < 0.05 -- T Welch test#

## T Welch Test for independent samples
t.test(WINDm~Type, alternative='two.sided', conf.level=.95, var.equal=FALSE, # Shows different variances p-value < 0.05 -- Samples from different populations#
       data=test_central)




######   Summary results of sites and random sites ####

## Import files
variables_northern <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/table_variables_northern.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")


## summary sites ALTm
summary(variables_northern$ALTm)

## summary sites TPI100m
summary(variables_northern$TPI100m)

## summary sites TPI500m
summary(variables_northern$TPI500m)

## summary sites TPI1000m
summary(variables_northern$TPI1000m)

## summary sites SLOm
summary(variables_northern$SLOm)

## summary sites ASPm
summary(variables_northern$ASPm)

## summary sites HYDROEm
summary(variables_northern$HYDROEm)

## summary sites HYDROCm
summary(variables_northern$HYDROCm)

## summary sites WETm
summary(variables_northern$WETm)

## summary sites GEOLEm
summary(variables_northern$GEOLEm)

## summary sites GEOLCm
summary(variables_northern$GEOLCm)

## summary sites VISPRm
summary(variables_northern$VISPRm)

## summary sites LCPCm
summary(variables_northern$LCPCm)

## summary sites TOTINSm
summary(variables_northern$TOTINSm)

## summary sites DIRINSm
summary(variables_northern$DIRINSm)

## summary sites DIFINSm
summary(variables_northern$DIFINSm)

## summary sites WINDm
summary(variables_northern$WINDm)


## Import files
variables_central <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/table_variables_central.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")


## summary sites ALTm
summary(variables_central$ALTm)

## summary sites TPI100m
summary(variables_central$TPI100m)

## summary sites TPI500m
summary(variables_central$TPI500m)

## summary sites TPI1000m
summary(variables_central$TPI1000m)

## summary sites SLOm
summary(variables_central$SLOm)

## summary sites ASPm
summary(variables_central$ASPm)

## summary sites HYDROEm
summary(variables_central$HYDROEm)

## summary sites HYDROCm
summary(variables_central$HYDROCm)

## summary sites WETm
summary(variables_central$WETm)

## summary sites GEOLEm
summary(variables_central$GEOLEm)

## summary sites GEOLCm
summary(variables_central$GEOLCm)

## summary sites VISPRm
summary(variables_central$VISPRm)

## summary sites LCPCm
summary(variables_central$LCPCm)

## summary sites TOTINSm
summary(variables_central$TOTINSm)

## summary sites DIRINSm
summary(variables_central$DIRINSm)

## summary sites DIFINSm
summary(variables_central$DIFINSm)

## summary sites WINDm
summary(variables_central$WINDm)



## Import files
random_sites_northern <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/random_sites_variables_northern.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")

## summary random sites ALTm
summary(random_sites_northern$ALTm)

## summary random sites TPI100m
summary(random_sites_northern$TPI100m)

## summary random sites TPI500m
summary(random_sites_northern$TPI500m)

## summary random sites TPI1000m
summary(random_sites_northern$TPI1000m)

## summary random sites SLOm
summary(random_sites_northern$SLOm)

## summary random sites ASPm
summary(random_sites_northern$ASPm)

## summary random sites HYDROEm
summary(random_sites_northern$HYDROEm)

## summary random sites HYDROCm
summary(random_sites_northern$HYDROCm) 

## summary random sites WETm
summary(random_sites_northern$WETm)

## summary random sites GEOLEm
summary(random_sites_northern$GEOLEm)

## summary random sites GEOLCm
summary(random_sites_northern$GEOLCm)

## summary random sites VISPRm
summary(random_sites_northern$VISPRm)

## summary random sites LCPCm
summary(random_sites_northern$LCPCm)

## summary random sites TOTINSm
summary(random_sites_northern$TOTINSm)

## summary random sites DIRINSm
summary(random_sites_northern$DIRINSm)

## summary random sites DIFINSm
summary(random_sites_northern$DIFINSm)

## summary random sites WINDm
summary(random_sites_northern$WINDm)



## Import files
random_sites_central <- read.csv(file = "C:/Users/Mikel/Documents/Curso_2022-2023/Sierras_paper/Github/mountain_ranges/csv/random_sites_variables_central.csv",header=TRUE, sep=";", stringsAsFactors=F, dec=",")


## summary random sites ALTm
summary(random_sites_central$ALTm)

## summary random sites TPI100m
summary(random_sites_central$TPI100m)

## summary random sites TPI500m
summary(random_sites_central$TPI500m)

## summary random sites TPI1000m
summary(random_sites_central$TPI1000m)

## summary random sites SLOm
summary(random_sites_central$SLOm)

## summary random sites ASPm
summary(random_sites_central$ASPm)

## summary random sites HYDROEm
summary(random_sites_central$HYDROEm)

## summary random sites HYDROCm
summary(random_sites_central$HYDROCm)

## summary random sites WETm
summary(random_sites_central$WETm)

## summary random sites GEOLEm
summary(random_sites_central$GEOLEm)

## summary random sites GEOLCm
summary(random_sites_central$GEOLCm)

## summary random sites VISPRm
summary(random_sites_central$VISPRm)

## summary random sites LCPCm
summary(random_sites_central$LCPCm)

## summary random sites TOTINSm
summary(random_sites_central$TOTINSm)

## summary random sites DIRINSm
summary(random_sites_central$DIRINSm)

## summary random sites DIFINSm
summary(random_sites_central$DIFINSm)

## summary random sites WINDm
summary(random_sites_central$WINDm)
