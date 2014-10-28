##----------------------------------------------------------------------------
## Title: Choropleth tematic map
## Author: Armando Enriquez Z.
## Date: October 28th, 2014
## Purpose: Create a choropleth tematic map with ggplot tools
##----------------------------------------------------------------------------

## R libraries required
## Probably you haven't got the packages yet: type "install.packages("package")"
library(ggplot2) ## graphics 
library(sp) ## spatial objects
library(maps)
library(maptools)
library(foreign) ## reading foreign files into R
library(mapproj)
library(RColorBrewer) ## color palettes

## Loading the information data frame
mapDesc <- read.csv("MEX_adm1.csv") ## state-level map information
str(mapDesc)

## Loading the shapefiles and converting to data frame
mapMex <- readShapePoly("MEX_adm1.shp") ## state-level map
mapMex <- fortify(mapMex)
str(mapMex)

## Loading data (life expectancy at birth per state)
lifeExp <- read.csv("life.csv", header = TRUE)

## Notice that the ids in the mapMex dataframe begin with "0", and the ids in 
## the lifeExp dataframe begin with 1. Additionally, id in mapMex is of 
## class "character".
mapMex$id <- as.numeric(mapMex$id)
mapMex$id <- mapMex$id + 1

## The variables have the same values now
summary(mapMex$id)
summary(lifeExp$id)

## We already know that the id's between lifeExp and mapMex do not match
## The problem are Baja, Baja Sur, Coahuila, Colima, Chiapas & Chihuahua
## Fix the problem by rearranging the lifeExp data frame to coincide w/ mapMex
lifeExp[2, 1] <- 3 ## Baja California to id 3
lifeExp[3, 1] <- 2 ## Baja Sur to id 2
lifeExp[5, 1] <- 7 ## Coahuila to id 7
lifeExp[6, 1] <- 8 ## Colima to id 8
lifeExp[7, 1] <- 5 ## Chiapas to id 5
lifeExp[8, 1] <- 6 ## Chihuahua to id 6

## We are almost done!
## Since lifeexp is a continuous variable, let us take the quantiles
quant <- quantile(lifeExp$lifeexp, c(0, 0.2, 0.4, 0.6, 0.8, 1))
lifeExp$quant <- cut(lifeExp$lifeexp, quant, 
                     labels = c("71.5-74.1", "74.1-74.7", 
                                "74.7-75.0", "75.0-75.2", "75.2-75.8"), 
                     include.lowest = TRUE)

## Create a 5-color palette for the variable of interest
## There are useful palettes (in hexadecimal format) at colorbrewer2.com
colors <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")


## The choropleth map
ggplot(lifeExp, aes(map_id = id, fill = quant)) +
  geom_map(map = mapMex, colour = "black") +
  scale_fill_manual(values = colors) +
  expand_limits(x = mapMex$long, y = mapMex$lat) + 
  coord_map("polyconic") +
  labs(fill = "Life expectancy 2013\nAges per quintiles") + 
  xlab("") + ylab("")