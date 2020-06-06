#Set the working directory. The bit between the "" needs to specify the path to the folder you wish to use
#you will see my file path below as am example
setwd("C:/Users/user/MScDataScience/CETM46/Tutorial11") # Note the single / (\\ will also work).

#Load the data. You may need to alter the file directory
Census.Data <-read.csv("practical_data.csv")

# Load packages
#install.packages('rgdal')
#install.packages("rgeos")
library("rgdal")
library("rgeos")

# Load the output area shapefiles
Output.Areas<- readOGR("./camden/Camden/shapefiles", "Camden_oa11")

# plots the shapefile
plot(Output.Areas)

# joins data to the shapefile
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

# sets the coordinate system to the British National Grid
proj4string(OA.Census) <- CRS("+init=EPSG:27700")


# loads packages
#install.packages('tmap')
#install.packages('leaflet')
library(tmap)
library(leaflet)

# this will prodyce a quick map of our qualification variable
qtm(OA.Census, fill = "Qualification")

# Creates a simple choropleth map of our qualification variable
tm_shape(OA.Census) + tm_fill("Qualification") 

library(RColorBrewer)

display.brewer.all()

# setting a colour palette
tm_shape(OA.Census) + tm_fill("Qualification", palette = "-Pastel2") 

# changing the intervals 
tm_shape(OA.Census) + tm_fill("Qualification", style = "quantile", palette = "Reds")

# number of levels
tm_shape(OA.Census) + tm_fill("Qualification", style = "quantile", n = 7, palette = "Reds") 

# includes a histogram in the legend
tm_shape(OA.Census) + tm_fill("Qualification", style = "pretty", n = 5, palette = "Reds", legend.hist = TRUE) 
tm_shape(OA.Census) + tm_fill("Qualification", style = "equal", n = 5, palette = "Reds", legend.hist = TRUE) 
tm_shape(OA.Census) + tm_fill("Qualification", style = "jenks", n = 5, palette = "Reds", legend.hist = TRUE) 
tm_shape(OA.Census) + tm_fill("Qualification", style = "quantile", n = 5, palette = "Reds", legend.hist = TRUE) 


# add borders
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds") + 
  tm_borders(alpha=.4)

# north arrow
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds") + 
  tm_borders(alpha=.4) +
  tm_compass()

# adds in layout, gets rid of frame
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification") + 
  tm_borders(alpha=.4) + 
  tm_compass() + 
  tm_layout(title = "Camden, London", legend.text.size = 1.1, legend.title.size = 1.4, legend.position = c("right", "top"), frame = FALSE) 

writeOGR(OA.Census, dsn = "C:/Users/user/MScDataScience/CETM46/Tutorial11", layer =  "Census_OA_Shapefile", driver="ESRI Shapefile")
