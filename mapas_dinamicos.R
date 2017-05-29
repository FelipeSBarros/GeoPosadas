rm(list=ls())
#Transformando dados
library(rgdal)
library(sp)
library(raster)
library(spatstat)

comercio <- readOGR("./shp/Shop.shp")
random <- readOGR("./shp/Random.shp")
build <- readOGR("./shp/Building.shp")
tur <- readOGR("./shp/Tour.shp")
posadas <-  readOGR("./shp/Posadas.shp")
plot(posadas)
posadas2 <-  readOGR("./shp/Posadas2.shp")
plot(posadas2)
plot(posadas, add=T, col="red")
#crs(comercio)
#Definindo SRC
#Albers <- CRS(as.character("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"))
WGS <- crs(comercio)
#Mudando SRC
#comercioAlbers <- spTransform(comercio, Albers)
posadas2 <-  spTransform(posadas2, WGS)
plot(posadas2)

#comercioDF <- comercio@coords
#head(comercioDF)
#class(comercioDF)
#comercioDF <- as.data.frame(comercioDF)
#head(as.data.frame(comercio@data[,"shop"]))
#comercioDF$shop <- as.data.frame(comercio@data[,"shop"])
#head(comercioDF)
#names(comercioDF) <- c("x", "y", "shop")
#plot(comercioDF$x, comercioDF$y)

#DENSIDADE
?as.owin
bbox(posadas2)
shopppp <- ppp(comercio@coords[,1], comercio@coords[,2],
               window=as.owin(c(bbox(posadas2)[1,], bbox(posadas2)[2,])))
buildpppp <- ppp(build@coords[,1], build@coords[,2],
               window=as.owin(c(bbox(posadas2)[1,], bbox(posadas2)[2,])))
tourppp <- ppp(tur@coords[,1], tur@coords[,2],
               window=as.owin(c(bbox(posadas2)[1,], bbox(posadas2)[2,])))
randomppp <- ppp(random@coords[,1], random@coords[,2],
               window=as.owin(c(bbox(posadas2)[1,], bbox(posadas2)[2,])))
plot(shopppp)
plot(buildpppp)
plot(tourppp)
plot(randomppp)

#gerando densidade
densshop <- density.ppp(shopppp, sigma=min(bw.scott(shopppp)))
plot(densshop)
densshop = raster(densshop)
plot(densshop)
densshop@crs <- WGS
densshop <- mask(densshop, posadas2)
plot(densshop)
writeRaster(densshop, "densshop.tif", overwrite=TRUE)

densshop <- as.SpatialGridDataFrame.im(densrandom)

densrandom <- density.ppp(randomppp, sigma=min(bw.scott(randomppp)))
plot(densrandom)
densrandom = raster(densrandom)
plot(densrandom)
densrandom@crs <- WGS
densrandom <- mask(densrandom, posadas2)
plot(densrandom)
writeRaster(densrandom, "densrandom.tif", overwrite=TRUE)

densrandom <- as.SpatialGridDataFrame.im

densbuild <- density.ppp(buildpppp, sigma=min(bw.scott(buildpppp)))
plot(densbuild)
densbuild = raster(densbuild)
plot(densbuild)
densbuild@crs <- WGS
densbuild <- mask(densbuild, posadas2)
plot(densbuild)
writeRaster(densbuild, "densbuild.tif", overwrite=TRUE)

denstur <- density.ppp(tourppp, sigma=min(bw.scott(tourppp)))
plot(denstur)
denstur = raster(denstur)
plot(denstur)
denstur@crs <- WGS
denstur <- mask(denstur, posadas2)
plot(denstur)
writeRaster(denstur, "DensTur.tif", overwrite=TRUE)
?raster
#densshop <- density.ppp(shopppp, sigma=min(bw.diggle(shopppp)))
#densshop <- density.ppp(shopppp, sigma=min(bw.ppl(shopppp)))
?density

#criando splayer paraleaflet
?spLayer
library(maptools)
library(rleafmap)

Densmap <- spLayer(as.SpatialGridDataFrame.im(densshop), layer = "v", 
                          cells.alpha = seq(0.1, 0.8, length.out = 12))
Densmap2 <- spLayer(as.SpatialGridDataFrame.im(densbuild), layer = "v", 
                   cells.alpha = seq(0.1, 0.8, length.out = 12))
Densmap3 <- spLayer(as.SpatialGridDataFrame.im(denstur), layer = "v", 
                    cells.alpha = seq(0.1, 0.8, length.out = 12))

#####
#Mapa
#####
?spLayer
#Dados vetoriais
# Dimorphandra_wilsonii<-spLayer(dw_spp_latlong, fill.col='green')
?writeMap
# Base map
bm1 <- basemap("mapquest.map")
bm2 <- basemap("stamen.watercolor")
writeMap(prefix='ShopHeatMap', bm1, bm2,#basemaps
         Densmap, 
         Densmap2, #Densmap3,
         width = 1200, height = 625, 
         setView = c(mean(comercio@coords[,2]), mean(comercio@coords[,1])),
         setZoom = 14,
         interface=ui(zoom = c("topleft", "topright", 
                               "bottomleft", "bottomright", "none")[1],
          layers = c("none", "topright", "topleft", "bottomleft", "bottomright")[2],
          attrib = c("bottomright", "topleft", "topright", "bottomleft", "none")[1],
          attrib.text = "TESTE!"),
         directView = c("viewer", "browser", "disabled")[2])


library(ggplot2)
datos <-  readOGR("./shp/Radios Censo 2010/Censo2010Fixed.shp")
datos <- datos@data
head(datos)
names(datos)
#ggplot(datos, aes( x = varon, y = totalpobl)) + geom_point(stat = "identity")+
#  stat_smooth(method=lm, se=TRUE, colour="grey")

names(datos)
# GRafico 3
ggplot(datos, aes( x = totalpobl, y = hogares)) + geom_point(stat = "identity")+
  stat_smooth(method=loess, level=0.99, colour="red")

# GRafico 2
library(gcookbook) # For the data set
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity")

# Grafico 1
t <- datos[order(datos$varon, decreasing=TRUE, na.last = TRUE),]
?order
t <- t[1:15,]
ggplot(t, aes(x=varon, y=reorder(toponimo_i, varon))) + geom_point()
