#Transformando dados
library(rgdal)
library(sp)
library(raster)
library(spatstat)

comercio <- readOGR("./shp/Shop.shp")
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
plot(shopppp)
plot(buildpppp)
plot(tourppp)

#gerando densidade
densshop <- density.ppp(shopppp, sigma=min(bw.scott(shopppp)))
plot(densshop)
densbuild <- density.ppp(buildpppp, sigma=min(bw.scott(buildpppp)))
plot(densbuild)
denstur <- density.ppp(tourppp, sigma=min(bw.scott(tourppp)))
plot(denstur)

#densshop <- density.ppp(shopppp, sigma=min(bw.diggle(shopppp)))
#densshop <- density.ppp(shopppp, sigma=min(bw.ppl(shopppp)))
?density
plot(densshop)
#criando splayer paraleaflet
?spLayer
library(maptools)
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
#bm1 <- basemap("mapquest.map")
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
