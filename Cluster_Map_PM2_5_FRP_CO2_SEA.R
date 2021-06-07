library(ncdf4) 
library(raster) 
library(rgdal) 
library(leaflet) 
library(stringr)
library(dplyr)

#open NetCDFfile
setwd("~/Downloads")
myncdata <- nc_open("2021-04.nc")
myncdata
#longlimits <- ncvar_get(myncdata, "longitude")
#latlimits <- ncvar_get(myncdata, "latitude")
#tm <- ncvar_get(myncdata, "time")

#set lat and long to SEA
longlimits = c(92,110)
latlimits = c(5,30)


#pm2.5 data

pm2p5 <- ncvar_get(myncdata, "pm2p5fire")
fillvalue <- ncatt_get(myncdata, "pm2p5fire", "_FillValue")
fillvalue
#nc_close(myncdata)
pm2p5[pm2p5 == fillvalue$value] <- NA
pm2p5.slice <- pm2p5[,,12]
r <- raster(t(pm2p5.slice), xmn=min(longlimits), xmx=max(longlimits), ymn=min(latlimits), ymx=max(latlimits), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
values(r)[values(r) <= 0] = NA


DF_pm2p5_LAO.slice<- as.data.frame(r, xy = TRUE)

names(DF_pm2p5_LAO.slice)[3]="PM25"

names(DF_pm2p5_LAO.slice)[1]="LON"
names(DF_pm2p5_LAO.slice)[2]="LAT"

onlyPM <- select(filter(DF_pm2p5_LAO.slice, PM25 > 0), c(LAT,LON,PM25))

#color_pal <- colorNumeric(c("#B01C39", "#B01C39", "#4B1D3F"), values(onlyPM$PM25), na.color = "transperent")
color_pal_pm2p5 <- colorFactor("Reds", domain = onlyPM$PM25)
popup_pm2p5 <- paste0("<b>","PM2.5 Level : ","</b>",
                "<b>",onlyPM$PM25,"</b><br>",
                "<b>At : ",onlyPM$LAT, " x ", onlyPM$LON,"</b>")

#co2fire data

co2fire <- ncvar_get(myncdata, "co2fire")
fillvalue <- ncatt_get(myncdata, "co2fire", "_FillValue")
fillvalue
#nc_close(myncdata)
co2fire[co2fire == fillvalue$value] <- NA
co2fire.slice <- co2fire[,,12]
r <- raster(t(pm2p5.slice), xmn=min(longlimits), xmx=max(longlimits), ymn=min(latlimits), ymx=max(latlimits), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
values(r)[values(r) <= 0] = NA


DF_co2fire.slice<- as.data.frame(r, xy = TRUE)

names(DF_co2fire.slice)[3]="CO2"

names(DF_co2fire.slice)[1]="LON"
names(DF_co2fire.slice)[2]="LAT"

onlyCO2 <- select(filter(DF_co2fire.slice, CO2 > 0), c(LAT,LON,CO2))

#color_pal <- colorNumeric(c("#B01C39", "#B01C39", "#4B1D3F"), values(onlyPM$PM25), na.color = "transperent")
color_pal_co2fire <- colorFactor("Blue", domain = onlyCO2$CO2)
popup_co2fire <- paste0("<b>","CO2 Level : ","</b>",
                "<b>",onlyCO2$CO2,"</b><br>",
                "<b>At : ",onlyCO2$LAT, " x ", onlyCO2$LON,"</b>")

#frp data

frpfire <- ncvar_get(myncdata, "frpfire")

fillvalue <- ncatt_get(myncdata, "frpfire", "_FillValue")
fillvalue
#nc_close(myncdata)
frpfire[frpfire == fillvalue$value] <- NA
frpfire.slice <- frpfire[,,12]
r <- raster(t(frpfire.slice), xmn=min(longlimits), xmx=max(longlimits), ymn=min(latlimits), ymx=max(latlimits), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
values(r)[values(r) <= 0] = NA


DF_frpfire.slice<- as.data.frame(r, xy = TRUE)

names(DF_frpfire.slice)[3]="FRP"

names(DF_frpfire.slice)[1]="LON"
names(DF_frpfire.slice)[2]="LAT"

onlyFRP <- select(filter(DF_frpfire.slice, FRP > 0), c(LAT,LON,FRP))

#color_pal <- colorNumeric(c("#B01C39", "#B01C39", "#4B1D3F"), values(onlyPM$PM25), na.color = "transperent")
color_pal_frpfire <- colorFactor("Greem", domain = onlyFRP$FRP)
popup_frpfire <- paste0("<b>","FRP Level : ","</b>",
                        "<b>",onlyFRP$FRP,"</b><br>",
                        "<b>At : ",onlyFRP$LAT, " x ", onlyFRP$LON,"</b>")


leaflet::leaflet() %>% addTiles() %>%
    #leaflet::addRasterImage(pm2p5_LAO) %>%
    leaflet::addCircleMarkers(lat = ~LAT, lng = ~LON, 
                               clusterOptions = markerClusterOptions(),
                               color = color_pal_co2fire(onlyCO2$CO2),
                               popup = ~popup_co2fire,
                               group = "CO2",
                                data = onlyCO2 ) %>%
    leaflet::addCircleMarkers(lat = ~LAT, lng =  ~LON, 
                              clusterOptions = markerClusterOptions(),
                              color = color_pal_pm2p5(onlyPM$PM25),
                              popup = ~popup_pm2p5,
                              group = "PM2.5",
                              data = onlyPM ) %>%
    leaflet::addCircleMarkers(lat = ~LAT, lng =  ~LON, 
                              clusterOptions = markerClusterOptions(),
                              color = color_pal_frpfire(onlyFRP$FRP),
                              popup = ~popup_frpfire,
                              group = "FRP",
                              data = onlyFRP ) %>%
    leaflet::addLayersControl(
      overlayGroups = c("PM2.5", "CO2", "FRP"),  # add these layers
      options = layersControlOptions(collapsed = FALSE)  # expand on hover?
    ) %>% 
    hideGroup(c("CO2", "FRP"))
    
# %>%
   # leaflet::addLegend(
      #  pal = color_pal(onlyPM$PM25),
     #   values = onlyPM$PM25,
     #   opacity = 1,
     #   title = "PM 2.5 Concentration ",
     #   position = "bottomright" )
    
    
dev.off()


