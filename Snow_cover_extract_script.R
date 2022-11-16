####Packages####
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(ncdf4)

####Import data####
locations<-read.csv("Zeller-Woodpecker Visual Scanning Project-ABMI 2021_Locations_202227.csv",fileEncoding = "UTF-8")%>%
  filter(!is.na(latitude))

nc_data<-nc_open("SCF_obsMEAN4xfremonthly_1981-2016.LF.th4.0.nc")
{
  sink('SCF_obsMEAN4xfremonthly_1981-2016.txt')
  print(nc_data)
  sink()
}

####Spatial data manipulation####
#based off of https://rpubs.com/boyerag/297592
#don't ask me why I do any of this
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

scf.array <- ncvar_get(nc_data, "snc") # store the data in a 3-dimensional array
dim(scf.array) 

fillvalue <- ncatt_get(nc_data, "snc", "_FillValue")
fillvalue
nc_close(nc_data) 

scf.array[scf.array == fillvalue$value] <- NA

scf.slice <- scf.array[, , 1] 
dim(scf.slice)

r <- raster(t(scf.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r)

r_brick <- brick(scf.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_brick <- flip(t(r_brick), direction='y')

####location specific####
Snowcover_all_locations<-data.frame(latitude=0,longitude=0,month=0,snc=0)

for(i in 1:nrow(locations)){
loclong<-locations$longitude[i]
loclat<-locations$latitude[i]
site_series <- extract(r_brick, SpatialPoints(cbind(loclong,loclat)), method='simple')
years<-sort(rep(seq(from=1981,to=2016),12),decreasing = F)

loc_df <- tibble(data.frame(snc=t(site_series), month=seq(from=01,to=12),year=years))
loc_df$snc<-replace_na(loc_df$snc,0)


data<-loc_df%>%group_by(month)%>%summarize(snc=mean(snc),latitude=loclat,longitude=loclong)
Snowcover_all_locations<-rbind(Snowcover_all_locations,data)
}

Snowcover_all_locations<-Snowcover_all_locations%>%filter(month!=0)
write.csv(Snowcover_all_locations,"Snowcover_all_locations.csv")
