readcleanrawdata(rawpath = "ebd_IN_relApr-2020.txt", sensitivepath = "ebd_relApr-2020_sensitive.txt")

createextramap = function(dist = "Salem", path1="India",name1="India_2011",path2="India States",
                      name2="IndiaStates_2011",path3="India Districts",name3="IndiaDistricts_2011")
{
  require(tidyverse)
  require(ggfortify)
  require(raster)
  require(rgdal)
  require(sp)
  require(sf)
  require(rgeos)
  
  # reading maps
  
  assign("indiamap",readOGR(path1,name1),.GlobalEnv)
  assign("statemap",readOGR(path2,name2),.GlobalEnv)
  assign("districtmap",readOGR(path3,name3),.GlobalEnv)

  gs1 = 6.6
  
  districts = dist
  
  filterstatemap = fortify(districtmap[districtmap@data$DISTRICT %in% districts,], region = c("DISTRICT"))
  coordinates(filterstatemap) = ~long + lat
  
  bb = bbox(filterstatemap) # creates a box with extents from map
  cs = c(gs1*1000/111111,gs1*1000/111111)  # cell size gs1 km x gs1 km
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd) # create required grids
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd))) # create spatial grid data frame
  finalmap = as(sp_grd, "SpatialPolygonsDataFrame") # SGDF to SPDF
  
  diff = gDifference(finalmap,districtmap[districtmap@data$DISTRICT %in% districts,])
  fmap = finalmap - diff
  
  save(indiamap,statemap,districtmap,fmap,file = "maps.RData")
  
}

createextramap(dist = "Salem")
addmapvars()


require(mapview)

load("data.RData")
mappath = "maps.RData"
load(mappath)

states = c("Tamil Nadu")
data1 = data %>% filter(ST_NM %in% states)

finalmap = fmap

temp = data1 %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,finalmap)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data1 = left_join(temp,data1)
names(data1)[1] = "gridg"

temp = data1 %>% 
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(gridg) %>%
  summarize(speciesrecorded = n_distinct(COMMON.NAME))

temp1 = data1 %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(gridg) %>%
  summarize(completelists = n_distinct(group.id))

temp2 = data1 %>% 
  group_by(gridg) %>%
  summarize(observers = n_distinct(OBSERVER.ID))

temp3 = data1 %>% 
  group_by(gridg) %>%
  summarize(locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)

temp$gridg = as.character(temp$gridg)
names(temp)[1] = "id"
temp$completelists[is.na(temp$completelists)] = 0

effortmap = merge(finalmap,temp, by =  "id")


proj4string(finalmap) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(effortmap) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


writeOGR(effortmap, "Salem6.6km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "Salem6.6Dec19.html")




#############################################################
## district maps

require(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)

load("data.RData")
load("maps.RData")

data[data$ST_NM == "Andhra Pradesh" & data$DISTRICT == "Nizamabad" & !is.na(data$ST_NM),]$ST_NM = "Telangana"

districtmap$ST_NM = as.character(districtmap$ST_NM)
districtmap$ST_NM[districtmap$ST_NM == "Andhra Pradesh" & districtmap$DISTRICT == "Nizamabad" & 
                    !is.na(districtmap$ST_NM)] = "Telangana"
states = "Gujarat"

data1 = data %>% filter(ST_NM %in% states)
temp = data1 %>% 
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(DISTRICT) %>%
  summarize(speciesrecorded = n_distinct(COMMON.NAME))
temp$DISTRICT = as.character(temp$DISTRICT)

finalmap = districtmap[districtmap@data$ST_NM %in% states,]
finalmap = merge(finalmap,temp, by =  "DISTRICT")

proj4string(finalmap) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

a = mapView(finalmap, zcol = NULL, map.types = c("Esri.WorldImagery","OpenTopoMap"),
            layer.name = NULL, 
            popup = leafpop::popupTable(finalmap,c("DISTRICT"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE), 
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "GJ_Districts.html")
