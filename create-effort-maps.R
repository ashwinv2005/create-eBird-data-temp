require(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)

load("data.RData")
load("maps.RData")
load("clips.RData")

################################################################

finalmap = g2clip
data1 = data %>% filter(year %in% c(2014:2018))

temp = data1 %>% 
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(g2clip) %>%
  summarize(speciesrecorded = n_distinct(COMMON.NAME))

temp1 = data1 %>% 
  #filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(g2clip) %>%
  summarize(lists = n_distinct(group.id))

temp2 = data1 %>% 
  group_by(g2clip) %>%
  summarize(observers = n_distinct(OBSERVER.ID))

temp3 = data1 %>% 
  group_by(g2clip) %>%
  summarize(locations = n_distinct(LOCALITY.ID))

temp4 = data1 %>% 
  group_by(g2clip) %>%
  filter(COMMON.NAME == "Indian Courser") %>%
  summarize(courser = n_distinct(group.id))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)
temp = left_join(temp,temp4)

temp$g2clip = as.character(temp$g2clip)
names(temp)[1] = "id"
temp$lists[is.na(temp$lists)] = 0

effortmap = merge(finalmap,temp, by =  "id")


proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"


#writeOGR(effortmap, "India10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("lists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Total Checklists"), 
            popup = leafpop::popupTable(effortmap,c("lists","speciesrecorded",
                                                    "observers","locations","courser"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$lists)))), alpha.regions = 0.5)
a = mapView(effortmap, zcol = c("courser"), map.types = c("OpenTopoMap"), 
            layer.name = c("Checklists with Indian Courser"), 
            popup = leafpop::popupTable(effortmap,c("lists","speciesrecorded",
                                                    "observers","locations","courser"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,1,max(na.omit(effortmap$courser)))), alpha.regions = 0.5)
mapshot(a, "India50km.html")



################################################################

states = c("Gujarat","Daman & Diu","Dadara & Nagar Havelli")
data1 = data %>% filter(ST_NM %in% states)

finalmap = rbind(GJmap,DDmap,DHmap)

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "GJ6.6Nov19.html")

################################################################

states = c("Tamil Nadu","Puducherry")
data1 = data %>% filter(ST_NM %in% states)

finalmap = rbind(TNmap,PUmap)

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "TN6.6Nov19.html")


################################################################

states = c("Arunanchal Pradesh")
data1 = data %>% filter(ST_NM %in% states)

finalmap = ARmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "AR6.6Nov19.html")


################################################################

states = c("Andhra Pradesh")
data1 = data %>% filter(ST_NM %in% states)

finalmap = ADmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "AD6.6Nov19.html")


################################################################

states = c("Jammu & Kashmir")
data1 = data %>% filter(ST_NM %in% states)

finalmap = JKmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "JK6.6Nov19.html")


################################################################

states = c("Jammu & Kashmir")
data1 = data %>% filter(ST_NM %in% states)

finalmap = LDmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "LD6.6Nov19.html")



################################################################

states = c("Assam")
data1 = data %>% filter(ST_NM %in% states)

finalmap = ASmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "AS6.6Nov19.html")


################################################################

states = c("Bihar")
data1 = data %>% filter(ST_NM %in% states)

finalmap = BHmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "BH6.6Nov19.html")



################################################################

states = c("Chandigarh")
data1 = data %>% filter(ST_NM %in% states)

finalmap = CGmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "CG6.6Nov19.html")


################################################################

states = c("Chhattisgarh")
data1 = data %>% filter(ST_NM %in% states)

finalmap = CHmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "CH6.6Nov19.html")




################################################################

states = c("NCT of Delhi")
data1 = data %>% filter(ST_NM %in% states)

finalmap = DLmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "DL6.6Nov19.html")


################################################################

states = c("Goa")
data1 = data %>% filter(ST_NM %in% states)

finalmap = GAmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "GA6.6Nov19.html")



################################################################

states = c("Haryana")
data1 = data %>% filter(ST_NM %in% states)

finalmap = HAmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "HA6.6Nov19.html")


################################################################

states = c("Himachal Pradesh")
data1 = data %>% filter(ST_NM %in% states)

finalmap = HPmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "HP6.6Nov19.html")



################################################################

states = c("Jharkhand")
data1 = data %>% filter(ST_NM %in% states)

finalmap = JHmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "JH6.6Nov19.html")


################################################################

states = c("Karnataka")
data1 = data %>% filter(ST_NM %in% states)

finalmap = KAmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "KA6.6Nov19.html")



################################################################

states = c("Kerala")
data1 = data %>% filter(ST_NM %in% states)

finalmap = KLmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "KL6.6Nov19.html")


################################################################

states = c("Lakshadweep")
data1 = data %>% filter(ST_NM %in% states)

finalmap = LKmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "LK6.6Nov19.html")



################################################################

states = c("Maharashtra")
data1 = data %>% filter(ST_NM %in% states)

finalmap = MHmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "MH6.6Nov19.html")


################################################################

states = c("Manipur")
data1 = data %>% filter(ST_NM %in% states)

finalmap = MNmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "MN6.6Nov19.html")



################################################################

states = c("Madhya Pradesh")
data1 = data %>% filter(ST_NM %in% states)

finalmap = MPmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "MP6.6Nov19.html")


################################################################

states = c("Mizoram")
data1 = data %>% filter(ST_NM %in% states)

finalmap = MZmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "MZ6.6Nov19.html")



################################################################

states = c("Nagaland")
data1 = data %>% filter(ST_NM %in% states)

finalmap = NGmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "NG6.6Nov19.html")


################################################################

states = c("Odisha")
data1 = data %>% filter(ST_NM %in% states)

finalmap = ORmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "OR6.6Nov19.html")



################################################################

states = c("Punjab")
data1 = data %>% filter(ST_NM %in% states)

finalmap = PBmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "PB6.6Nov19.html")


################################################################

states = c("Rajasthan")
data1 = data %>% filter(ST_NM %in% states)

finalmap = RJmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "RJ6.6Nov19.html")



################################################################

states = c("Sikkim")
data1 = data %>% filter(ST_NM %in% states)

finalmap = SKmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "SK6.6Nov19.html")


################################################################

states = c("Tripura")
data1 = data %>% filter(ST_NM %in% states)

finalmap = TPmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "TP6.6Nov19.html")



################################################################

states = c("Telangana")
data1 = data %>% filter(ST_NM %in% states)

finalmap = TSmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "TS6.6Nov19.html")


################################################################

states = c("Uttarakhand")
data1 = data %>% filter(ST_NM %in% states)

finalmap = UKmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "UK6.6Nov19.html")



################################################################

states = c("Uttar Pradesh")
data1 = data %>% filter(ST_NM %in% states)

finalmap = UPmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,1000,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "UP6.6Nov19.html")


################################################################

states = c("West Bengal")
data1 = data %>% filter(ST_NM %in% states)

finalmap = WBmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "WB6.6Nov19.html")


#################
################################################################

states = c("Meghalaya")
data1 = data %>% filter(ST_NM %in% states)

finalmap = MGmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "MG6.6Nov19.html")


################################################################

states = c("Andaman & Nicobar Island")
data1 = data %>% filter(ST_NM %in% states)

finalmap = ANmap

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


#writeOGR(effortmap, "TN10km.kml", layer="effort", driver="KML") 

a = mapView(effortmap, zcol = c("completelists"), map.types = c("OpenTopoMap"), 
            layer.name = c("Complete Checklists"), 
            popup = leafpop::popupTable(effortmap,c("speciesrecorded","completelists",
                                                    "observers","locations"), feature.id = FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,max(na.omit(effortmap$completelists)))), alpha.regions = 0.5)
mapshot(a, "AN6.6Nov19.html")

