#Getting centroids
#Actually need to dis for all countries in the impact distribution file

#Impact distribution for MaxSev_range
imp_dist <- Impact_Distribution %>%
  dplyr::select(ISO3) %>%
  dplyr::filter(ISO3 != "NA") %>%
  pull()

#Some studies didn't provide information on lower than country level information
subdiv <- Impacts.nonDD %>% 
  filter(GID_1 != "NA") %>%
  distinct(GID_1 = GID_1, .keep_all = F) %>%
  pull() %>%
  as.character()

div <- Impacts.nonDD %>% 
  filter(GID_1 == "NA") %>%
  distinct(Country = Country, .keep_all = F) %>%
  pull() %>%
  as.character()

#Creating empty sf files
cent_sub <- lvl_1_sf %>% 
  filter(NAME_0 == "") %>%
  dplyr::select(GID_1, geometry)

cent_div <- lvl_1_sf %>% 
  filter(NAME_0 == "") %>%
  dplyr::select(NAME_0, geometry)

area_sub <- data.frame(GID_1 = c(),
                       area = c())

area_div <- data.frame(NAME_0 = c(),
                       area = c())

imp_dist_div <- data.frame(GID_0 = c(),
                           area = c())

#Calculate centroids and area and add to empty sf file
#Sub-national level information available
for(i in subdiv){
  
  loc <- lvl_1_sf %>% 
    filter(GID_1 == i) %>%
    st_centroid() %>%
    dplyr::select(GID_1, geometry)
  
  cent_sub <- rbind(cent_sub, loc)
  
}

for(i in subdiv){
  
  loc <- lvl_1_sf %>% 
    filter(GID_1 == i) %>%
    st_area() 
  
  temp <- data.frame(GID_1 = i,area = loc/1000000) #to km2
  area_sub <- rbind(area_sub, temp)
  
}

#Sub-national level information not available
for(i in div){
  
  loc <- lvl_0_sf %>% 
    filter(NAME_0 == i) %>%
    st_centroid() %>%
    dplyr::select(NAME_0, geometry)
  
  cent_div <- rbind(cent_div, loc)
  
}

for(i in div){
  
  loc <- lvl_0_sf %>% 
    filter(NAME_0 == i) %>%
    st_area() 
  
  temp <- data.frame(NAME_0 = i,area = loc/1000000) #to km2
  area_div <- rbind(area_div, temp)
  
}

for(i in imp_dist){
  
  loc <- lvl_0_sf %>% 
    dplyr::filter(GID_0 == i) %>%
    st_area() 
  
  temp <- data.frame(GID_0 = i,area = loc/1000000) #to km2
  imp_dist_div <- rbind(imp_dist_div, temp)
}

Impact_Distribution <- Impact_Distribution %>%
  left_join(imp_dist_div, by = c("ISO3" = "GID_0")) %>%
  group_by(scientificName) %>%
  distinct(ISO3 = ISO3, .keep_all = T) %>% 
  mutate(area = as.double(area)) %>%
  rename(area_km2 = "area")

#Determine approximate "vertical" distance to equator from centroid (maintaining longitude)
#Sub-national level information available
cent_sub_xy <- st_coordinates(cent_sub)
cent_sub <- cbind(cent_sub, cent_sub_xy)
eq_sub <- cent_sub %>% 
  mutate(eq_y = rep(0,length(GID_1))) %>%
  st_drop_geometry()

eq_sub_sf <- st_as_sf(eq_sub,
                      coords = c("X", "eq_y"),
                      agr = "constant",
                      crs = st_crs(lvl_0_sf),    
                      stringsAsFactors = FALSE,
                      remove = TRUE)

dis <- c()
for(i in 1:nrow(eq_sub_sf)){
  
  d <- st_distance(cent_sub[i,], eq_sub_sf[i,])
  dis <- c(dis,d/1000)
  
}

cd_sub <- cbind(cent_sub, dis, area_sub$area)

#Sub-national level information not available
cent_div_xy <- st_coordinates(cent_div)
cent_div <- cbind(cent_div, cent_div_xy)
eq_div <- cent_div %>% 
  mutate(eq_y = rep(0,length(NAME_0)),
         GID_1 = rep("NA", length(NAME_0))) %>%
  st_drop_geometry()

eq_div_sf <- st_as_sf(eq_div,
         coords = c("X", "eq_y"),
         agr = "constant",
         crs = st_crs(lvl_0_sf),    
         stringsAsFactors = FALSE,
         remove = TRUE)

dis <- c()
for(i in 1:nrow(eq_div_sf)){
  
  d <- st_distance(cent_div[i,], eq_div_sf[i,])
  dis <- c(dis,d/1000) #Get km units
  
}

cd <- cbind(cent_div, dis, area_div$area)

#Combine both datasets
impacts_temp <- Impacts.nonDD %>% 
  left_join(cd, by = c("Country" = "NAME_0")) %>%
  left_join(cd_sub, by = "GID_1")

subs <- impacts_temp %>%
  filter(is.na(X.y)) %>%
  dplyr::select(!c(X.y, Y.y, dis.y, area_sub.area, geometry.y)) %>%
  rename(decimalLongitude = "X.x") %>%
  rename(decimalLatitude = "Y.x") %>%
  rename(distance = "dis.x") %>%
  rename(area = "area_div.area") %>%
  rename(geometry = "geometry.x")
  
subs_two <- impacts_temp %>%
  filter(!is.na(X.y)) %>%
  dplyr::select(!c(X.x, Y.x, dis.x, area_div.area, geometry.x)) %>%
  rename(decimalLongitude = "X.y") %>%
  rename(decimalLatitude = "Y.y") %>%
  rename(distance = "dis.y") %>%
  rename(area = "area_sub.area") %>%
  rename(geometry = "geometry.y")

Impacts.nonDD <- subs_two %>% 
  bind_rows(subs) %>%
  st_drop_geometry()

#Removing uneccessary objects
rm(impacts_temp, subs, subs_two, loc, d, dis, div, i, subdiv, eq_div, eq_div_sf,
   eq_sub, eq_sub_sf, cent_div, cent_div_xy, cent_sub, cent_sub_xy, cd, cd_sub,
   area_div, area_sub, temp)
