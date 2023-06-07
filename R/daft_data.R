
library(ggmap)
library(sf)
rm(list = ls())

ire <- read.csv("Data/data_all_ireland.csv")

register_google(key = 'key')

#append ireland to address
ire$address <- paste0(ire$address,", Ireland")

#geocode the address using Google Maps API - limit of 2500 searches per day
locations_df <- mutate_geocode(ire, address)
cbd_add <- locations_df
write.csv(cbd_add, "Data/daft_geocode.csv", row.names=FALSE)


cbd_add <- read.csv("Data/daft_geocode.csv")
print(unique(cbd_add$energy_eff))

cbd_add[,"ener_a"] <- 0
cbd_add[,"ener_a"][cbd_add[,"energy_eff"] %in% c("a1","a2","a3")] <- 1

cbd_add[,"ener_b"] <- 0
cbd_add[,"ener_b"][cbd_add[,"energy_eff"] %in% c("b1","b2","b3")] <- 1

cbd_add[,"ener_a_b"] <- 0
cbd_add[,"ener_a_b"][cbd_add[,"energy_eff"] %in% c("a1","a2","a3","b1","b2","b3")] <- 1

cbd_add[,"ener_c"] <- 0
cbd_add[,"ener_c"][cbd_add[,"energy_eff"] %in% c("c1","c2","c3")] <- 1

cbd_add[,"ener_d_e"] <- 0
cbd_add[,"ener_d_e"][cbd_add[,"energy_eff"] %in% c("d1","d2","e1","e2")] <- 1

cbd_add[,"ener_f_g"] <- 0
cbd_add[,"ener_f_g"][cbd_add[,"energy_eff"] %in% c("/g","/f")] <- 1

cbd_add[,"apart"] <- 0
cbd_add[,"apart"][cbd_add[,"prop_type"] %in% c("apartment","duplex")] <- 1

cbd_add[,"detached"] <- 0
cbd_add[,"detached"][cbd_add[,"prop_type"] %in% c("detached")] <- 1

cbd_add[,"semi_d"] <- 0
cbd_add[,"semi_d"][cbd_add[,"prop_type"] %in% c("semi-d")] <- 1

cbd_add[,"bunga"] <- 0
cbd_add[,"bunga"][cbd_add[,"prop_type"] %in% c("bundaglow")] <- 1

cbd_add[,"terrace"] <- 0
cbd_add[,"terrace"][cbd_add[,"prop_type"] %in% c("terrace","townhouse")] <- 1

cbd_add[,"end_house"] <- 0
cbd_add[,"end_house"][cbd_add[,"prop_type"] %in% c("end of terrace")] <- 1

cbd_add[,"scheme"] <- 0
cbd_add[,"scheme"][cbd_add[,"prop_type"] %in% c("semi-d","end of terrace","terrace","townhouse")] <- 1

cbd_add[,"single"] <- 0
cbd_add[,"single"][cbd_add[,"prop_type"] %in% c("bundaglow","detached")] <- 1

cbd_add <- cbd_add[c("price","size","no_beds","no_baths","ener_a", "ener_b",
                     "ener_c", "ener_d_e","ener_f_g", "apart", "detached",
                     "semi_d", "bunga", "terrace", "end_house","lon", "lat",
                     "url_link","scheme","single","ener_a_b")]
#reproject
cbd_add <- subset(cbd_add, !is.na(lat))
cbd_add$ppsm <- cbd_add$price/cbd_add$size
#drop top and bottom 5%
quan_bot <- quantile(cbd_add$ppsm, probs = c(0.05))
quan_top <- quantile(cbd_add$ppsm, probs = c(0.95))
cbd_add <- subset(cbd_add, ppsm >= quan_bot)
cbd_add <- subset(cbd_add, ppsm <= quan_top)

#geocode
ire_sf <- st_as_sf(cbd_add, coords = c("lon", "lat"), crs = 4326)
ire_sf <- st_transform(ire_sf, 2157)
st_write(ire_sf,"Data/ire.prop.gpkg",append=FALSE)

#load county and create grid
cty <- st_read("Data/county.gpkg")
cty <- st_transform(cty, 2157)
cty <- st_buffer(cty,5000)
cty[,"merge_1"] <- 1
cty <- aggregate(cty["merge_1"], by = list(cty$merge_1), FUN = function(x) x[1])
ctygrid <- st_make_grid(cty, cellsize = c(5000, 5000), what = "polygons")
ctygrid <- st_intersection(ctygrid, cty)
#plot(ctygrid)

#convert to sp
grid.sp <- as_Spatial(ctygrid)
house.sp <- as_Spatial(ire_sf)

library(GWmodel)

# Determine optimal bandwidth based on AIC
DM <- gw.dist(dp.locat=coordinates(house.sp), rp.locat=coordinates(house.sp))
bw.opt <- bw.gwr(price~size, data=house.sp, kernel = "gaussian", dMat = DM,adaptive=TRUE)

# Run GWR with adaptive bandwidth
# Compute new distance matrix
DM_5km <- gw.dist(dp.locat=coordinates(house.sp), rp.locat=coordinates(grid.sp))

# Run GWR with the optimal bandwidth and the new grid
gwr_res_5km <- gwr.basic(price~size, data=house.sp, regression.points=grid.sp, 
                         bw=bw.opt, adaptive = TRUE, dMat=DM_5km, kernel='gaussian')

# Join the gwr results back to the grid
grid.sp$size_coef <- gwr_res_5km$SDF$size


spplot(grid.sp, "size_coef")

#scsi - real cost of housing delivery
#2650 per m2 - excludes cost of land

grid.sp$viability_ratio <- grid.sp$size_coef/2650
spplot(grid.sp, "viability_ratio")



#load shapefile and county boundaries
eds <- st_read("Data/ED_16/Electoral_Divisions__CSO_Generalised_20M.shp")
eds <- st_transform(eds,2157)
names(eds)[names(eds) == "GUID_"] <- "GUID"
#all counties includes northern ireland
all_cty <- st_read("Data/all_counties/all_counties_ITM95.shp")
all_cty <- st_transform(all_cty,2157)


#convert to sp
grid.sp <- as_Spatial(eds)
house.sp <- as_Spatial(ire_sf)

# Run GWR with adaptive bandwidth
# Compute new distance matrix
DM_ed <- gw.dist(dp.locat=coordinates(house.sp), rp.locat=coordinates(grid.sp))

# Run GWR with the optimal bandwidth and the new grid
gwr_res_ed <- gwr.basic(price~size, data=house.sp, regression.points=grid.sp, 
                         bw=bw.opt, adaptive = TRUE, dMat=DM_ed, kernel='gaussian')

# Join the gwr results back to the grid
grid.sp$size_coef <- gwr_res_ed$SDF$size
spplot(grid.sp, "size_coef")

