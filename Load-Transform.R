library(ggmap)
library(sf)
library(spData)

register_google(key="xxx") # insert api key here

## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC.
##
## name_col: Name of a column in `states` that supplies the states'
##           names.
lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}


waste <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')
waste[36540,5:6] <- c('Canada', 'CAN')
write.csv(waste, './data/HydroWaste.csv', row.names = FALSE)


waste <- read.csv('./data/HydroWaste.csv')
head(waste)

waste$perc_cap <- waste$WASTE_DIS / waste$DESIGN_CAP * 100
waste$perc_cap[which(waste$DESIGN_CAP == 0)] <- NA


# get subset where QUAL_CAP = 1 (i.e., DESIGN_CAP is reported in m^3/day)
waste2 <- subset(waste, QUAL_CAP == 1)
waste2$over_cap <- waste2$perc_cap > 100

USwaste <- subset(waste2, CNTRY_ISO == 'USA')
head(USwaste)
USwaste$state <- lonlat_to_state(data.frame(USwaste$LON_WWTP, USwaste$LAT_WWTP))
head(USwaste)

MEwaste <- subset(USwaste, state == 'Maine')
head(MEwaste)


