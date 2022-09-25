
qmplot(LON_WWTP, LAT_WWTP, 
       data = USwaste, 
       xlim = c(-125,-60),
       ylim = c(25,50),
       size = I(1)) # size = size of dot

# qmplot(LON_WWTP, LAT_WWTP, 
#        data = USwaste, 
#        xlim = c(-80,-75),
#        ylim = c(47,49),
#        size = I(1)) # size = size of dot

test <- subset(waste, LON_WWTP > -80 & LON_WWTP < -75 & LAT_WWTP > 47 & LAT_WWTP < 49)

# location in row 36540 is in Canada: changing country/country_iso in main dataset
waste[36540,]


maine <- get_map("maine", zoom = 10)


me_map <- map_data("state", region = "maine")

ggplot(me_map) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray") +
  geom_point(data = MEwaste, aes(x = LON_WWTP, y = LAT_WWTP)) +
  geom_point(data = MEwaste, aes(x = LON_OUT, y = LAT_OUT), color = 'cyan')

me_bound <- data.frame(me_map$long, me_map$lat)
exclude <- c(-72.5,47.5 , -66,47.5, -66,43 , -72.5,43 , -72.5,47.9) %>% matrix(ncol = 2, byrow = T) %>% data.frame()
names(exclude) <- c("long", "lat")
outer_area <- rbind(data.frame(long = me_map$long, lat = me_map$lat), exclude)

me <- get_map("Maine", 
              maptype = 'terrain',
              zoom = 7)

ggmap(me, size = c(600,900)) +
  #  geom_path(data = me_map, aes(x = long, y = lat, group = group)) +
  #  geom_polygon(data = outer_area, aes(x = long, y = lat), fill = 'white')
  geom_point(data = MEwaste, aes(x = LON_WWTP, y = LAT_WWTP, color = "Treatment Plant Location", size = POP_SERVED)) +
  geom_point(data = MEwaste, aes(x = LON_OUT, y = LAT_OUT, color = "Treatment Plant Outflow Location")) +
  geom_point(data = MEwaste[which(MEwaste$over_cap),], aes(x = LON_WWTP, y = LAT_WWTP), shape = 2, size = 5, color = 'purple', stroke = 2) +
  geom_segment(data = MEwaste, aes(x = LON_WWTP, xend = LON_OUT, y = LAT_WWTP, yend = LAT_OUT), color = 'red', size = 1) +
  scale_color_manual(name = "Site",
                     breaks = c('Treatment Plant Location', 'Treatment Plant Outflow Location'),
                     values = c('Treatment Plant Location' = 'black',
                                'Treatment Plant Outflow Location' = 'cyan')) +
  guides(size = guide_legend(title = "Population Served by Plant")) + 
  labs(title = "Wastewater Treatment in Maine, USA",
          subtitle = "Data from https://figshare.com/articles/dataset/HydroWASTE_version_1_0/14847786/1") +
  annotate(geom = "text", x = -68, y = 43.5, 
           label = "Red lines connect plants with\ntheir outflow locations\nTriangles enclose plants that are\nover designed capacity (m^3/day).") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 20))

ggsave("./me-map.png")

# WASTE_DIS is reported in m^3/day
ggmap(me, size = c(600,900)) +
  #  geom_path(data = me_map, aes(x = long, y = lat, group = group)) +
  #  geom_polygon(data = outer_area, aes(x = long, y = lat), fill = 'white')
  geom_point(data = MEwaste, aes(x = LON_WWTP, y = LAT_WWTP, color = "Treatment Plant Location", size = WASTE_DIS)) +
  geom_point(data = MEwaste, aes(x = LON_OUT, y = LAT_OUT, color = "Treatment Plant Outflow Location")) +
  scale_color_manual(name = "Site",
                     breaks = c('Treatment Plant Location', 'Treatment Plant Outflow Location'),
                     values = c('Treatment Plant Location' = 'black',
                                'Treatment Plant Outflow Location' = 'cyan'))

