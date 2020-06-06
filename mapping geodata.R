# CREAATING A BASE WORLD MAP

#Get the world map country border points
library(maps)
library(ggplot2)
world_map <- map_data("world")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")

#Creat a base plot with gpplot2 and add borders
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")+borders(
    database = "world",
    regions = ".", # region
    fill = NA, # fil color
    colour = "grey50", # border color
    xlim = NULL,
    ylim = NULL,
    xlim = c(-130, -60), ylim = c(20, 50) # specify limits of long and lat to get borders
  )

#Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")

base_world_messy


#Strip the map down so it looks super clean (and beautiful!)
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

base_world

# plotting data points on maps with R

cities #DATAFRAME

#Add simple data points to map
map_data <- 
  base_world +
  geom_point(data=cities, 
             aes(x=Longitude, y=Latitude), colour="Deep Pink", 
             fill="Pink",pch=21, size=5, alpha=I(0.7))

map_data

# Vary size of data points

#Add data points to map with value affecting size
map_data_sized <- 
  base_world +
  geom_point(data=cities, 
             aes(x=Longitude, y=Latitude, size=Value), colour="Deep Pink", 
             fill="Pink",pch=21, alpha=I(0.7)) 

map_data_sized

# Vary color of data points

#Add data points to map with value affecting colour
map_data_coloured <- 
  base_world +
  geom_point(data=cities, 
             aes(x=Longitude, y=Latitude, colour=Value), size=5, alpha=I(0.7))

map_data_coloured




------------------------------------------------------------------------------------------------

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)



ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen")+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))+
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) # gives a specific location eg country














