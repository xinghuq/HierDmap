### plot the diversity on geographic map

######################




normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
### Data with Longitude, Latitude
ggDivgeomap=function(data,x, y,scale = "medium", returnclass = "sf",size,title="Global Genetic Diversity Map",subtitle=("Scaled relative genetic diversity of HGDP populations"),...){
###get world map data
world <- rnaturalearth::ne_countries(scale = scale, returnclass = returnclass)
# genetic world map
requireNamespace("ggplot2")
ggplot2::ggplot(data = world) +
  ggplot2::geom_sf() +ggplot2::geom_point(data = data, mapping = aes(x=x, y= y), color="red", alpha = .2, size=size)+
  ggplot2::labs( x = "Longitude", y = "Latitude") +
  ggplot2::ggtitle(title, subtitle = paste0(subtitle))
}

#ggDivgeomap(data = Dprofile,x=Longitude, y= Latitude,scale = "medium", returnclass = "sf",size=normalize(HiereHGDPq1$HierD$MDpop)*10,tittle="Human Population Genetic Diversity Map",subtitle=("Scaled relative genetic diversity of HGDP populations"))

########################
#library(sf)
#library(rgeos)
#library(ggspatial)
#library(rworldmap)




rDivgeomap=function(x, y, xlim = c(-180, 180), ylim = c(-90, 90), 
                    asp = 1, bg = "aliceblue", cols = "white", fill = T, border = "lightgrey", 
                   wrap=c(-180,180),col = grDevices::rgb(red = 0, green = 0, blue = 1, alpha = 0.3),
                    # define size as number of flights div. by 50
                    size, pch = 20,...){
# get map
worldmap <- rworldmap::getMap(resolution = "coarse")

CheckArguments=FALSE
defaultW <- getOption("warn") 
options(warn = -1)

rgeos::plot(worldmap, xlim=xlim, ylim = ylim, 
     asp = asp, bg = bg, col = cols, fill = fill, border = border, 
      wrap=wrap,...)
graphics::points(x, y,col =col,cex =  size, pch=pch,... )

}

#  rDivgeomap(Dprofile$Longitude,Dprofile$Latitude,size=normalize(Dprofile$Dq1)*10)

#########################overlay an area with shading to indicate the density of diversity.

# create map with density layer

#dDivgeomap=function(data,x=Longitude, y=Latitude,label=Population,border_colour=NA, border_fill="antiquewhite",density_alpha = I(.2),
#density_fill, density_size = 1, bins = 5, geom = "polygon",point_color="red", pointalpha = .2, pointsize=normalize(HiereHGDPq1$HierD$MDpop)*10,...){

#ggplot2::ggplot(data, (aes(x, y))) +   
#ggplot2:: borders("world", colour=border_colour, fill=border_fill)  +
#ggplot2::stat_density_2d(aes(fill = density_fill,  alpha = density_alpha,size = density_size, bins = 5, data = NULL,geom = "polygon")) +
#ggplot2::geom_point(color=point_color, alpha = pointalpha, size=pointsize) +
  # define color of density polygons
#ggplot2::scale_fill_gradient(low = "grey50", high = "grey20") +
# ggplot2:: theme(panel.background = element_rect(fill = "lightblue", colour = "lightblue"),
#panel.grid.major = element_blank(), 
#panel.grid.minor = element_blank(),
# surpress legend
#legend.position = "none",
#axis.line=element_blank(),
#axis.text.x=element_blank(),
#axis.text.y=element_blank(),
#axis.ticks=element_blank(),
#axis.title.x=element_blank(),
#axis.title.y=element_blank()) +
#ggplot2::geom_text(aes(x, y, label=label),color = "gray20", fontface = "italic", check_overlap = T, size = 2,alpha = pointsize,...)

#}

dDivgeomap=function(data,x, y,label,border_colour=NA, border_fill="antiquewhite",density_alpha = I(.2),
                    density_fill, density_size = 1, bins = 5, geom = "polygon",point_color="red", pointalpha = .2, pointsize,...){
  requireNamespace("ggplot2")
  ggplot2::ggplot(data, (ggplot2::aes(x, y))) +   
    ggplot2:: borders("world", colour=border_colour, fill=border_fill)  +
    ggplot2::stat_density_2d(data = data,ggplot2::aes(x, y, fill = density_fill,  alpha = I(.2)),
                             size = 1, bins = 5, geom = "polygon") +
    ggplot2::geom_point(color=point_color, alpha = pointalpha, size=pointsize) +
    # define color of density polygons
    ggplot2::scale_fill_gradient(low = "grey50", high = "grey20") +
    ggplot2:: theme(panel.background = element_rect(fill = "lightblue", colour = "lightblue"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    # surpress legend
                    legend.position = "none",
                    axis.line=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank()) +
    ggplot2::geom_text(ggplot2::aes(x, y, label=label),color = "gray20", fontface = "italic", check_overlap = T, size = 2,alpha = pointsize,...)
  
}



# dDivgeomap(Dprofile,x=Dprofile$Longitude, y=Dprofile$Latitude,label=Dprofile$Population,border_colour=NA, border_fill="antiquewhite",density_fill =Dprofile$Dq1 , density_alpha = I(.2),
#           density_size = 1, bins = 5, geom = "polygon",point_color="red", pointalpha = .2, pointsize=normalize(Dprofile$Dq1)*10)


##########################################
##You can also overlay maps to show densities (e.g. of airports) as shown below.

# load library
#library(ggmap)
# define box
dgDivgeomap=function(data,x , y,lon = c(-160, 160), lat = c(-60, 70),  mapsource = "osm", mapcolor = "color", maptype="satellite",
                    point_color="red", pointalpha = .2, pointsize,...){
  
gdmap <- ggmap::make_bbox(lon = lon, lat = lat)
wdiv = ggmap::get_map(location=gdmap, source = mapsource, color = mapcolor, maptype=maptype)
wdiv = ggmap::ggmap(wdiv)
requireNamespace("ggplot2")
wdiv +stat_density2d(data = data,  aes(x = x, y= y, fill = ..level..,  alpha = I(.2)), size = 1, bins = 5, geom = "polygon",...) +
  geom_point(data = data, mapping = aes(x=x, y= y), color=point_color, alpha = pointalpha, size=pointsize,...) +
  # define color of density polygons
  scale_fill_gradient(low = "grey50", high = "grey20") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "aliceblue", colour = "aliceblue"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # surpress legend
        legend.position = "none")

}

##dgDivgeomap(Dprofile,x=Dprofile$Longitude, y=Dprofile$Latitude,lon = c(-160, 160), lat = c(-60, 70),  mapsource = "osm", mapcolor = "color", maptype="satellite",
 #                    point_color="red", pointalpha = .2, pointsize=normalize(Dprofile$Dq1)*10)



