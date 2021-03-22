#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example 2.2:Visualization - 
#'### Part III: maps
#'
suppressPackageStartupMessages({
library(raster)
library(httr)
library(sf)
library(dplyr)
library(fasterize)
library(ggplot2)
library(rasterVis)
library(ggspatial)
library(sp)
library(sf)
library(raster)
library(RColorBrewer)
library(geobuffer) }) # geobuffer_pts
#'
#' functions to import maps from the atlas 
get_X_Y_coordinates <- function(x) {
  sftype <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  if(sftype == "POINT") {
    xy <- as.data.frame(sf::st_coordinates(x))
    dplyr::bind_cols(x, xy)
  } else {
    x
  }
}

sf_fisbroker <- function(url) {
  typenames <- basename(url)
  url <- httr::parse_url(url)
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "GetFeature",
                    srsName = "EPSG:25833",
                    TYPENAMES = typenames)
  request <- httr::build_url(url)
  print(request)
  out <- sf::read_sf(request)
  out <- sf::st_transform(out, 4326)
  out <- get_X_Y_coordinates(out)
  return(out)
}

export_format <- c(
  "geojson", 
  "sqlite"
)

sf_save <- function(z, fname) {
  ifelse(!dir.exists(fname), dir.create(fname), "Folder exists already")
  ff <- paste(file.path(fname, fname), export_format, sep = ".")
  purrr::walk(ff, ~{ sf::st_write(z, .x, delete_dsn = TRUE)})
  saveRDS(z, paste0(file.path(fname, fname), ".rds"))
  
}
#'
#' funtion to resample
resample_to_footprint = function(r, footprint_rast) {
  r_new = raster::crop(x=r, y=extent(footprint_rast))  # first crop 
  r_new = raster::projectRaster(r_new, footprint_rast) # reproject
  return(r_new)
}

#'
#'  import the Imprevious map - ekd102
###################################################################
# 01.02 Impervious Soil Coverage (Sealing of Soil Surface) (Edition 2017)
###################################################################
#	The real use of the built-up areas, the existing green and open space,
# the urban structure, the sealing and the types of surface. 
# Spatial reference block / partial block map ISU5 (Information System
# City and Environment) as of December 31, 2015.
####################################################################
#'
impervious_map <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/sach_nutz2015_nutzsa")
#'
impervious_map <- impervious_map %>%
  dplyr::mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
  dplyr::select(gml_id, RAUMID, everything()) %>%
  dplyr::arrange(RAUMID)
#'
dplyr::glimpse(impervious_map)
#'
#' sf_save(impervious_map, "impervious_map")
#'
summary(impervious_map)
str(filter(impervious_map, FLALLE==160724)) #key TUCC
#$ VG        : num 68.9 = Sealing, track ballast is considered sealed [% of area size]
#$ VG_0      : num 68.9 = Sealing, track ballast is considered unsealed [% of area size]
#$ PROBAU    : num 47.5 = Built-up sealed area [% of area size]
#$ PROVGNEU  : num 21.4 = Sealed area undeveloped, track ballast is considered sealed [% of area size]
#$ PROVGNEU_0: num 21.4 = Unbuilt sealed area, track ballast is considered unsealed [% of area size]
#$ KL1       : int 15   = Surface type 1 [% of undeveloped sealed area]
#$ KL2       : int 70   = Surface type 2 [% of undeveloped sealed area]
#$ KL3       : int 12   = Surface type 3 [% of undeveloped sealed area]
#$ KL4       : int 3    = Surface type 4 [% of undeveloped sealed area]
#'
impervious_map <- st_transform(impervious_map, crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
#'
#' create an empty raster of 10m resolution
map_raster <- raster(xmn=370000,xmx=415741,ymn=5799519,ymx=5837199, 
               res=10,crs=crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
#'
raster_imprevious <- raster::stack(
  fasterize(impervious_map, map_raster, field="VG",fun="max"),
  fasterize(impervious_map, map_raster, field="VG",fun="max"),
  fasterize(impervious_map, map_raster, field="PROBAU",fun="max"),
  fasterize(impervious_map, map_raster, field="PROVGNEU_0",fun="max"))
#'
raster_imprevious[[2]]@data@values[which(is.na(raster_imprevious[[2]]@data@values))] <- 100
#'
names(raster_imprevious) <- c("Impervious","Impervious_with_str",
                               "Built_up_area","Unbuilt")
#'
plot(raster_imprevious[[1]], col=terrain.colors(10))
#'
imprevious_zoom <- crop(raster_imprevious, extent(381673,388803,5811189,5821467))
#'
#'
#' EC Tower and DWD stations points 
points <- data.frame(
  x = c(385566.5, 384597.4, 386525.1,385368.3),
  y = c(5813229, 5812858, 5819332,5825159),
  city = c("ROTH", "DWD_Dahlem", "TUCC", "DWD_Tegel"))
#'
#'
#' create a buffer around the tower
pts_buf_1500m <- geobuffer_pts(xy = data.frame(lon = 13.32785,  
                                               lat = 52.51228), 
                               dist_m = 1500, output = "sp")
#'
summary(pts_buf_1500m)
pts_buf_1500m <- spTransform(pts_buf_1500m, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
#'
plot(imprevious_zoom$Impervious,axes = F, box = F) #,horizontal=TRUE,legend.args=list(text='impervious (%)')
plot(pts_buf_1500m, add = T, lwd = .5, fg = 2)  
#'
load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/berlin_district.RData"))
st_geometry(berlin.sf)
berlin.Border <- st_transform(berlin.sf, crs(raster_imprevious))
#'
#' Vegetation Height Berlin
ggplot() +
  layer_spatial(raster_imprevious$Unbuilt) + 
  layer_spatial(berlin.sf, fill="transparent", size=1.2) + 
  #layer_spatial(pts_buf_1500m, fill="transparent", col="red", size=1.2, linetype="dashed") + 
  geom_spatial_point(aes(x=points$x[c(1,3)], y=points$y[c(1,3)]), 
                     crs =crs(raster_imprevious), size=2, 
                     col=c("black","black")) +
  geom_spatial_label(aes(x=points$x[c(1,3)], y=points$y[c(1,3)],label=points$city[c(1,3)]),
                     crs = crs(raster_imprevious),
                     fill="transparent", col=c("black","black"), 
                     vjust=c(-0.5,-0.5),
                     hjust=c(0.5, .05), label.size= 0, size=6) +
  guides(linetype = guide_legend(title=NULL, order = 2), 
         color = guide_legend(order=1)) +
  scale_fill_gradientn(breaks=seq(0,30,5),limits=c(0,30),
                       colors=brewer.pal('Greens', n=9), name="",na.value = NA,
                       guide =guide_colorbar(direction = "horizontal",
                                             label.position = "bottom",
                                             title.vjust=9, label.vjust=9,
                                             frame.colour = "black",
                                             frame.linewidth = 0.5,
                                             frame.linetype = 1,
                                             title.position = "left",
                                             barwidth=30,barheight=1.2,nbin=30,
                                             label.theme=element_text(angle=0,size=20))) +
  annotation_scale(location = "bl", height = unit(0.4, "cm"),
                   pad_x=unit(1.75,"cm"), pad_y=unit(2.5,"cm"),
                   text_pad = unit(0.25, "cm"),
                   text_cex = 1.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(2, "cm"),width = unit(2, "cm"),
                         pad_x=unit(3.5,"cm"), pad_y=unit(2.5,"cm")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.spacing.y = unit(-0.5, "lines"),
        legend.box.spacing = unit(-1.5, "lines"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect("white"))
#'
#' impervious map
ggplot() +
  layer_spatial(imprevious_zoom$Impervious_with_str) + 
  layer_spatial(pts_buf_1500m, fill="transparent", col="red", linetype="dashed", size=1.2) + 
  geom_spatial_point(aes(x = points[3,1], y=points[3,2]), 
                     crs =crs(imprevious_zoom), size = 2, 
                     col=c("red")) +
  geom_spatial_label(aes(x=points[3,1], y=points[3,2],
                         label=points[3,3]),
                     crs = crs(imprevious_zoom),
                     fill="transparent", col=c("red"), 
                     vjust=c(-0.5),
                     hjust=c(0.3), label.size= 0, size=6) +
  guides(linetype = guide_legend(title=NULL, order = 2), 
         color = guide_legend(order=1)) +
  scale_fill_gradientn(breaks=seq(0,100,10),limits=c(0,100),
                       colors=terrain.colors(10), name="", na.value = 0,
                       guide =guide_colorbar(direction = "horizontal",
                                             label.position = "bottom",
                                             label.vjust=-2,
                                             frame.colour = "black",
                                             frame.linewidth = 0.5,
                                             frame.linetype = 1,
                                             #title.position = "left",
                                             barwidth=21,barheight=1.2,nbin=10,
                                             label.theme=element_text(angle=0,size=18))) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.spacing.y = unit(-0.4, "lines"),
        legend.box.spacing = unit(-1.5, "lines"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect("white"))
