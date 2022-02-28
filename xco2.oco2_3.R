######################################################################################
# This is an R script to create a raster map from OCO-2 and OCO-3.                   #   
# Input files are of a NETCDF (.nc) type taken from satellite observations.          #
# Files are downloaded from                                                          #
#     https://disc.gsfc.nasa.gov/datasets/OCO2_L2_Lite_FP_10r/summary (OCO-2)        #
#     https://disc.gsfc.nasa.gov/datasets/OCO3_L2_Lite_FP_10r/summary (OCO-3)        #
# This script transforms a mosaic consisting of a number of satellite observations   #
#     in one month to a dataframe.                                                   #
# Refer to below comments for more detailed instructions/explanations.               #
# Created by Alberth Nahas on 2021-12-12 03:00 pm WIB.                               #
# Email: alberth.nahas@bmkg.go.id                                                    #
# Version 1.0.1 (2021-12-19)                                                         #
# Disclaimer: This is a free-to-share, free-to-use script. That said, it comes with  #
#             absolutely no warranty. User discretion is advised.                    #
######################################################################################


### CLEAR WORKSPACE ###
rm(list = ls())
gc()
start.clock <- Sys.time()

### INCLUDE LIBRARIES ###
library(ncdf4)
library(ggplot2)
library(rnaturalearth) 
library(rnaturalearthdata)
library(sf)

### SET .nc FILES TO WORK WITH ###
setwd("~/Documents/Satellite/OCO-2/202109/")  # adjust to the right directory
ffile <- file.choose() 
fn01 <- basename(ffile)
fpath <- dirname(ffile)

### COLLECT .nc FILES ON A LIST ###
fn <- list.files(path = fpath, 
                 pattern = "oco2_",   # adjust to "oco3_"
                 all.files = FALSE, 
                 full.names = FALSE, 
                 recursive = FALSE)
print(paste("There are ",length(fn), " netcdf file(s) in this directory."))


### SOME NAMING ###
fvar <- substr(fn01, start = 8, stop = 10)
fdate <- substr(fn01, start = 12, stop = 15)
ncname <- paste0(fvar, "_", fdate,"_OCO-2.nc")   # adjust to "OCO-3.nc"
csvname <- paste0(fvar, "_", fdate,"_OCO-2.csv")  # adjust to "OCO-3.nc"

### CONSTRUCT A DATAFRAME FROM .nc FILES ###
co2df <- NULL
for (i in seq_along(fn)) {
  nc <- nc_open(fn[i])
  co2 <- ncvar_get(nc, "xco2")
  lat <- ncvar_get(nc, "latitude")
  lon <- ncvar_get(nc, "longitude")
  # concatenate the new data to the global data frame
  co2df <- rbind(co2df, data.frame(lat = lat, 
                                   lon = lon, 
                                   co2 = co2))
  # close file
  nc_close(nc)
}

### CREATE A .csv FILE TO BUILD THE RASTER FILE ###
write.csv(co2df, file = csvname, row.names = FALSE, quote = TRUE, na = "NA")

### CREATE A RASTER FILE FROM THE .csv FILE ###
# Generating a netcdf file by following a template.
# Resolution of the file may be changed, but make sure it will be
#    well-mapped when plotted.
# Lon-lat boundaries cover the globe.
pts <- read.table(csvname, header = TRUE, sep = ",")
coordinates(pts) <- ~lon+lat
rst01 <- raster(resolution = 0.1, # adjust as needed
                xmn = -180, # Western-most boundary
                xmx = 180, # Eastern-most boundary
                ymn = -90, # Southern-most boundary
                ymx = 90) # Northern-most boundary
# Raster file generated with max values for overlapping grid cell(s).
rst <- rasterize(pts, rst01, pts$co2, fun = max) # max is used instead of mean
##rst <- reclassify(rst, cbind(NA, 0))
proj4string(rst) <- CRS("+init=EPSG:4326") # Standard WGS 84 map projection
ncdf <- brick(rst)
# Some file metadata are created, others may be added.
writeRaster(ncdf, 
            file = ncname, 
            overwrite = TRUE, 
            format = "CDF", # A netcdf format
            varname = "co2", 
            varunit = "ppm", 
            longname = "CO2 mixing ratio",
            xname = "longitude",
            yname = "latitude", 
            zname = "time",
            zunit = "day") # Timestep and time origin are not specified

### CREATE A MAP WITH TRAJECTORY-TYPE XCO2 ###
##world <- ne_countries(scale = "medium", returnclass = "sf")
##ggplot(data = world) +
##  geom_rect(data = NULL, aes(xmin = -180, xmax= 180, ymin = -90, ymax = 90), fill = "#454565") +
##  geom_sf(fill = "#8f9f7d") +
##  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
##  geom_point(data = co2df, aes(x = lon, y = lat, colour = co2)) +
##  scale_color_gradientn( colours = c("skyblue", "blue", "red", "orange", "yellow")) +
##  labs(title = "CO2", x = "longitude", y = "latitude")


### PRINT ELAPSED TIME ###
stop.clock <- Sys.time()
how.many <- round(as.numeric(difftime(stop.clock, start.clock, units = "mins")), 2)
time.spent <- paste("Work has been completed in", how.many,"minutes")
print(time.spent)


### END OF LINE ###