######################################################################################
# This is an R script to create a raster map from OCO-2 and OCO-3.                   #   
# Input files are of a NETCDF (.nc) type taken from satellite observations.          #
# Files are downloaded from                                                          #
#     https://disc.gsfc.nasa.gov/datasets/OCO2_L2_Lite_FP_10r/summary (OCO-2)        #
#     https://disc.gsfc.nasa.gov/datasets/OCO3_L2_Lite_FP_10r/summary (OCO-3)        #
# This script transforms a mosaic consisting of a number of satellite observations   #
#     in one month to a dataframe.                                                   #
# The script interpolates points into raster by applying appropriate interpolation   #
#     method for the Indonesian region.                                              #
# Refer to below comments for more detailed instructions/explanations.               #
# Created by Alberth Nahas on 2021-12-12 03:00 pm WIB.                               #
# Email: alberth.nahas@bmkg.go.id                                                    #
# Version 2.0.2 (2022-03-02)                                                         #
# Disclaimer: This is a free-to-share, free-to-use script. That said, it comes with  #
#             absolutely no warranty. User discretion is advised.                    #
######################################################################################


### CLEAR WORKSPACE ###
rm(list = ls())
gc()
start.clock <- Sys.time()

### INCLUDE LIBRARIES ###
library(ncdf4)
library(sf)
library(tidyverse)
library(gstat)
library(readr)
library(raster)

### SET .nc FILES TO WORK WITH ###
#setwd(file.path("C:\WORKING\SnS2022\SnS2022-1","localdata"))  # adjust to the right directory <- not used at all
ffile <- file.choose() # select an .nc file for extracting name
fn01 <- basename(ffile)
fpath <- dirname(ffile)
setwd(fpath) #best practice, setwd after selecting file

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
#adjusted export directory
ncname <- file.path("C:/WORKING/SnS2022/SnS2022-1/localdata/OCO/results","mx",paste0(fvar, "_mx_", fdate,"_OCO-2.nc"))   # adjust to "OCO-3.nc"
ncname2 <- file.path("C:/WORKING/SnS2022/SnS2022-1/localdata/OCO/results","sns",paste0(fvar, "_sns_", fdate,"_OCO-2.nc"))   # adjust to "OCO-3.nc"
csvname <- file.path("C:/WORKING/SnS2022/SnS2022-1/localdata/OCO/results","csv",paste0(fvar, "_", fdate,"_OCO-2.csv"))  # adjust to "OCO-3.nc"

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
                                   co2 = co2,
                                   aco2 = co2 - median(co2)))
  # close file
  nc_close(nc)
}

### CREATE A .csv FILE TO BUILD THE RASTER FILE ###
co2df_sub <- subset(co2df, lat >= -11 & lat <= 6 & lon >= 95 & lon <= 141)
write.csv(co2df_sub, file = csvname, row.names = FALSE, quote = TRUE, na = "NA")
pts_CO2 <- read_csv(csvname,
                    col_types = cols(co2 = col_double(), aco2 = col_double(),
                                     lon = col_double(), lat = col_double())
) %>% 
  dplyr::select(lon, lat, co2, aco2)
print(pts_CO2)

### CREATE A SPATIAL FILE BASED ON xco2 ###
sf_CO2 <- st_as_sf(pts_CO2, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

### CREATE A RASTER TEMPLATE FILE ###
# Boundary box for max-min lat and lon
bbox <- c(
  "xmin" = 95,
  "ymin" = -11,
  "xmax" = 141,
  "ymax" = 6)
# Generate a grid template based on defined boundaries
# Grid cell size is given as "by" and might need to adjust
#    accordingly
grd_template <- expand.grid(
  X = seq(from = bbox["xmin"], to = bbox["xmax"], by = 0.1),
  Y = seq(from = bbox["ymin"], to = bbox["ymax"], by = 0.1))
# {raster} expects a PROJ.4 string, see https://epsg.io/4326
crs_raster_format <- "+proj=longlat +datum=WGS84 +no_defs"
# Rasterize the grid template
grd_template_raster <- grd_template %>% 
  dplyr::mutate(Z = 0) %>% 
  raster::rasterFromXYZ( 
    crs = crs_raster_format)

### INTERPOLATE POINT DATA TO RASTER TEMPLATE ###
# Build a formula to fit raster using Inverse Distance Weighted Method
fit_IDW_co2 <- gstat( 
  formula = co2 ~ 1,
  data = as(sf_CO2, "Spatial"),
  nmax = 10, nmin = 3,
  set = list(idp = 2.0)) # inverse distance power
# reset the CRS
proj4string(fit_IDW_co2$data$var1$data) = ""
proj4string(fit_IDW_co2$data$var1$data) = crs_raster_format

fit_IDW_aco2 <- gstat( 
  formula = aco2 ~ 1,
  data = as(sf_CO2, "Spatial"),
  nmax = 10, nmin = 3,
  set = list(idp = 2.0)) # inverse distance power
# again, reset the CRS
proj4string(fit_IDW_aco2$data$var1$data) = ""
proj4string(fit_IDW_aco2$data$var1$data) = crs_raster_format

# Interpolate data using the formula --> This usually took a long time
interp_IDW_co2 <- interpolate(grd_template_raster, fit_IDW_co2)
interp_IDW_aco2 <- interpolate(grd_template_raster, fit_IDW_aco2)

### CREATE A NETCDF OUTPUT FILE BASED ON THE INTERPOLATED VALUES ###
#co2_median <- cellStats(interp_IDW, median)
#co2rst <- brick(interp_IDW)
co2rst <- brick(interp_IDW_co2)
aco2rst <- brick(interp_IDW_aco2)
#co2sns <- co2rst - co2_median
# Some file metadata are created, others may be added.
writeRaster(co2rst, 
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
writeRaster(aco2rst, 
            file = ncname2, 
            overwrite = TRUE, 
            format = "CDF", # A netcdf format
            varname = "co2_diff", 
            varunit = "ppm", 
            longname = "Difference of CO2 mixing ratio from median",
            xname = "longitude",
            yname = "latitude", 
            zname = "time",
            zunit = "day") # Timestep and time origin are not specified

### PRINT ELAPSED TIME ###
stop.clock <- Sys.time()
how.many <- round(as.numeric(difftime(stop.clock, start.clock, units = "mins")), 2)
time.spent <- paste("Work has been completed in", how.many,"minutes")
print(time.spent)


### END OF LINES ###
