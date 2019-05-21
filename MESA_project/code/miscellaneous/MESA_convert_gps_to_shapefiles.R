# ---------------------------- #
#    Convert GPS Coordinates   #
#      to an ESRI Shapefile    #
#         MESA Project         #
#         May 15, 2019         #
#           K. Sumner          #
# ---------------------------- #


# load libraries
library(tidyverse)
library(sp)
library(GISTools)
library(rgdal)


# read in the data set using the "import dataset" button on the top right panel
# rename the data set to make it easier to work with
spatial_data = read_csv("Desktop/mesa_satscan/may_july_cc_for_kelsey_for_satscan.csv")
setwd("Desktop")

# remove columns that aren't needed
colnames(spatial_data)
spatial_data = spatial_data[,c("labid","numerator","denominator","gps_coordinates_latitude","gps_coordinates_longitude")]


# first convert the data frame to a spatial points data frame
class(spatial_data)
head(spatial_data)
coordinates(spatial_data) <- c("gps_coordinates_longitude","gps_coordinates_latitude") # this pulls out the lat/long coordinates and creates a spatial points data frame
class(spatial_data) 

# now export the spatial points data frame as a shapefile
# add proj4 string
proj4string(spatial_data) <- CRS("+init=epsg:4326")
writeOGR(spatial_data, dsn=".", layer="mesa_geoda", driver="ESRI Shapefile") # this is in geographical projection


# make case and control files
case_data = read_csv("Desktop/may-july cc for kelsey v2.csv")

# create a new column that is a rep of just 1
case_data$count = rep(1,nrow(case_data))

# make separate case and control data sets
case_data_only = case_data[which(case_data$Outbreak == 1),]
control_data_only = case_data[which(case_data$Outbreak == 0),]
table(case_data$Outbreak)
table(case_data_only$Outbreak)
table(control_data_only$Outbreak)

# write out the data sets
write_csv(case_data_only,"case_file.csv")
write_csv(control_data_only,"control_file.csv")

# remove the NA rows from the big case file
case_data = case_data[-which(is.na(case_data$Outbreak)),]

# export big case file as coordinates file
write_csv(case_data,"coordinates_file.csv")

