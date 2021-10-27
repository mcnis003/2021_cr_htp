#!/usr/bin/env Rscript

# use: "/usr/local/bin/Rscript ../../scripts/r/extract_plot_data_vc.R 2017 --vanilla"

# the purpose of this script is to extract Dn data from orthomosaic files with defined plots

# set working directory and args while troubleshooting
#setwd("/Users/ianmcnish/Google\ Drive\ File\ Stream/My\ Drive/projects/2017_btn/working_photos/btn_2017_06_14_flight_1_120ft")
# args <- 2017
# args <- 2018

args <- commandArgs(trailingOnly=TRUE)

cat(paste0("\n"))
cat(paste0("*****begin data extraction*****\n"))

library(tiff)
library(jpeg)
library(plyr)
library(abind)

library(devtools)
install("../../scripts/rfunctions")
library(rfunctions)

#load plot coordinate data
if(args == 2017){
  plot_coords <- read.csv("../../plot_coords/combined_coords_utm.csv",header=TRUE)
}
if(args == 2018){
  plot_coords <- read.csv("../../plot_coords/2018_plot_coords/results/2018_plot_coords_x_y.csv",header=TRUE)
}

#load each photo
cat(paste0("begin loading photos\n"))
mask_photo <- readTIFF("final_orthomosaic_data/orthophoto_classified.tif", convert = TRUE)
#x655nm_photo <- readTIFF("final_orthomosaic_data/655nm_orthophoto_cropped.tif", convert = TRUE)[,,1]
x725nm_photo <- readTIFF("final_orthomosaic_data/725nm_orthophoto_cropped.tif", convert = TRUE)[,,1]
x800nm_photo <- readTIFF("final_orthomosaic_data/800nm_orthophoto_cropped.tif", convert = TRUE)[,,1]
rgb_photo <- readTIFF("final_orthomosaic_data/rgb_orthophoto_cropped.tif", convert = TRUE)[,,c(1:3)]
cat(paste0("finish loading photos\n"))

#find the minimum dimensions of the photos, usually 1 row or column will need to be cropped from the largest photo(s)
min_rows <- min(c(dim(mask_photo)[1],
                  dim(x725nm_photo)[1],
                  dim(x800nm_photo)[1]))

min_columns <- min(c(dim(mask_photo)[2],
                     dim(x725nm_photo)[2],
                     dim(x800nm_photo)[2]))

#produce a combined array with data from all sensors and the mask photo
combined_array <- abind(mask_photo[c(1:min_rows), c(1:min_columns)],
                        x725nm_photo[c(1:min_rows), c(1:min_columns)],
                        x800nm_photo[c(1:min_rows), c(1:min_columns)],
                        as.matrix(rgb_photo[c(1:min_rows), c(1:min_columns), 1]),
                        as.matrix(rgb_photo[c(1:min_rows), c(1:min_columns), 2]),
                        as.matrix(rgb_photo[c(1:min_rows), c(1:min_columns), 3]),
                        along = 3)

#calculate the x and y bounds of each plot in this set of photos
mask_world_file <- "final_orthomosaic_data/rgb_orthophoto_cropped.wld"

plot_coords$new_nw_x <- utm_to_x(world_file = mask_world_file, utm_x = plot_coords$nw_x_utm)
plot_coords$new_nw_y <- utm_to_y(world_file = mask_world_file, utm_y = plot_coords$nw_y_utm)
plot_coords$new_se_x <- utm_to_x(world_file = mask_world_file, utm_x = plot_coords$se_x_utm)
plot_coords$new_se_y <- utm_to_y(world_file = mask_world_file, utm_y = plot_coords$se_y_utm)

#loop through plots, crop plot, and calculate plot statistics
cat(paste0("begin extraction plot data\n"))
for (i in 1:nrow(plot_coords)){
  
  photo_subset <- combined_array[c(plot_coords$new_nw_y[i]:plot_coords$new_se_y[i]),
                                 c(plot_coords$new_nw_x[i]:plot_coords$new_se_x[i]),]
  
  plot_coords$leaf_pixels[i] <- as.numeric(sum(photo_subset[,,1]))
  #plot_coords$mean_655nm[i] <- mean(photo_subset[,,2])
  plot_coords$mean_725nm[i] <- mean(photo_subset[,,2])
  plot_coords$mean_800nm[i] <- mean(photo_subset[,,3])
  plot_coords$mean_red[i] <- mean(photo_subset[,,4])
  plot_coords$mean_green[i] <- mean(photo_subset[,,5])
  plot_coords$mean_blue[i] <- mean(photo_subset[,,6])
  
}
cat(paste0("finish extraction plot data\n"))

#add the working directory to the results file, this field contains the date of the flight
plot_coords$flight <- getwd()
write.csv(plot_coords, "final_orthomosaic_data/flight_results.csv", row.names=FALSE)

cat(paste0("*****finish data extraction*****\n"))
cat(paste0("\n"))
