# ran before: gdal_translate -of JPEG -scale -co worldfile=yes rgb_autoexposure_calibration/odm_orthophoto.tif rgb_autoexposure_calibration/odm_orthophoto.jpg

library(devtools)
install("../../scripts/rfunctions")
library(rfunctions)
library(tiff)
library(raster)

#load crop coordinates in utm from example file
crop_coords <- read.csv("../../ground_control_panels/control_panel_positions/results/control_panel_x_y.csv", header = TRUE)

#load RGB photo

orig_photos_directories <- c("rgb_orthophoto",
                             "725nm_orthophoto",
                             "800nm_orthophoto")

cropped_photos_directories <- c("rgb_autoexposure_calibration_control_panels",
                                "725nm_autoexposure_calibration_control_panels",
                                "800nm_autoexposure_calibration_control_panels")

for (i in 1:3){
  
  cat(paste0("crop rois for ", orig_photos_directories[i], "\n"))
  
  orig_photo <- readTIFF(paste0(orig_photos_directories[i], "/odm_orthophoto.tif"), convert = TRUE)
  
  #load worldfile from stitched photo
  world_file <- paste0(paste0(orig_photos_directories[i], "/odm_orthophoto.wld"))
  
  #calculate x and y positions in new photo
  crop_coords$nw_x_new <- utm_to_x(world_file = world_file, utm_x = crop_coords$nw_x_utm)
  crop_coords$nw_y_new <- utm_to_y(world_file = world_file, utm_y = crop_coords$nw_y_utm)
  crop_coords$se_x_new <- utm_to_x(world_file = world_file, utm_x = crop_coords$se_x_utm)
  crop_coords$se_y_new <- utm_to_y(world_file = world_file, utm_y = crop_coords$se_y_utm)
  
  for (ii in 1:nrow(crop_coords)){
    temp <- orig_photo[c(crop_coords$nw_y_new[ii]:crop_coords$se_y_new[ii]), c(crop_coords$nw_x_new[ii]:crop_coords$se_x_new[ii]), 1:4]
    writeTIFF(temp, paste0(cropped_photos_directories[i], "/", crop_coords$ground_control_panel[ii], ".tif"))
  }
  
}
