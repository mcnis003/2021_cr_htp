#!/usr/bin/env Rscript

# use: "/usr/local/bin/Rscript ../../scripts/r/gcp_subset_calibrated_photos_vc.R --vanilla"

# the purpose of this script is to subset the photos near each gcp into new folders before identification of the gcps

cat(paste0("\n"))
cat(paste0("*****begin subset by gcp*****\n"))

#produce vectors for directory structure

photo_directories <- c("rgb/",
                        "655nm/",
                        "725nm/",
                        "800nm/")

calibrated_photos_directories <- c("rgb_autoexposure_calibration/",
                                   "655nm_autoexposure_calibration/",
                                   "725nm_autoexposure_calibration/",
                                   "800nm_autoexposure_calibration/")

calibrated_photo_gcp_directories <- c("rgb_autoexposure_calibration_gcps/",
                                   "655nm_autoexposure_calibration_gcps/",
                                   "725nm_autoexposure_calibration_gcps/",
                                   "800nm_autoexposure_calibration_gcps/")

#read in and calculate min and max positions for each gcp
ground_control_point_info <- read.csv("../../ground_control_points/ground_control_point_info.csv", header = TRUE)
ground_control_point_info$approximate_latitude_min <- ground_control_point_info$approximate_latitude - 0.00015
ground_control_point_info$approximate_latitude_max <- ground_control_point_info$approximate_latitude +  0.00015
ground_control_point_info$approximate_longitude_min <- ground_control_point_info$approximate_longitude -  0.00015
ground_control_point_info$approximate_longitude_max <- ground_control_point_info$approximate_longitude +  0.00015

ground_control_point_directories <- ground_control_point_info$ground_control_point_name

for (i in 1:length(photo_directories)){
  
  photo_list <- list.files(path = calibrated_photos_directories[i], pattern = "*.tif", full.names = TRUE)
  photo_info <- read.csv(paste0(photo_directories[i], "pix4d.csv"), header = TRUE)
  
  cat(paste0("begin moving ", photo_directories[i], "\n"))
  
    for (ii in 1:length(ground_control_point_directories)){
      
      ground_control_point_info_temp <- ground_control_point_info[ground_control_point_info$ground_control_point_name == ground_control_point_directories[ii],]
      photo_info_temp <- photo_info[photo_info$Lat..decimal.degrees. < ground_control_point_info_temp$approximate_latitude_max[1],]
      photo_info_temp <- photo_info_temp[photo_info_temp$Lat..decimal.degrees. > ground_control_point_info_temp$approximate_latitude_min[1],]
      photo_info_temp <- photo_info_temp[photo_info_temp$Lon..decimal.degrees. < ground_control_point_info_temp$approximate_longitude_max[1],]
      photo_info_temp <- photo_info_temp[photo_info_temp$Lon..decimal.degrees. > ground_control_point_info_temp$approximate_longitude_min[1],]
      
      cat(paste0("begin moving ", ground_control_point_directories[ii], "\n"))
      
      for (iii in 1:length(photo_info_temp$File.Name)){
        
        from_file <- sub(".jpg", ".tif", paste0(calibrated_photos_directories[i], "calibrated_", photo_info_temp$File.Name[iii]))
        to_file <- sub(".jpg", ".tif", paste0(calibrated_photo_gcp_directories[i], ground_control_point_directories[ii], "/", "calibrated_", photo_info_temp$File.Name[iii]))
        
        file.copy(from = from_file,
                  to = to_file)
        
      }
      
      cat(paste0("finish moving ", ground_control_point_directories[ii], "\n"))
      
    }
  
  cat(paste0("finished moving ", photo_directories[i], "\n"))
  
}

cat(paste0("*****finish subset by gcp*****\n"))
cat(paste0("\n"))