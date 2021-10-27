#!/usr/bin/env Rscript

# use: "/usr/local/bin/Rscript ../../scripts/r/format_gcp_lists_for_odm_vc.R --vanilla"
#setwd("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/working_photos/btn_2018_06_15_flight_1_120ft/")

cat(paste0("\n"))
cat(paste0("*****begin formating gcp list*****\n"))
cat(paste0("\n"))

#load packages
library(sqldf)

args = commandArgs(trailingOnly=TRUE)
#args <- c("manual")

#remove hyphens from file names
file.rename(list.files(pattern="calibrated_.*", recursive = TRUE), sub("-", "_", list.files(pattern="calibrated_.*", recursive = TRUE)))

#load gcp coordinates from autodetection process
if (args[1] == "auto"){
  
  #load gcp definitions from other process, updates to this data come from r script in that folder
  gcp_definitions <- read.csv("../../ground_control_points/gcp_positions/results/gcp_x_y.csv")
  
  gcp_positions <- read.csv("combined_control_panel_results.csv", header = FALSE)
  
  names(gcp_positions) <- c("file_name", "channel", "ground_control_point_name", "x", "y")
  gcp_positions$file_name <- sub("-", "_", gcp_positions$file_name)
  
  #merge and format gcp data
  gcp_list <- sqldf('select *
                    from gcp_positions
                    left join gcp_definitions
                    on gcp_positions.ground_control_point_name = gcp_definitions.ground_control_point_name')
  
  gcp_list$elevation <- 298
  gcp_list$file_name <- sub(".*/.*/", "", gcp_list$file_name)
  gcp_list$channel <- sub("_.*_.*_gcps", "", gcp_list$channel)
  gcp_list <- gcp_list[, c("channel", "x_utm", "y_utm", "elevation", "x", "y", "file_name")]
  
  
}

#or load gcp coordinates from manual process
if (args[1] == "manual"){
  
  #load manual gcp definitions
  gcp_definitions <- read.csv("../../ground_control_points/2017_gcp_templates/gcp_descriptions.csv")
  
  gcp_positions_rgb <- read.csv("gcp_lists/rgb_gcp_list_original.csv", header = TRUE)
  gcp_positions_655nm <- read.csv("gcp_lists/655_nm_gcp_list_original.csv", header = TRUE)
  gcp_positions_725nm <- read.csv("gcp_lists/725_nm_gcp_list_original.csv", header = TRUE)
  gcp_positions_800nm <- read.csv("gcp_lists/800_nm_gcp_list_original.csv", header = TRUE)
  
  gcp_positions_rgb$file <- sub("-", "_", gcp_positions_rgb$file)
  gcp_positions_rgb <- gcp_positions_rgb[which(gcp_positions_rgb$file %in% sub(pattern = "calibrated_",
                                                                               replacement = "",
                                                                               x = list.files(path = "rgb_autoexposure_calibration"))),]
  gcp_positions_rgb$channel <- "rgb"
  
  gcp_positions_655nm$file <- sub("-", "_", gcp_positions_655nm$file)
  gcp_positions_655nm <- gcp_positions_655nm[which(gcp_positions_655nm$file %in% sub(pattern = "calibrated_",
                                                                               replacement = "",
                                                                               x = list.files(path = "655nm_autoexposure_calibration"))),]
  gcp_positions_655nm$channel <- "655nm"
  
  gcp_positions_725nm$file <- sub("-", "_", gcp_positions_725nm$file)
  gcp_positions_725nm <- gcp_positions_725nm[which(gcp_positions_725nm$file %in% sub(pattern = "calibrated_",
                                                                                   replacement = "",
                                                                                   x = list.files(path = "725nm_autoexposure_calibration"))),]
  gcp_positions_725nm$channel <- "725nm"
  
  gcp_positions_800nm$file <- sub("-", "_", gcp_positions_800nm$file)
  gcp_positions_800nm <- gcp_positions_800nm[which(gcp_positions_800nm$file %in% sub(pattern = "calibrated_",
                                                                                   replacement = "",
                                                                                   x = list.files(path = "800nm_autoexposure_calibration"))),]
  gcp_positions_800nm$channel <- "800nm"
  
  gcp_positions <- rbind(gcp_positions_rgb, gcp_positions_655nm, gcp_positions_725nm, gcp_positions_800nm)
  gcp_positions <- gcp_positions[, c("file", "channel", "object", "x", "y")]
  names(gcp_positions) <- c("file_name", "channel", "ground_control_point_name", "x", "y")
  
  gcp_list <- sqldf('select *
                    from gcp_positions
                    left join gcp_definitions
                    on gcp_positions.ground_control_point_name = gcp_definitions.object')
  
  gcp_list <- gcp_list[, c("channel", "x_utm", "y_utm", "elevation", "x", "y", "file_name")]
  gcp_list$file_name <- paste0("calibrated_", gcp_list$file_name)
  
}

if(args[1] != "auto" && args[1] != "manual"){
  
  cat(paste0("\n"))
  cat(paste0("*****gcp flag not set! quiting****\n"))
  cat(paste0("\n"))
  
  quit()
  
}

#loop through channels with gcp data and print results to file for use in ODM
for (i in 1:length(unique(gcp_list$channel))){
  gcp_list_temp <- gcp_list[gcp_list$channel == unique(gcp_list$channel)[i],]
  gcp_list_temp <- gcp_list_temp[, -1]
  
  cat(paste0("EPSG:32615", '\n'), file = paste0("gcp_lists/", unique(gcp_list$channel)[i], "_gcp_list.txt"))
  write.table(gcp_list_temp,file = paste0("gcp_lists/",unique(gcp_list$channel)[i], "_gcp_list.txt"),quote = FALSE,row.names = FALSE,col.names = FALSE,append=TRUE,sep='\t')
}

Sys.setenv(TZ="CST6CDT")

#completion file for use in the makefile
sink(file = 'status/04_gcp_lists_complete.txt')
cat(paste("gcp lists complete: ",Sys.time(),sep=""))
sink()

cat(paste0("\n"))
cat(paste0("*****finish formating gcp list*****\n"))
cat(paste0("\n"))