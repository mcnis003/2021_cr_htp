#!/usr/bin/env Rscript

cat(paste0("\n"))
cat(paste0("*****begin subsetting gcp lists*****\n"))
cat(paste0("\n"))

#load packages
library(sqldf)

#args for troublshooting, order: channel folder, gcp definitions file, scaling factor, wgs84 region code, project name
args=c(1.9231, "EPSG:32615")
#setwd("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/working_photos/btn_2017_06_04_flight_1_120ft/")

#read gcp definitions
gcp_definitions=read.csv("../../ground_control_points/2017_gcp_templates/gcp_descriptions.csv")

#vectors of channels for loop
channels=c("rgb_gcp_list_original.csv",
           "655_nm_gcp_list_original.csv",
           "725_nm_gcp_list_original.csv",
           "800_nm_gcp_list_original.csv")

result_files=c("rgb_gcp_list.txt",
               "655nm_gcp_list.txt",
               "725nm_gcp_list.txt",
               "800nm_gcp_list.txt")

calibration_directories=c("rgb_autoexposure_calibration/",
                          "655nm_autoexposure_calibration/",
                          "725nm_autoexposure_calibration/",
                          "800nm_autoexposure_calibration/")

#loop through channels
for (i in 1:length(channels)){

  #read the unscaled gcps
  gcp_list=read.csv(paste("gcp_lists/",channels[i],sep=""))

  #create a new gcp table and rescale those gcps by args[1]
  gcp_list_unscaled=sqldf("select gcp_definitions.easting, gcp_definitions.northing, gcp_definitions.elevation, gcp_list.x, gcp_list.y, gcp_list.file from gcp_list left join gcp_definitions on gcp_list.object = gcp_definitions.object")
  gcp_list_rescaled=gcp_list_unscaled
  if (i==2){
    gcp_list_rescaled$x=round(as.numeric((2400/1000))*gcp_list_rescaled$x, digits = 0)
    gcp_list_rescaled$y=round(as.numeric((2400/1000))*gcp_list_rescaled$y, digits = 0)
  }else{
    gcp_list_rescaled$x=round(as.numeric(args[1])*gcp_list_rescaled$x, digits = 0)
    gcp_list_rescaled$y=round(as.numeric(args[1])*gcp_list_rescaled$y, digits = 0)
  }
  
  #add "calibrated_" to file names
  gcp_list_rescaled$file=paste("calibrated_",gcp_list_rescaled$file,sep="")
  calibrated_file_list=as.data.frame(list.files(calibration_directories[i]))
  colnames(calibrated_file_list)<-"file"
  
  #limit gcp list to photos in calibrated photos folder
  gcp_list_rescaled=sqldf("select gcp_list_rescaled.easting, gcp_list_rescaled.northing, gcp_list_rescaled.elevation, gcp_list_rescaled.x, gcp_list_rescaled.y, gcp_list_rescaled.file from gcp_list_rescaled inner join calibrated_file_list on gcp_list_rescaled.file = calibrated_file_list.file")
  
  #insert the WGS84 code for mn region and write the rescaled data to the new file without header, row names, or quoted strings
  cat(paste(args[2],'\n',sep=""), file=paste("gcp_lists/",result_files[i],sep=""))
  write.table(gcp_list_rescaled,file=paste("gcp_lists/",result_files[i],sep=""),quote=FALSE,row.names = FALSE,col.names = FALSE, append=TRUE, sep='\t')

}

#completion file for use in the makefile
sink(file='status/04_gcp_lists_complete.txt')
cat(paste("gcp transfer complete: ",Sys.time(),sep=""))
sink()

cat(paste0("\n"))
cat(paste0("*****finish subsetting gcp lists*****\n"))
cat(paste0("\n"))
