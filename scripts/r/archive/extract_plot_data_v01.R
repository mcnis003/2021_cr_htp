#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

library(jpeg)
library(plyr)

#arguments for troubleshooting
#args=c("/Users/ianmcnish/GoogleDrive/projects/2017_btn/working_photos/btn_2017_07_05_flight_1_120ft/founders/","/Users/ianmcnish/GoogleDrive/projects/2017_btn/working_photos/btn_2017_07_05_flight_1_120ft/founders/rgb_autoexposure_calibration/","/Users/ianmcnish/GoogleDrive/projects/2017_btn/working_photos/btn_2017_07_05_flight_1_120ft/founders/rgb_autoexposure_calibration/red/","rgb")

plot_coords=read.csv("/Users/ianmcnish/GoogleDrive/projects/2017_btn/plot_coords/combined_coords_utm.csv",header=TRUE)
plot_coords=na.exclude(plot_coords)
year=readChar(paste(args[1],"year.txt",sep=""),4)
month=readChar(paste(args[1],"month.txt",sep=""),2)
day=readChar(paste(args[1],"day.txt",sep=""),2)
#date=as.Date(paste(year,month,day, sep=""),"%Y%m%d")

photo=readJPEG(paste(args[3],"image_calibrated.jpg",sep=""))
classifications=readJPEG(paste(args[1],"rgb_autoexposure_calibration/","rgb_image_cropped_classified.jpg",sep=""))

world_file=read.table(paste(args[2],"odm_orthophoto_cropped.wld",sep=""))

plot_coords$nw_x_photo=round((plot_coords$nw_x_utm-world_file[5,1])/world_file[1,1],0)
plot_coords$nw_y_photo=round((plot_coords$nw_y_utm-world_file[6,1])/world_file[4,1],0)
plot_coords$se_x_photo=round((plot_coords$se_x_utm-world_file[5,1])/world_file[1,1],0)
plot_coords$se_y_photo=round((plot_coords$se_y_utm-world_file[6,1])/world_file[4,1],0)

i=0

for (i in 1:nrow(plot_coords)){
  
  x_min=plot_coords$nw_x_photo[i]
  x_max=plot_coords$se_x_photo[i]
  y_min=plot_coords$nw_y_photo[i]
  y_max=plot_coords$se_y_photo[i]
  
  photo_subset=photo[y_min:y_max,x_min:x_max]
  classifications_subset=classifications[y_min:y_max,x_min:x_max]
  
  photo_subset=as.data.frame(as.table(photo_subset))
  classifications_subset=as.data.frame(as.table(classifications_subset))
  photo_subset$classification=classifications_subset$Freq
  photo_subset=subset(photo_subset, photo_subset$classification==0)

  plot_coords$mean[i]=mean(photo_subset$Freq)
  plot_coords$area[i]=nrow(photo_subset)
  plot_coords$year[i]=year
  plot_coords$month[i]=month
  plot_coords$day[i]=day
}

if(args[4]=="red"){
  colnames(plot_coords) <- c("plot","nw_x","nw_y","se_x","se_y","nw_x_utm","nw_y_utm","se_x_utm","se_y_utm","nw_x_photo","nw_y_photo","se_x_photo","se_y_photo","mean_red","area","year","month","day")
}

if(args[4]=="green"){
  colnames(plot_coords) <- c("plot","nw_x","nw_y","se_x","se_y","nw_x_utm","nw_y_utm","se_x_utm","se_y_utm","nw_x_photo","nw_y_photo","se_x_photo","se_y_photo","mean_green","area","year","month","day")
}

if(args[4]=="blue"){
  colnames(plot_coords) <- c("plot","nw_x","nw_y","se_x","se_y","nw_x_utm","nw_y_utm","se_x_utm","se_y_utm","nw_x_photo","nw_y_photo","se_x_photo","se_y_photo","mean_blue","area","year","month","day")
}

if(args[4]=="655_nm"){
  colnames(plot_coords) <- c("plot","nw_x","nw_y","se_x","se_y","nw_x_utm","nw_y_utm","se_x_utm","se_y_utm","nw_x_photo","nw_y_photo","se_x_photo","se_y_photo","mean_655_nm","area","year","month","day")
}

if(args[4]=="725_nm"){
  colnames(plot_coords) <- c("plot","nw_x","nw_y","se_x","se_y","nw_x_utm","nw_y_utm","se_x_utm","se_y_utm","nw_x_photo","nw_y_photo","se_x_photo","se_y_photo","mean_725_nm","area","year","month","day")
}

if(args[4]=="800_nm"){
  colnames(plot_coords) <- c("plot","nw_x","nw_y","se_x","se_y","nw_x_utm","nw_y_utm","se_x_utm","se_y_utm","nw_x_photo","nw_y_photo","se_x_photo","se_y_photo","mean_800_nm","area","year","month","day")
}


write.csv(plot_coords,paste(args[3],"channel_results.csv",sep=""),row.names=FALSE)

               