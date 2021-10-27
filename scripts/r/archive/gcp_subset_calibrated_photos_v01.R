args = commandArgs(trailingOnly=TRUE)

#args for troubleshooting
#args=vector(length=7)
#args[1]="/Users/ianmcnish/GoogleDrive/projects/2017_btn/working_photos/btn_2017_06_04_flight_1_120ft/"
#args[2]=44.9897
#args[3]=-93.18528
#args[4]=44.989348
#args[5]=-93.185762
#args[6]="founders_pyt_f2"

channels=c("rgb/","655_nm/","725_nm/","800_nm/")
folders=c("rgb_autoexposure_calibration","655_nm_autoexposure_calibration","725_nm_autoexposure_calibration","800_nm_autoexposure_calibration")

dir.create(paste(args[1],args[6],"/",sep=""))

for (i in 1:4){
  
  dir.create(paste(args[1],args[6],"/",folders[i],sep=""))
  location_data=read.csv(paste(args[1],channels[i],"pix4d.csv",sep=""))
  location_data$File.Name.tiff=sub("*.jpg",".tif",location_data$File.Name)
  location_data_subset=subset(location_data,location_data$Lat..decimal.degrees. < args[2])
  location_data_subset=subset(location_data_subset,location_data_subset$Lon..decimal.degrees. < args[3])
  location_data_subset=subset(location_data_subset,location_data_subset$Lat..decimal.degrees. > args[4])
  location_data_subset=subset(location_data_subset,location_data_subset$Lon..decimal.degrees. > args[5])
  
  for (ii in 1:nrow(location_data)){
    file.copy(paste(args[1],folders[i],"/","calibrated_",location_data_subset$File.Name.tiff[ii],sep=""),paste(args[1],args[6],"/",folders[i],"/","calibrated_",location_data_subset$File.Name.tiff[ii],sep=""))
  }
  
}

sink(file=paste(args[1],args[6],"/",'subset_calibrated_photos_complete.txt',sep=""))
cat(paste("subset calibrated photos complete: ",Sys.time(),sep=""))
sink()