#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#load libraries
library(jpeg)
library(exif)
library(plyr)
library(sqldf)
library(raster)

#these libraries are used for parallel computing
library(foreach)
library(doMC)
registerDoMC(4) #number of parallel tasks, in this case 4 is the maximum for this process (4 wavelengths)

#arguments for troubleshooting, order: flight folder
#args=c("/Users/ianmcnish/GoogleDrive/projects/2017_btn/working_photos/btn_2017_06_04_flight_1_120ft/")

#names of channel folders, these are used to create the folders and to control loops
channels=c("rgb/","655_nm/","725_nm/","800_nm/")
folders=c("rgb_autoexposure_calibration","655_nm_autoexposure_calibration","725_nm_autoexposure_calibration","800_nm_autoexposure_calibration")

#this code recreeates the quantum efficiency curve of the sentera quad cam sensor, this information is used to calibrate the narrow channel photos
x=350:1000
y=318.1818*dnorm(x,525,183.559)
qe=NULL
qe$wavelength=x
qe$efficiency=y
qe=as.data.frame(qe)
qe_655_nm=sum(qe[(650-350):(660-350),2])
qe_725_nm=sum(qe[(720-350):(730-350),2])
qe_800_nm=sum(qe[(795-350):(805-350),2])

#the values from the rgb camera are not calibrated by quantum efficiency
qe_rgb=1

#the quantum efficiencies are placed into an vector so that they can be accessed in the loop
quantum_efficiencies=c(qe_rgb,qe_655_nm,qe_725_nm,qe_800_nm)

#placeholder vector that will hold the correction factor for each sensor, the correction factor is neccesary because the values are saved as 8-bit images
correction=c(0,0,0,0)
correction_file_name=c("rgb_correction.txt","655_nm_correction.txt","725_nm_correction.txt","800_nm_correction.txt")

#this code executes the calibration procedure for each sensor in parallel, there is no error handling for missing sensor data
for (i in 1:length(channels)){

  #create the directory for this sensor, read in the autoexposure data for the sensor, generate lists of file paths and names to use in the loop
  dir.create(paste(args[1],folders[i],sep=""))
  autoexposure_data=read.csv(paste(args[1],channels[i],"autoexposure.csv",sep=""))
  image_meta_data=read.csv(paste(args[1],channels[i],"image-metadata.txt",sep=""))
  jpeg_data=read.csv(paste(args[1],channels[i],"jpegs.csv",sep=""))
  gps_data=read.csv(paste(args[1],channels[i],"gps.csv",sep=""))
  file_paths=list.files(path=paste(args[1],channels[i],sep=""),pattern="*.jpg",full.names = TRUE)
  file_names=list.files(path=paste(args[1],channels[i],sep=""),pattern="*.jpg",full.names = FALSE)
  
  #loop through files in sensor folder
  max_Dn=vector(length=length(file_paths))
  for (ii in 1:length(file_paths)){
    #this code correct the number of channels in single channel photos
      assign(paste("photo",ii,sep=""),readJPEG(file_paths[ii]))
    temp_photo=get(paste("photo",ii,sep=""))
    temp_photo=255*temp_photo #calculate Dn
    #these calculations are defined in sentera literature
    temp_photo=temp_photo/(autoexposure_data$Integration.Time..us.[ii]*(2^autoexposure_data$Analog.Gain[ii])*autoexposure_data$Digital.Gain[ii])
    temp_photo=temp_photo/quantum_efficiencies[i]
    max_Dn[ii]=max(temp_photo)
    assign(paste("photo",ii,sep=""),temp_photo)
  }
  correction=0.95/max(max_Dn)
  
  for(ii in 1:length(file_paths)){
    
    if (i==2){
      assign(paste("photo",ii,sep=""),get(paste("photo",ii,sep=""))[0:950,0:1000,1])
    }
    
    writeJPEG(correction*get(paste("photo",ii,sep="")),paste(args[1],folders[i],"/calibrated_",file_names[ii],sep=""))
    
    from_file=paste(args[1],channels[i],file_names[ii],sep="")
    to_file=paste(args[1],folders[i],"/calibrated_",file_names[ii],sep="")
    timestamp=jpeg_data$Time.Stamp..us.since.epoch.[which(jpeg_data$File.Name==file_names[ii])]
    new_lat=gps_data$Lat..deg.[which(abs(gps_data$Timestamp..microseconds.since.epoch.-timestamp)==min(abs(gps_data$Timestamp..microseconds.since.epoch.-timestamp)))]
    new_lon=gps_data$Lon..deg.[which(abs(gps_data$Timestamp..microseconds.since.epoch.-timestamp)==min(abs(gps_data$Timestamp..microseconds.since.epoch.-timestamp)))]
    
    if (i==1){
      command=paste("exiftool -overwrite_original -TagsFromFile ",from_file," ",to_file,sep="")
      system(command)
      
    } else{
      command=paste("exiftool -overwrite_original -TagsFromFile ",from_file," ",to_file,sep="")
      system(command)
      command=paste("convert ",to_file," -colorspace Gray ",to_file,sep="")
      system(command)
    }
    
    command=paste("exiftool -overwrite_original -GPSLatitude=\"",new_lat,"\" -GPSLongitude=\"",new_lon,"\" ",to_file,sep="")
    system(command)
    
  }
  
}

#completion file for use in the makefile
sink(file=paste(args[1],'autoexposure_calibration_complete.txt',sep=""))
cat(paste("autoexposure calibration complete: ",Sys.time(),sep=""))
sink()

