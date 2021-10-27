#!/usr/bin/env Rscript

# use: "/usr/local/bin/Rscript ../../scripts/r/calibrate_by_autoexposure_vc.R --vanilla"

cat(paste0("\n"))
cat(paste0("*****begin autoexposure calibration*****\n"))

#load libraries
library(jpeg)
library(exif)
library(plyr)
library(tiff)

#these libraries are used for parallel computing
library(foreach)
library(doMC)
registerDoMC(4) #number of parallel tasks, in this case 4 is the maximum for this process (4 wavelengths)

#names of channel folders, these are used to create the folders and to control loops
channels <- c("rgb/","655nm/","725nm/","800nm/")
folders <- c("rgb_autoexposure_calibration","655nm_autoexposure_calibration","725nm_autoexposure_calibration","800nm_autoexposure_calibration")

#this code recreeates the quantum efficiency curve of the sentera quad cam sensor, this information is used to calibrate the narrow channel photos
x <- 350:1000
y <- 318.1818*dnorm(x,525,183.559)
qe <- NULL
qe$wavelength <- x
qe$efficiency <- y
qe <- as.data.frame(qe)
qe_655_nm <- sum(qe[(650-350):(660-350),2])
qe_725_nm <- sum(qe[(720-350):(730-350),2])
qe_800_nm <- sum(qe[(795-350):(805-350),2])

#the values from the rgb camera are not calibrated by quantum efficiency
qe_rgb <- 1

#the quantum efficiencies are placed into an vector so that they can be accessed in the loop
quantum_efficiencies <- c(qe_rgb,qe_655_nm,qe_725_nm,qe_800_nm)

#placeholder vector that will hold the correction factor for each sensor, the correction factor is neccesary because the values are saved as 8-bit images
correction <- c(0,0,0,0)
correction_file_name <- c("rgb_correction.txt","655nm_correction.txt","725nm_correction.txt","800nm_correction.txt")

#this code executes the calibration procedure for each sensor in parallel, there is no error handling for missing sensor data
foreach(i=1:length(channels)) %dopar%{

  #create the directory for this sensor, read in the autoexposure data for the sensor, generate lists of file paths and names to use in the loop
  dir.create(paste0(folders[i]))
  autoexposure_data <- read.csv(paste0(channels[i],"autoexposure.csv"))
  file_paths <- list.files(path=paste0(channels[i]),pattern="*.jpg",full.names = TRUE)
  file_names <- list.files(path=paste0(channels[i]),pattern="*.jpg",full.names = FALSE)
  
  #the correction factor is determined for each channel seperately using the 10th photo, the correction factor is a value that will generate a photo with a mean Dn at 1/10 the dynamic range. Setting this as a lower number eliminates the possibility of calculating values > 255.
  #loop through files in sensor folder
  max_Dn <- vector(length=length(file_paths), mode = 'numeric')
  for (ii in 1:length(file_paths)){
    #this code correct the number of channels in single channel photos
    assign(paste0('photo',ii),readJPEG(file_paths[ii]))
    temp_photo <- get(paste('photo',ii,sep=""))
    temp_photo <- 255*temp_photo #calculate Dn
    #these calculations are defined in sentera literature
    temp_photo <- temp_photo/(autoexposure_data$Integration.Time..us.[ii]*(2^autoexposure_data$Analog.Gain[ii])*autoexposure_data$Digital.Gain[ii])
    temp_photo <- temp_photo/quantum_efficiencies[i]
    max_Dn[ii] <- max(temp_photo)
    #assign(paste('photo',ii,sep=""),temp_photo)
  }
  correction <- 0.99/max(max_Dn)
  
  #save the correction factor, the Dn will need to be divided by the correction factor later in the analysis
  sink(file=paste0(folders[i],"/",correction_file_name[i]))
  cat(correction)
  sink()
  
  #loop through files in sensor folder
  for (iii in 1:length(file_paths)){
    #this code correct the number of channels in single channel photos
    if (i==1){
      photo <- readJPEG(file_paths[iii])
    } else{
        photo <- readJPEG(file_paths[iii])
        photo <- photo[,,1]
        }
    photo <- 255*photo #calculate Dn
    #these calculations is defined in sentera literature
    photo <- photo/(autoexposure_data$Integration.Time..us.[iii]*(2^autoexposure_data$Analog.Gain[iii])*autoexposure_data$Digital.Gain[iii])
    photo <- photo/quantum_efficiencies[i]
    photo <- correction*photo
    #photo <- 255*photo
    #file_names_tiff=sub("*.jpg",".tif",file_names)
    
    #save the calibrated file
    #writeTIFF(correction[i]*photo,paste0(folders[i],"/calibrated_",file_names_tiff[ii]), bits.per.sample = 16L)
    if (i==1){writeJPEG(photo,paste0(folders[i],"/calibrated_",file_names[iii]), quality = 1)
    } else{writeJPEG(photo,paste0(folders[i],"/calibrated_",file_names[iii]), quality = 1)}
    
    #this code uses the terminal perl tool exifdata to transfer lat/lon data from the original photo to the new photo
    
    from_file <- paste0(channels[i],file_names[iii])
    #to_file=paste0(folders[i],"/calibrated_",file_names_tiff[ii])
    to_file <- paste0(folders[i],"/calibrated_",file_names[iii])
    
    command <- paste0("exiftool -overwrite_original -TagsFromFile ",from_file," ",to_file)
    system(command)
    
  }
  
}

#completion file for use in the makefile

Sys.setenv(TZ="CST6CDT")

sink(file=paste0('status/02_autoexposure_calibration_complete.txt'))
cat(paste0("autoexposure calibration complete: ",Sys.time()))
sink()

cat(paste0("*****finish autoexposure calibration*****\n"))
cat(paste0("\n"))