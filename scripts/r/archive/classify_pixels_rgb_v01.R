#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

library(randomForest)
library(jpeg)
library(plyr)

#arguments for troubleshooting
#args=c("/Users/ianmcnish/Google Drive/projects/2017_btn/working_photos/btn_2017_05_29_flight_1_120ft/BTN20170529flight1rgb_Orthomosaic_MonMay29200845/red/", "655_nm", "image_calibrated_aligned_cropped.jpg", "image_calibrated_aligned_cropped_classified.jpg")

load(paste("/Users/ianmcnish/GoogleDrive/projects/2017_btn/rf_training_materials/",args[2],"_model.rda",sep=""))
image=readJPEG(paste(args[1],args[3],sep=""))

red=as.data.frame(as.table(image[,,1]))
colnames(red) <- c("y","x","red")
green=as.data.frame(as.table(image[,,2]))
colnames(green) <- c("y","x","green")
blue=as.data.frame(as.table(image[,,3]))
colnames(blue) <- c("y","x","blue")
blue$Dn=255*blue$blue
red$red=red$red*255
red$blue=blue$blue*255
red$green=green$green*255
newdata=red[,3:5]


cat("image loaded in R\n\n")

#image cropped before fine alignment
#image_subset=image[3026:5711,2431:11072]

#writeJPEG(image_subset,paste(args[1],"image_calibrated_aligned_twice_cropped.jpg",sep=""))

rows=nrow(image)
columns=ncol(image)


#image=as.data.frame(as.table(image[,,1]))

cat("image rearranged in R\n\n")

#create Dn object for predictions
#Dn=image$Freq
#Dn=255*Dn

image_classifications=predict(model,newdata)

cat("image classified in R\n\n")

red$classification=image_classifications
red$classification <- revalue(red$classification, c("other"=1, "plant"=0))
red$classification <-as.numeric(red$classification)
red$classification=red$classification-1

classifications=matrix(red$classification,nrow=rows,ncol=columns)

writeJPEG(classifications,target=paste(args[1],args[4],sep=""),quality = 1)

cat("classified image saved in R\n\n")



