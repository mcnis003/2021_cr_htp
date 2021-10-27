#!/usr/bin/env Rscript

# use: "/usr/local/bin/Rscript ../../scripts/r/classify_pixels_rgb_vc.R --vanilla"

# the purpose of this script is to classify the pizels of RGB orthophotos and print a black and white mask file

# set working directory and args while troubleshooting
#setwd("/Users/ianmcnish/Google\ Drive\ File\ Stream/My\ Drive/projects/2017_btn/working_photos/btn_2017_06_14_flight_1_120ft")

#load libraries
library(randomForest)
library(jpeg)
library(plyr)
library(tiff)

#load classification model
load("../../rf_training_materials/rgb_model.rda")

#load RGB image to be classified
image <- readTIFF("final_orthomosaic_data/rgb_orthophoto_cropped.tif", convert = TRUE)

cat("image loaded in R\n")

#manipulate model into flat format
red <- as.data.frame(as.table(image[,,1]))
colnames(red) <- c("y","x","red")
green <- as.data.frame(as.table(image[,,2]))
colnames(green) <- c("y","x","green")
blue <- as.data.frame(as.table(image[,,3]))
colnames(blue) <- c("y","x","blue")

#convert 0-1 scale to 0-255 scale
blue$Dn <- 255*blue$blue
red$red <- red$red*255
red$blue <- blue$blue*255
red$green <- green$green*255
newdata <- red[,3:5]

cat("image manipulated in R\n")

rows <- nrow(image)
columns <- ncol(image)

#classify the pixels based on previously produced model
image_classifications <- predict(model, newdata)

cat("image classified in R\n")

#manipulate classifcations into a matrix
red$classification <- image_classifications
red$classification <- revalue(red$classification, c("other"=1, "plant"=0))
red$classification <-as.numeric(red$classification)
red$classification <- red$classification-1
classifications <- matrix(red$classification, nrow = rows, ncol = columns)

cat("image rearranged in R\n")

writeTIFF(classifications, "final_orthomosaic_data/orthophoto_classified.tif")

cat("classified image saved in R\n")