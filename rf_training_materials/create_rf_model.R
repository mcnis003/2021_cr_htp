library(randomForest)
library(jpeg)
library(plyr)

classifications=read.csv("/Users/ianmcnish/GoogleDrive/projects/2017_btn/rf_training_materials/training_data_classifications.csv")
digital_numbers=read.csv("/Users/ianmcnish/GoogleDrive/projects/2017_btn/rf_training_materials/training_data_dn_values.csv")
combined_data=merge(classifications,digital_numbers)
combined_data$red=combined_data$red*255
combined_data$green=combined_data$green*255
combined_data$blue=combined_data$blue*255

model=randomForest(classification~red+green+blue,data=combined_data,ntree=100)
save(model, file = paste("/Users/ianmcnish/GoogleDrive/projects/2017_btn/rf_training_materials/","rgb","_model.rda", sep=""))
