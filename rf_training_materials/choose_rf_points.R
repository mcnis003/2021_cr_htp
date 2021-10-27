library(jpeg)
setwd("/Users/ianmcnish/GoogleDrive/projects/2017_btn/working_photos/")

set.seed(07152017)
n_per_photo=10
n_photos=10

flight_list=c("btn_2017_06_12_flight_1_120ft/","btn_2017_06_21_flight_1_120ft/","btn_2017_06_26_flight_1_120ft/","btn_2017_06_27_flight_1_120ft/","btn_2017_06_29_flight_1_120ft/","btn_2017_06_30_flight_1_120ft/","btn_2017_07_03_flight_1_120ft/","btn_2017_07_05_flight_1_120ft/")
results=data.frame(matrix(vector(), 0, 6,dimnames=list(c(), c("file","x", "y", "red","green","blue"))),stringsAsFactors=F)



for (i in 1:length(flight_list)){
  
  file_list=list.files(path=paste(flight_list[i],"rgb_autoexposure_calibration",sep=""),pattern = "*.jpg",full.names = TRUE)
  sample_photos=sample(file_list,size=n_photos)
  
  for (ii in 1:length(sample_photos)){
    
    points=data.frame(matrix(vector(), n_per_photo, 6,dimnames=list(c(), c("file","x", "y", "red","green","blue"))),stringsAsFactors=F)
    img <- readJPEG(sample_photos[ii])
    height=dim(img)[1]
    width=dim(img)[2]
    
    for(iii in 1:n_per_photo){
      
      points$file[iii]=sample_photos[ii]
      points$x[iii]=sample(1:width,1)
      points$y[iii]=sample(1:height,1)
      points$red[iii]=img[points$y[iii],points$x[iii],1]
      points$green[iii]=img[points$y[iii],points$x[iii],2]
      points$blue[iii]=img[points$y[iii],points$x[iii],3]
      
    }
    
    
    
    results=rbind(results,points)
    
    
    
  }
  
}

write.csv(results,"/Users/ianmcnish/GoogleDrive/projects/2017_btn/rf_training_materials/training_data_dn_values.csv",row.names = FALSE)

