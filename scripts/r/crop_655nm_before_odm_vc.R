#!/usr/bin/env Rscript

# use: "/usr/local/bin/Rscript ../../scripts/r/crop_655nm_before_odm_vc.R --vanilla"

cat(paste0("\n"))
cat(paste0("*****begin cropping 655nm photos*****\n"))
cat(paste0("\n"))

#load libraries
library(jpeg)

#produce a list of files, when run in script there should only be "calibrated_*" photos, but using this pattern allows for use out of order
photos <- list.files(path = "655nm_autoexposure_calibration/", pattern = "calibrated_.*", full.names = TRUE)

#loop through photos, remove right most side of photos, save photos
for (i in 1:length(photos)){
  current_photo <- readJPEG(source = photos[i])
  current_photo <- current_photo[0:950,0:1000]
  writeJPEG(current_photo, target = photos[i], quality = 1)
  
  from_file <- paste0("655nm/",sub("655nm_autoexposure_calibration//calibrated_", "", photos[i]))
  to_file <- photos[i]
  command <- paste0("exiftool -overwrite_original -TagsFromFile ",from_file," ",to_file)
  system(command)
}

cat(paste0("\n"))
cat(paste0("*****finish cropping 655nm photos*****\n"))
cat(paste0("\n"))