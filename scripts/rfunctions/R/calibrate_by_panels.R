calibrate_by_panels <- function(intercept, slope, original_file, new_file, correction_file){
  
  cat("begin calibrating orthomosaic photo")
  
  tiff_orig <- readTIFF(source = original_file, convert = TRUE, info = FALSE)
  tiff_calibrated <- tiff_orig*intercept+slope
  tiff_calibrated[tiff_calibrated<0] <- 0
  tiff_correction <- 0.95/max(tiff_calibrated)
  tiff_calibrated <- tiff_655_correction*tiff_calibrated
  writeTIFF(tiff_calibrated, new_file)
  
  sink(file = correction_file)
  cat(tiff_correction)
  sink()
  
  cat("finish calibrating orthomosaic photo")

}