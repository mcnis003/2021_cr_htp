x_to_utm <- function(world_file, x){
  
  world_file_data <- read.table(world_file)
  
  x_scale <- world_file_data[1,1]
  y_rotation <- world_file_data[2,1]
  x_rotation <- world_file_data[3,1]
  y_scale <- world_file_data[4,1]
  x_reference <- world_file_data[5,1]
  y_reference <- world_file_data[6,1]
  
  lat_utm <- x_reference + x*x_scale
  
  return(lat_utm)
  
}