y_to_utm <- function(world_file, y){
  
  world_file_data <- read.table(world_file)
  
  x_scale <- world_file_data[1,1]
  y_rotation <- world_file_data[2,1]
  x_rotation <- world_file_data[3,1]
  y_scale <- world_file_data[4,1]
  x_reference <- world_file_data[5,1]
  y_reference <- world_file_data[6,1]
  
  lon_utm <- y_reference + y*y_scale
  
  return(lon_utm)
  
}