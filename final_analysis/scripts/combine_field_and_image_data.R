setwd("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis")
library(sqldf)

# read field observations
flat_field_data_2017_early <- read.csv(file = "results/flat_field_data_2017_early.csv")

flat_field_data_2017_late <- read.csv(file = "results/flat_field_data_2017_late.csv")

flat_field_data_2018 <- read.csv(file = "results/flat_field_data_2018.csv")

combined_field_data <- rbind(flat_field_data_2017_early,
                             flat_field_data_2017_late,
                             flat_field_data_2018)

combined_field_data$date <- as.Date(combined_field_data$date)

# read image data
flight_result_files <- list.files(path="/Users/ianmcnish/Google\ Drive\ File\ Stream/My\ Drive/projects/2017_btn/",pattern="flight_results.csv",recursive=TRUE,full.names = TRUE)

image_results <- read.csv(flight_result_files[1])

for (i in 2:length(flight_result_files)){
  temp <- read.csv(flight_result_files[i])
  image_results <- rbind(image_results, temp)
  }

image_results$date <- sub(pattern = "/Volumes/GoogleDrive/My Drive/projects/2017_btn/working_photos/btn_", replacement = "", x = image_results$flight)
image_results$date <- sub(pattern = "_flight_1_120ft", replacement = "", x = image_results$date)
image_results$date <- as.Date( image_results$date, "%Y_%m_%d")

all_results <- sqldf('select combined_field_data.plot_id,
                     combined_field_data.line,
                     combined_field_data.cr_severity,
                     combined_field_data.date,
                     image_results.mean_725nm,
                     image_results.mean_800nm,
                     image_results.mean_red,
                     image_results.mean_green,
                     image_results.mean_blue
                     from combined_field_data left join image_results
                      on combined_field_data.plot_id = image_results.plot
                        and combined_field_data.date = image_results.date')

all_results <- na.omit(all_results)

all_results$environment <- NA

for (i in 1:nrow(all_results)){
  
  if (all_results$plot_id[i] >= 5000 && all_results$plot_id <= 5499){
    all_results$environment[i] <- "2017_early"
  }
  
  if (all_results$plot_id[i] >= 5500 && all_results$plot_id <= 5999){
    all_results$environment[i] <- "2017_late"
  }
  
  if (all_results$plot_id[i] >= 9000 && all_results$plot_id <= 9999){
    all_results$environment[i] <- "2018"
  }
  
}

write.csv(x = all_results, file = "results/image_and_field_data.csv", row.names = FALSE)
