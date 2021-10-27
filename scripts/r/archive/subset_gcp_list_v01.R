#!/usr/bin/env Rscript

# use: "/usr/local/bin/Rscript ../../scripts/r/subset_gcp_list_vc.R --vanilla"

# import the data output from the autodetection program, name the header, and modify entries for use in opendronemap

data <- read.csv("gcp_lists/combined_gcp_list_from_auto.csv", header = FALSE)
names(data) <- c("file", "channel", "object", "x", "y")
data <- data[,c("channel", "object", "x", "y", "file")]
data$channel <- sub("_.*", "", data$channel)
data$file <- sub("^.*/", "", data$file)

#produce list of channels
channels <- unique(data$channel)

#loop through channels, subset by channel, and print results
for (i in 1:length(channels)){
  data_temp <- data[data$channel == channels[i], c(2:5)]
  file_name <- paste0("gcp_lists/", channels[i], "_", "gcp_list.csv")
  write.csv(x = data_temp, file = file_name, row.names = FALSE)
}
