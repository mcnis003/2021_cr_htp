library(reshape)
library(dplyr)
library(lubridate)
library(agricolae)
library(rrBLUP)
source("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2019_plant_genome_cr_gwas/scripts/functions.R")

#load field data
field_data_2017_early <- read.csv("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2019_plant_genome_cr_gwas/data/17_founder_BT_early.csv")
field_data_2017_early <- field_data_2017_early[, c("PlotId",
                                                   "Name",
                                                   "CR",
                                                   "CR_2",
                                                   "CR_3",
                                                   "CR_4",
                                                   "CR_5",
                                                   "CR_6",
                                                   "CR_7",
                                                   "CR_8",
                                                   "CR_9",
                                                   "CR_11",
                                                   "CR_12",
                                                   "CR_13",
                                                   "CR_15",
                                                   "CR_16",
                                                   "heading_date")]

field_data_2017_late <- read.csv("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2019_plant_genome_cr_gwas/data/17_founder_BT_late.csv")
field_data_2017_late <- field_data_2017_late[, c("PlotId",
                                                 "Name",
                                                 "CR_7",
                                                 "CR_8",
                                                 "CR_9",
                                                 "CR_11",
                                                 "CR_12",
                                                 "CR_13",
                                                 "CR_15",
                                                 "CR_16",
                                                 "heading_date")]

field_data_2018 <- read.csv("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2019_plant_genome_cr_gwas/data/18_founder_BT.csv")
field_data_2018 <- field_data_2018[, c("PlotId",
                                       "Name",
                                       "CR_1",
                                       "CR_2",
                                       "CR_3",
                                       "CR_4",
                                       "CR_5",
                                       "CR_6",
                                       "CR_7")]

dates_2017 <- read.csv("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2019_plant_genome_cr_gwas/data/2017_bt_founder_observation_dates.csv")
dates_2018 <- read.csv("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2019_plant_genome_cr_gwas/data/2018_bt_founder_observation_dates.csv")

#create flat tables
flat_field_data_2017_early <- cr_mm_flat(df = field_data_2017_early,
                                         dates = dates_2017)

flat_field_data_2017_late <- cr_mm_flat(df = field_data_2017_late,
                                        dates = dates_2017)

flat_field_data_2018 <- cr_mm_flat(df = field_data_2018,
                                   dates = dates_2018)

write.csv(x = flat_field_data_2017_early, file = "/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis/results/flat_field_data_2017_early.csv", row.names = FALSE)

write.csv(x = flat_field_data_2017_late, file = "/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis/results/flat_field_data_2017_late.csv", row.names = FALSE)

write.csv(x = flat_field_data_2018, file = "/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis/results/flat_field_data_2018.csv", row.names = FALSE)

#calculate audpc for each environment
audpc_field_data_2017_early <- calculate_audpc(df = flat_field_data_2017_early,
                                               trial = "2017_early")

audpc_field_data_2017_late <- calculate_audpc(df = flat_field_data_2017_late,
                                              trial = "2017_late")

audpc_field_data_2018 <- calculate_audpc(df = flat_field_data_2018,
                                         trial = "2018")



#combine results
audpc_field_data_2017_early <- merge(x = audpc_field_data_2017_early,
                                     y = field_data_2017_early,
                                     by.x = "plot_id",
                                     by = "PlotId")

audpc_field_data_2017_late <- merge(x = audpc_field_data_2017_late,
                                    y = field_data_2017_late,
                                    by.x = "plot_id",
                                    by = "PlotId")

audpc_field_data_2018 <- merge(x = audpc_field_data_2018,
                                    y = field_data_2018,
                                    by.x = "plot_id",
                                    by = "PlotId")

write.csv(x = audpc_field_data_2017_early,
          file = "/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis/results/audpc_field_data_2017_early.csv",
          row.names = FALSE)

write.csv(x = audpc_field_data_2017_late,
          file = "/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis/results/audpc_field_data_2017_late.csv",
          row.names = FALSE)

write.csv(x = audpc_field_data_2018,
          file = "/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis/results/audpc_field_data_2018.csv",
          row.names = FALSE)
