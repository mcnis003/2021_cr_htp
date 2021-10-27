#!/usr/bin/env Rscript

# use: "/usr/local/bin/Rscript ../../scripts/r/calibrate_by_control_panels_vc.R --vanilla"

# the purpose of this script is to load the control panel data extracted by the cpp script find_control_panel,
# calculte calibration curves, and calibrate the orthomosaic photos

library(plyr)
library(reshape2)
library(sqldf)
library(tiff)

library(devtools)
install("../../scripts/rfunctions")
library(rfunctions)

control_panel_data <- read.csv("gcp_lists/control_panel_results.csv", header = FALSE)
names(control_panel_data) <- c("control_panel", "template", "black", "dark_gray", "light_gray", "white")

control_panel_measurements <- read.csv("../../control_panel_measurements/control_panel_measurements_tall.csv",header = TRUE)

reference_reflection <- expand.grid(unique(control_panel_measurements$panel), unique(control_panel_measurements$color))
names(reference_reflection) <- c("panel", "color")
wavelengths <- c(655, 725, 800)

for (i in 1:nrow(reference_reflection)){
  
  temp <- control_panel_measurements[which(control_panel_measurements$panel == reference_reflection$panel[i] & control_panel_measurements$color == reference_reflection$color[i]),]
  
  reference_reflection$x655[i] <- mean(temp[which(temp$variable > 650 & temp$variable < 660),]$value)
  reference_reflection$x725[i] <- mean(temp[which(temp$variable > 720 & temp$variable < 730),]$value)
  reference_reflection$x800[i] <- mean(temp[which(temp$variable > 795 & temp$variable < 805),]$value)
  
}

reference_reflection <- melt(data = reference_reflection, id.vars = c("panel", "color"))

control_panel_data$channel <- substr(control_panel_data$control_panel, 0, 3)
control_panel_data$control_panel <- sub(".*/", "", control_panel_data$control_panel)
control_panel_data$control_panel <- sub("\\.tif", "", control_panel_data$control_panel)

control_panel_data <- melt(data = control_panel_data, id.vars = c("control_panel", "template", "channel"))

#rgb is not calibrated
control_panel_data <-  control_panel_data[control_panel_data$channel != "rgb",]
control_panel_data$value <- sub("\\[", "", control_panel_data$value)
control_panel_data$value <- sub("\\]", "", control_panel_data$value)
Dn <- strsplit(control_panel_data$value, split = ",")
Dn <- t(as.data.frame(Dn))
control_panel_data$obs_dn <- Dn[,1]

control_panel_data <- control_panel_data[, c("control_panel", "channel", "variable", "obs_dn")]
control_panel_data$control_panel <- mapvalues(x = control_panel_data$control_panel, from = c("west", "middle", "east"), to = c(4, 5, 6))
reference_reflection$variable <- sub("x", "", reference_reflection$variable)

control_panel_data <- sqldf('select control_panel_data.control_panel,
                            control_panel_data.channel,
                            control_panel_data.variable as color,
                            control_panel_data.obs_dn,
                            reference_reflection.value as exp_ref
                            from control_panel_data
                            left join reference_reflection
                            on control_panel_data.control_panel = reference_reflection.panel
                            and control_panel_data.variable = reference_reflection.color
                            and control_panel_data.channel = reference_reflection.variable')

channels <- unique(control_panel_data$channel)
intercept <- as.vector(NULL)
slope <- as.vector(NULL)
for (i in 1:length(channels)){
  temp <- control_panel_data[control_panel_data$channel == channels[i],]
  model <- lm(exp_ref ~ obs_dn, data = temp)
  intercept[i] <- model$coefficients[1]
  slope[i] <- model$coefficients[2]
  plot(temp$obs_dn, temp$exp_ref)
}

  
calibrate_by_panels(intercept = intercept[1],
                    slope = slope[1],
                    original_file = "655nm_autoexposure_calibration/odm_orthophoto.tif",
                    new_file = "655nm_autoexposure_calibration/odm_orthophoto_calibrated.tif",
                    correction_file = "655nm_autoexposure_calibration/orthophoto_correction.txt")

calibrate_by_panels(intercept = intercept[2],
                    slope = slope[2],
                    original_file = "725nm_autoexposure_calibration/odm_orthophoto.tif",
                    new_file = "725nm_autoexposure_calibration/odm_orthophoto_calibrated.tif",
                    correction_file = "725nm_autoexposure_calibration/orthophoto_correction.txt")

calibrate_by_panels(intercept = intercept[3],
                    slope = slope[3],
                    original_file = "800nm_autoexposure_calibration/odm_orthophoto.tif",
                    new_file = "800nm_autoexposure_calibration/odm_orthophoto_calibrated.tif",
                    correction_file = "800nm_autoexposure_calibration/orthophoto_correction.txt")

