setwd("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis")
set.seed(05032020)

library(sqldf)
library(ggplot2)
library(doParallel)
library(caret)
library(irr)
library(rel)

# read data
all_results <- read.csv(file = "results/image_and_field_data.csv")
all_results$date_environment <- paste0(all_results$date, all_results$environment)
all_results$cr_severity <- as.numeric(all_results$cr_severity)

# set folds to be observation dates
n_date_environment_combos <- length(unique(all_results$date_environment))
folds <- groupKFold(group = all_results$date_environment, k = n_date_environment_combos)

# set modeling parameters
objControl <- trainControl(method = "cv", number = n_date_environment_combos, index = folds)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# train model
objModel <- train(cr_severity~mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                  data = all_results,
                  method='rf', 
                  trControl=objControl)

# end training
stopCluster(cl)

#report results
sink(file = "results/cross_validation_results.txt")
cat("cross-validation R2\n")
caret::R2(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE, formula = 'corr')
cat("cross-validation MAE\n")
caret::MAE(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE)
cat("cross-validation RMSE\n")
caret::RMSE(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE)
cat("cross-validation variable importance\n")
objModel$finalModel$importance
sink()
