setwd("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis")
set.seed(05032020)

library(sqldf)
library(ggplot2)
library(doParallel)
library(caret)
library(irr)
library(rel)
library(plyr)
library(rnoaa)
library(chillR)

# read data
all_results <- read.csv(file = "results/image_and_field_data.csv")
all_results$date_environment <- paste(all_results$environment, all_results$date)
all_results$cr_severity <- as.numeric(as.character(all_results$cr_severity))

#How is the variance of visually observed disease severity among and within environments and time points?

#effect of environment (fixed) MSE: sigma2++sigma2_t:e+K^2_e
#effect of time points within environment (random) MSE: sigma^2+sigma2_t:e
simple_anova_model <- lm(formula = cr_severity ~ environment/date, data = all_results)
simple_anova_result <- anova(simple_anova_model)
#environment f-test
simple_anova_result$`F value`[1] = simple_anova_result$`Mean Sq`[1]/simple_anova_result$`Mean Sq`[2]
simple_anova_result$`Pr(>F)`[1] = pf(q = simple_anova_result$`F value`[1], df1 = simple_anova_result$Df[1], df2 = simple_anova_result$Df[2])
#write result
write.csv(x = simple_anova_result, file = "results/simple_anova_table.csv", row.names = TRUE)

par(mar = c(10, 5, 1, 1))
boxplot(formula = cr_severity~date_environment, data = all_results, las = 2, ylab = "observed diease severity")
par(mar = c(5.1, 4.1, 4.1, 2.1))

#Can the image data from one time point be used to accurately estimate disease severity within the same time point?

#produce result data
within_date_results <- data.frame(date_environments = unique(all_results$date_environment),
                                  r2 = NA,
                                  MAE = NA,
                                  RMSE = NA,
                                  cor = NA,
                                  var_obs = NA)

#loop through days and print results
for (i in 1:nrow(within_date_results)){
  
  # select sing time point
  single_day <- all_results[which(all_results$date_environment == within_date_results$date_environments[i]),]
  
  # set modeling parameters
  objControl <- trainControl(method = "cv", number = 3)
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  
  # train model
  objModel <- train(cr_severity~mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                    data = single_day,
                    method='rf', 
                    trControl=objControl)
  
  # end training
  stopCluster(cl)
  
  within_date_results$r2[i] <- caret::R2(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE, formula = 'corr')
  within_date_results$MAE[i] <- caret::MAE(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE)
  within_date_results$RMSE[i] <- caret::RMSE(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE)
  within_date_results$cor[i] <- cor(x = objModel$finalModel$y, y = objModel$finalModel$predicted)
  within_date_results$var_obs[i] <- var(x = objModel$finalModel$y)
  
  
}

# add days after planting data to result table
within_date_results$date <- as.Date(sub(pattern = ".*\ ", replacement = "", x = within_date_results$date_environments))
within_date_results$environment <- sub(pattern = "\ .*", replacement = "", x = within_date_results$date_environments)
within_date_results$planting_date <- as.Date(plyr::mapvalues(x = within_date_results$environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
within_date_results$days_after_planting <- within_date_results$date - within_date_results$planting_date
write.csv(x = within_date_results, file = "results/within_date_results.csv", row.names = FALSE)

within_2017_early <- within_date_results[which(within_date_results$environment == "2017_early"),]
within_2017_early <- within_2017_early[order(within_2017_early$date),]
within_2017_late <- within_date_results[which(within_date_results$environment == "2017_late"),]
within_2017_late <- within_2017_late[order(within_2017_late$date),]
within_2018 <- within_date_results[which(within_date_results$environment == "2018"),]
within_2018 <- within_2018[order(within_2018$date),]

#plot R2s
par(mar = c(5.1, 4.1, 1, 1))
plot(x = within_2017_early$days_after_planting, y = within_2017_early$r2,
     xlab = "Days after planting",
     ylab = parse(text = "R^2"),
     xlim = c(30, 67),
     ylim = c(0, 0.39),
     col = "red", pch = 16)
lines(x = within_2017_early$days_after_planting, y = within_2017_early$r2, type = "l", col = "red")
points(x = within_2017_late$days_after_planting, y = within_2017_late$r2, col = "green", pch = 16)
lines(x = within_2017_late$days_after_planting, y = within_2017_late$r2, type = "l", col = "green")
points(x = within_2018$days_after_planting, y = within_2018$r2, col = "blue", pch = 16)
lines(x = within_2018$days_after_planting, y = within_2018$r2, type = "l", col = "blue")
legend(x = 52, y = 0.4, legend = c("2017 early", "2017 late", "2018"), pch = 16, col = c("red", "blue", "green"), lty = 1, bty = "n")
par(mar = c(5.1, 4.1, 4.1, 2.1))

#plot correlations
par(mar = c(5.1, 4.1, 1, 1))
plot(x = within_2017_early$days_after_planting, y = within_2017_early$cor,
     xlab = "Days after planting",
     ylab = "Pearson correlation coefficient",
     xlim = c(30, 67),
     ylim = c(-0.05, 0.65),
     col = "red", pch = 16)
lines(x = within_2017_early$days_after_planting, y = within_2017_early$cor, type = "l", col = "red")
points(x = within_2017_late$days_after_planting, y = within_2017_late$cor, col = "green", pch = 16)
lines(x = within_2017_late$days_after_planting, y = within_2017_late$cor, type = "l", col = "green")
points(x = within_2018$days_after_planting, y = within_2018$cor, col = "blue", pch = 16)
lines(x = within_2018$days_after_planting, y = within_2018$cor, type = "l", col = "blue")
legend(x = 52, y = 0.675, legend = c("2017 early", "2017 late", "2018"), pch = 16, col = c("red", "blue", "green"), lty = 1, bty = "n")
par(mar = c(5.1, 4.1, 4.1, 2.1))

####################################
#Can the image data from one time point be used to accurately estimate disease severity from another time point?

# function given training and testing data sets will report results
predict_severity <- function(test_df, train_df){
  
  #set up training parameters
  objControl <- trainControl(method='cv', number = 3)
  
  #start multi-core cluster
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  
  #train
  objModel <- train(cr_severity~mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                    data = train_df,
                    method='rf', 
                    trControl=objControl)
  
  #stop multi-core cluster
  stopCluster(cl)
  
  #test model
  predictions <- predict(object=objModel$finalModel, newdata = test_df[,c("mean_red",
                                                                          "mean_green",
                                                                          "mean_blue",
                                                                          "mean_725nm",
                                                                          "mean_800nm")])
  
  # report results
  return(c(caret::R2(pred = predictions, obs = test_df$cr_severity, na.rm = TRUE, formula = 'corr'),
           caret::MAE(pred = predictions, obs = test_df$cr_severity, na.rm = TRUE),
           caret::RMSE(pred = predictions, obs = test_df$cr_severity, na.rm = TRUE),
           cor(x = test_df$cr_severity, y = predictions)))
  
}

# produce results table
all_combos <- expand.grid(unique(all_results$date_environment), unique(all_results$date_environment))
names(all_combos) <- c("train", "test")
all_combos$r2 <- NA
all_combos$mae <- NA
all_combos$rmse <- NA
all_combos$cor <- NA

#cycle through combinations and print results to result table
for (i in 1:nrow(all_combos)){
  
  temp <- predict_severity(train_df = all_results[which(all_results$date_environment == all_combos[i,1]),],
                           test_df = all_results[which(all_results$date_environment == all_combos[i,2]),])
  
  all_combos$r2[i] <- temp[1]
  all_combos$mae[i] <- temp[2]
  all_combos$rmse[i] <- temp[3]
  all_combos$cor[i] <- temp[4]
  
}

# save results
write.csv(x = all_combos, file = "results/all_day_combinations.csv", row.names = FALSE)
#all_combos <- read.csv(file = "results/all_day_combinations.csv")

# add descriptive data to combo results
all_combos$train_environment <- sub(pattern = "\ .*", replacement = "", x = all_combos$train)
all_combos$test_environment <- sub(pattern = "\ .*", replacement = "", x = all_combos$test)
all_combos$train_planting_date <- as.Date(plyr::mapvalues(x = all_combos$train_environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
all_combos$test_planting_date <- as.Date(plyr::mapvalues(x = all_combos$test_environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
all_combos$train_date <- as.Date(sub(pattern = ".*\ ", replacement = "", x = all_combos$train))
all_combos$test_date <- as.Date(sub(pattern = ".*\ ", replacement = "", x = all_combos$test))
all_combos$train_days_after_planting <- all_combos$train_date - all_combos$train_planting_date
all_combos$test_days_after_planting <- all_combos$test_date - all_combos$test_planting_date
all_combos$diff_days_after_planting <- all_combos$test_days_after_planting-all_combos$train_days_after_planting

# remove combinations with same time point as training and testing set
all_combos <- subset(all_combos, all_combos$train != all_combos$test)

#plot R2s
plot(x = all_combos$diff_days_after_planting, y = all_combos$r2, xlab = "Difference in days after planting (days)", ylab = parse(text = 'R^2'))

#plot cors
plot(x = all_combos$diff_days_after_planting, y = all_combos$cor, xlab = "Difference in days after planting (days)", ylab = "cor")

#plot MAEs
plot(x = new_all_combos$diff_days_after_planting, y = new_all_combos$mae, xlab = "Difference in days after planting (days)", ylab = "MAE")


#######################################

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
lm(objModel$finalModel$y~objModel$finalModel$predicted)$coefficients[1]
lm(objModel$finalModel$y~objModel$finalModel$predicted)$coefficients[2]
sink()

# print boxplots of observed and predicted values
png(filename = "figures/cross_validation_boxplot.png",
     width = 2000, height = 2400, units = "px", pointsize = 50)
par(mfrow=c(2,1))
par(mar=c(5.1, 4.1, 1.2, 1))
boxplot(objModel$finalModel$predicted~objModel$finalModel$y,
        xlab = "Observed disease severity",
        ylab = "Predicted disease severity",
        lwd = 3)
boxplot(objModel$finalModel$predicted-objModel$finalModel$y~objModel$finalModel$y,
        xlab = "Observed disease severity",
        ylab = "Predicted - observed disease severity",
        lwd = 3)
dev.off()

# print histogram of single observation date MAEs
png(filename = "figures/cross_validation_environment_maes.png",
    width = 1100, height = 800, units = "px", pointsize = 50)
par(mar=c(5.1, 4.1, 1, 1), lwd=3)
hist(objModel$resample$MAE,
     xlab = "MAE",
     ylab = "Count of observations",
     main = NULL,
     lwd = 3)
dev.off()

# print histogram of single observation date R2s
png(filename = "figures/cross_validation_environment_r2s.png",
    width = 1100, height = 800, units = "px", pointsize = 50)
par(mar=c(5.1, 4.1, 1, 1), lwd=3)
hist(objModel$resample$Rsquared,
     xlab = parse(text = 'R^2'),
     ylab = "Count of observations",
     main = NULL,
     lwd = 3)
dev.off()

within_date_results <- data.frame(date_environments = unique(all_results$date_environment),
                                  r2 = NA,
                                  MAE = NA,
                                  RMSE = NA,
                                  cor = NA)

for (i in 10:nrow(within_date_results)){
  
  single_day <- all_results[which(all_results$date_environment == within_date_results$date_environments[i]),]
  
  # set modeling parameters
  objControl <- trainControl(method = "cv", number = 5)
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  
  # train model
  objModel <- train(cr_severity~mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                    data = single_day,
                    method='rf', 
                    trControl=objControl)
  
  # end training
  stopCluster(cl)
  
  within_date_results$r2[i] <- caret::R2(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE, formula = 'corr')
  within_date_results$MAE[i] <- caret::MAE(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE)
  within_date_results$RMSE[i] <- caret::RMSE(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y, na.rm = TRUE)
  within_date_results$cor[i] <- cor(x = objModel$finalModel$y, y = objModel$finalModel$predicted)
  
}

within_date_results$date <- as.Date(substr(x = within_date_results$date_environment, start = 1, stop = 11))
within_date_results$environment <- substr(x = within_date_results$date_environment, start = 11, stop = nchar(as.character(within_date_results$date_environment)))
within_date_results$planting_date <- as.Date(plyr::mapvalues(x = within_date_results$environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
within_date_results$days_after_planting <- within_date_results$date - within_date_results$planting_date

plot(x = within_date_results$days_after_planting, y = within_date_results$r2, xlab = "Days after planting", ylab = parse(text = 'R^2'))
within_date_results <- within_date_results[order(-within_date_results$r2),]
with(within_date_results[1:5,], text(r2~days_after_planting, labels = paste(environment, date), pos = 4))

plot(x = within_date_results$days_after_planting, y = within_date_results$cor, xlab = "Days after planting", ylab = "Pearson correlation")


all_results$prediction <- objModel$finalModel$predicted
temp <- all_results[which(all_results$date_environment == "2017-06-232017_late"),]
plot(x = all_results$cr_severity, y = all_results$prediction)
confusionMatrix(data = objModel,norm = "none" )
confusionMatrix(data = objModel,norm = "overall" )
confusionMatrix(data = objModel,norm = "average" )

confusionMatrix(data = all_results$prediction,
                reference = all_results$cr_severity)

temp <- all_results[which(all_results$date_environment == "2017-06-232017_late"),]

all_results[which(all_results$line == "MNBT1021-1"),]$cr_severity
all_results[which(all_results$line == "MNBT1021-1"),]$prediction



confusionMatrix(data = all_results$prediction,
                reference = all_results$cr_severity)

irr::kappa2(ratings = data.frame(prediction = all_results$prediction,
                                 reference = all_results$cr_severity),
            weight = "squared")
hist(as.numeric(as.character(all_results$cr_severity)))
plot(x = as.numeric(as.character(all_results$cr_severity)), y = all_results$mean_725nm)
plot(x = as.numeric(as.character(all_results$cr_severity)), y = all_results$mean_800nm)
plot(x = as.numeric(as.character(all_results$cr_severity)), y = all_results$mean_red)
plot(x = as.numeric(as.character(all_results$cr_severity)), y = all_results$mean_green)
plot(x = as.numeric(as.character(all_results$cr_severity)), y = all_results$mean_blue)

objModel$finalModel$confusion

# function given training and testing data sets will report results
predict_severity <- function(test_df, train_df){
  
  objControl <- trainControl(method='cv', number = 3)
  
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  objModel <- train(cr_severity~mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                    data = train_df,
                    method='rf', 
                    trControl=objControl)
  stopCluster(cl)
  # summarize results
  predictions <- predict(object=objModel$finalModel, newdata = test_df[,c("mean_red",
                                                                          "mean_green",
                                                                          "mean_blue",
                                                                          "mean_725nm",
                                                                          "mean_800nm")])
  
  
  
  
  
  return(c(caret::R2(pred = predictions, obs = test_df$cr_severity, na.rm = TRUE, formula = 'corr'),
           caret::MAE(pred = predictions, obs = test_df$cr_severity, na.rm = TRUE),
           caret::RMSE(pred = predictions, obs = test_df$cr_severity, na.rm = TRUE)))
  
}

all_combos <- expand.grid(unique(all_results$date_environment), unique(all_results$date_environment))
names(all_combos) <- c("train", "test")
all_combos$r2 <- NA
all_combos$mae <- NA
all_combos$rmse <- NA

for (i in 1:nrow(all_combos)){
  
  temp <- predict_severity(train_df = all_results[which(all_results$date_environment == all_combos[i,1]),],
                           test_df = all_results[which(all_results$date_environment == all_combos[i,2]),])
  
  all_combos$r2[i] <- temp[1]
  all_combos$mae[i] <- temp[2]
  all_combos$rmse[i] <- temp[3]
  
}

write.csv(x = all_combos, file = "results/all_day_combinations.csv", row.names = FALSE)

best_days <- all_results[which(all_results$date_environment %in% c("2017-06-232017_early",
                                                                   "2017-06-212017_early",
                                                                   "2017-07-032017_late",
                                                                   "2018-06-222018",
                                                                   "2018-06-252018")),]

# set folds to be observation dates
n_date_environment_combos <- length(unique(best_days$date_environment))
folds <- groupKFold(group = best_days$date_environment, k = n_date_environment_combos)

# set modeling parameters
objControl <- trainControl(method = "cv", number = n_date_environment_combos, index = folds)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# train model
objModel <- train(cr_severity~mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                  data = best_days,
                  method='rf', 
                  trControl=objControl)

# end training
stopCluster(cl)

objModel$finalModel$y
objModel$finalModel$predicted
cor.test(x = objModel$finalModel$y, y = objModel$finalModel$predicted)
cor(x = objModel$finalModel$y, y = objModel$finalModel$predicted)
caret::R2(pred = objModel$finalModel$predicted, obs = objModel$finalModel$y)
plot(x = objModel$finalModel$y, y = objModel$finalModel$predicted)
best_days$predicted <- objModel$finalModel$predicted
summary(lm(formula = cr_severity ~ predicted, data = best_days))

unique(all_results$date_environment)
combos_environments <- expand.grid(unique(x = all_results$environment), unique(x = all_results$environment))

names(combos_environments) <- c("train", "test")
combos_environments$r2 <- NA
combos_environments$mae <- NA
combos_environments$rmse <- NA

for (i in 1:nrow(combos_environments)){
  
  temp <- predict_severity(train_df = all_results[which(all_results$environment == combos_environments[i,1]),],
                           test_df = all_results[which(all_results$environment == combos_environments[i,2]),])
  
  combos_environments$r2[i] <- temp[1]
  combos_environments$mae[i] <- temp[2]
  combos_environments$rmse[i] <- temp[3]
  
}

all_combos$train_environment <- substr(all_combos$train, start = 11, stop = nchar(as.character(all_combos$train)))
all_combos$test_environment <- substr(all_combos$test, start = 11, stop = nchar(as.character(all_combos$test)))
all_combos$train_planting_date <- as.Date(plyr::mapvalues(x = all_combos$train_environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
all_combos$test_planting_date <- as.Date(plyr::mapvalues(x = all_combos$test_environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
all_combos$train_date <- as.Date(substr(x = all_combos$train, start = 1, stop = 11))
all_combos$test_date <- as.Date(substr(x = all_combos$test, start = 1, stop = 11))
all_combos$train_days_after_planting <- all_combos$train_date - all_combos$train_planting_date
all_combos$test_days_after_planting <- all_combos$test_date - all_combos$test_planting_date
all_combos$diff_days_after_planting <- all_combos$test_days_after_planting-all_combos$train_days_after_planting
new_all_combos <- subset(all_combos, all_combos$train != all_combos$test)
plot(x = new_all_combos$diff_days_after_planting, y = new_all_combos$r2, xlab = "Difference in days after planting (days)", ylab = parse(text = 'R^2'))
plot(x = new_all_combos$diff_days_after_planting, y = new_all_combos$mae, xlab = "Difference in days after planting (days)", ylab = "MAE")

####weather
weather_2017 <- isd(usaf = "726580", wban = "14922", year = 2017, token = "ldLIXqVAfJfRiAIRxdHOhhbyAMXRFoVM")
weather_2018 <- isd(usaf = "726580", wban = "14922", year = 2018, token = "ldLIXqVAfJfRiAIRxdHOhhbyAMXRFoVM")

convert_date_to_gdd_format <- function(date){
  return(as.numeric(gsub(pattern = "-", replacement = "", x = date)))
}

calculate_gdd <- function(begin_date, end_date, isd_data){
  begin_date <- convert_date_to_gdd_format(date = begin_date)
  end_date <- convert_date_to_gdd_format(date = end_date)
  isd_data <- isd_data[which(isd_data$date > begin_date & isd_data$date > end_date),]
  GDD(HourTemp = isd_data$temperature, summ = T, Tbase = 5)
}

calculate_gdd(begin_date = as.Date("2017-04-15"), end_date = as.Date("2017-05-15"), isd_data = rbind(x = weather_2017[,c("date", "time", "temperature")], y = weather_2018[,c("date", "time", "temperature")]))

all_combos$train_gdd <- max(GDD(HourTemp = as.numeric(weather_2017[which(as.numeric(weather_2017$date) > 20170515 & as.numeric(weather_2017$date) < 20170615),]$temperature),
    summ = T, Tbase = 5))
date_environment_conversions <- data.frame(date_environment = unique(all_results$date_environment),
                                           date = as.Date(substr(x = unique(all_results$date_environment), start = 1, stop = 11)),
                                           environment = substr(x = unique(all_results$date_environment), start = 11, stop = nchar(unique(all_results$date_environment))))



environment_pairs_r2 <- matrix(data = NA,
                               nrow = length(unique(x = all_results$environment)),
                               ncol = length(unique(x = all_results$environment)))
rownames(environment_pairs_r2) <- unique(x = all_results$environment)
colnames(environment_pairs_r2) <- unique(x = all_results$environment)
environment_pairs_mae <- environment_pairs_r2
environment_pairs_rmse <- environment_pairs_r2

for (c in 1:ncol(environment_pairs_r2)){
  
  for (r in 1:nrow(environment_pairs_r2)){
    
    temp <- predict_severity(train_df = all_results[which(all_results$environment == colnames(environment_pairs_r2)[c]),],
                             test_df = all_results[which(all_results$environment == colnames(environment_pairs_r2)[r]),])
    
    environment_pairs_r2[r, c] <- temp[1]
    environment_pairs_mae[r, c] <- temp[2]
    environment_pairs_rmse[r, c] <- temp[3]
    
  }
  
}




date_environment_conversions$days_after_planting <- date_environment_conversions$planting_date - date_environment_conversions$date
