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
all_results$planting_date <- as.Date(plyr::mapvalues(x = all_results$environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
all_results$date <- as.Date(all_results$date)
all_results$fact_date <- as.factor(all_results$date)
all_results$days_after_planting <- all_results$date - all_results$planting_date
all_results$environment_color <- as.character(plyr::mapvalues(x = all_results$environment, from = c("2017_early", "2017_late", "2018"), to = c("red", "green", "blue")))


#analysis of variance

#train model treating date a a categorical and continuous variable
categorical_model <- lm(formula = cr_severity~environment*fact_date%in%environment, data = all_results)
continuous_model <- lm(formula = cr_severity~environment*days_after_planting%in%environment, data = all_results)

#compare models
anova(categorical_model, continuous_model)
#categorical model explain much more variance
anova_result <- anova(categorical_model)

#environment f-test
anova_result$`F value`[1] = anova_result$`Mean Sq`[1]/anova_result$`Mean Sq`[2]
anova_result$`Pr(>F)`[1] = pf(q = anova_result$`F value`[1], df1 = anova_result$Df[1], df2 = anova_result$Df[2])
#write result
write.csv(x = anova_result, file = "results/simple_anova_table.csv", row.names = TRUE)

#print boxplots of visually observed disease severity
pdf(file = "figures/distribution_vods.pdf", width = 12, height = 12)
par(mar = c(11, 5, 1, 1))
boxplot(formula = cr_severity~date_environment,
        data = all_results,
        las = 2,
        ylab = "Visually observed disease severity")
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

# within time point cross-validation

#produce result data
within_date_results <- data.frame(date_environments = unique(all_results$date_environment),
                                  r2 = NA,
                                  MAE = NA,
                                  RMSE = NA,
                                  cor = NA,
                                  var_obs = NA)

#initiate empty result table
all_results_within_date <- all_results
all_results_within_date$estimate <- NA
all_results_within_date <- all_results_within_date[NULL,]

#loop through days and print results
for (i in 1:nrow(within_date_results)){
  
  # select sing time point
  single_day <- all_results[which(all_results$date_environment == within_date_results$date_environments[i]),]
  
  # set modeling parameters
  objControl <- trainControl(method = "repeatedcv", number = 5, repeats = 40)
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
  within_date_results$cor[i] <- sqrt(max(objModel$results$Rsquared))
  within_date_results$var_obs[i] <- var(x = objModel$finalModel$y)
  sqrt(max(objModel$results$Rsquared))
  
  single_day$estimate <- objModel$finalModel$predicted
  
  all_results_within_date <- rbind(all_results_within_date, single_day)
  
}

# add days after planting data to result table
within_date_results$date <- as.Date(sub(pattern = ".*\ ", replacement = "", x = within_date_results$date_environments))
within_date_results$environment <- sub(pattern = "\ .*", replacement = "", x = within_date_results$date_environments)
within_date_results$planting_date <- as.Date(plyr::mapvalues(x = within_date_results$environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
within_date_results$days_after_planting <- within_date_results$date - within_date_results$planting_date
#write.csv(x = within_date_results, file = "results/within_date_results.csv", row.names = FALSE)
within_date_results <- read.csv(file = "results/within_date_results.csv")
library(RColorBrewer)
colorr
plot(x = all_results_within_date[which(all_results_within_date$environment == "2017_early"),]$cr_severity,
     y = all_results_within_date[which(all_results_within_date$environment == "2017_early"),]$estimate,
     col = factor(all_results_within_date[which(all_results_within_date$environment == "2017_early"),]$date_environment))

plot(x = all_results_within_date[which(all_results_within_date$environment == "2017_late"),]$cr_severity,
     y = all_results_within_date[which(all_results_within_date$environment == "2017_late"),]$estimate,
     col = factor(all_results_within_date[which(all_results_within_date$environment == "2017_late"),]$date_environment))

plot(x = all_results_within_date[which(all_results_within_date$environment == "2018"),]$cr_severity,
     y = all_results_within_date[which(all_results_within_date$environment == "2018"),]$estimate,
     col = factor(all_results_within_date[which(all_results_within_date$environment == "2018"),]$date_environment))

within_2017_early <- within_date_results[which(within_date_results$environment == "2017_early"),]
within_2017_early <- within_2017_early[order(within_2017_early$date),]
within_2017_late <- within_date_results[which(within_date_results$environment == "2017_late"),]
within_2017_late <- within_2017_late[order(within_2017_late$date),]
within_2018 <- within_date_results[which(within_date_results$environment == "2018"),]
within_2018 <- within_2018[order(within_2018$date),]

#boxplots by environment
mean(within_date_results[which(within_date_results$environment == "2017_early"),]$cor)
mean(within_date_results[which(within_date_results$environment == "2017_late"),]$cor)
mean(within_date_results[which(within_date_results$environment == "2018"),]$cor)
mean(within_date_results$cor)

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
legend(x = 52, y = 0.4, legend = c("2017 early", "2017 late", "2018"), pch = 16, col = c("red", "green", "blue"), lty = 1, bty = "n")
par(mar = c(5.1, 4.1, 4.1, 2.1))

#plot correlations
pdf(file = "figures/within_time_point_cv.pdf", width = 6, height = 6)
par(mar = c(5.1, 4.1, 1, 1))
plot(x = within_2017_early$days_after_planting, y = within_2017_early$cor,
     xlab = "Days after planting",
     ylab = parse(text = 'italic(r)[o:e]'),
     xlim = c(30, 67),
     ylim = c(0.1, 0.6),
     col = "red", pch = 16)
lines(x = within_2017_early$days_after_planting, y = within_2017_early$cor, type = "l", col = "red")
points(x = within_2017_late$days_after_planting, y = within_2017_late$cor, col = "green", pch = 16)
lines(x = within_2017_late$days_after_planting, y = within_2017_late$cor, type = "l", col = "green")
points(x = within_2018$days_after_planting, y = within_2018$cor, col = "blue", pch = 16)
lines(x = within_2018$days_after_planting, y = within_2018$cor, type = "l", col = "blue")
legend(x = 57, y = 0.62, legend = c("2017 early", "2017 late", "2018"), pch = 16, col = c("red", "green", "blue"), lty = 1, bty = "n")
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

best_day <- all_results_within_date[which(all_results_within_date$date_environment == "2018 2018-06-22"),]
best_day_fit <- lm(formula = estimate~cr_severity, data = best_day)

## rounded coefficients for better output
cf <- round(coef(best_day_fit), 2) 

## sign check to avoid having plus followed by minus for negative coefficients
eq <- paste0("estimated severity = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " observed severity")

pdf(file = "figures/within_time_2018_06_22_scatterplot.pdf", width = 6, height = 6)
par(mar = c(5.1, 4.1, 2, 1))
plot(x = best_day$cr_severity,
     y = best_day$estimate,
     xlab = "Visually observed disease severity (%)",
     ylab = "Image estimated disease severity (%)")
abline(coef(best_day_fit)[1:2])
## printing of the equation
mtext(eq, 3, line=0)
mtext("environment = 2018, time point = 22-June-2018", 3, line=1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

####################################
#Can the image data from one time point be used to accurately estimate disease severity from another time point?

# function given training and testing data sets will report results
predict_severity <- function(test_df, train_df){
  
  #set up training parameters
  objControl <- trainControl(method='cv', number = 5)
  
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

boxplot(formula = cor~test, data = all_combos)

#plot R2s
plot(x = all_combos$diff_days_after_planting, y = all_combos$r2, xlab = "Difference in days after planting (test-train)", ylab = parse(text = 'R^2'))

#plot cors
plot(x = all_combos$diff_days_after_planting, y = all_combos$cor, xlab = "Difference in days after planting (test-train)", ylab = "Pearson correlation coefficient")

#plot MAEs
plot(x = all_combos$diff_days_after_planting, y = all_combos$mae, xlab = "Difference in days after planting (test-train)", ylab = "MAE")

#######################################
# Leave-one-environment-out cross-validation

# set folds to be observation dates
n_date_environment_combos <- length(unique(all_results$date_environment))
folds <- groupKFold(group = all_results$date_environment, k = n_date_environment_combos)


# set modeling parameters
objControl <- trainControl(method = "repeatedcv", number = n_date_environment_combos, index = folds, repeats = 40)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# train model
objModel <- train(cr_severity~mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                  data = all_results,
                  method='rf', 
                  trControl=objControl)

# end training
stopCluster(cl)

objModel$results
hist(objModel$resample$Rsquared)
hist(sqrt(objModel$resample$Rsquared))
result <- objModel$resample[order(objModel$resample$Resample),]
result$date_environment <- all_results[!duplicated(all_results$date_environment),]$date_environment
result$cor <- sqrt(result$Rsquared)
  
cor_result <- data.frame(date_environment = unique(all_results$date_environment), cor = NA)
for (i in 1:nrow(cor_result)){
  cor_result$cor[i] <- cor(x = all_results[which(all_results$date_environment == cor_result$date_environment[i]),]$cr_sev,
                       y = objModel$finalModel$predicted[which(all_results$date_environment == cor_result$date_environment[i])])
  
}

cor_result$date <- as.Date(sub(pattern = ".*\ ", replacement = "", x = cor_result$date_environment))
cor_result$environment <- sub(pattern = "\ .*", replacement = "", x = cor_result$date_environment)
cor_result$planting_date <- as.Date(plyr::mapvalues(x = cor_result$environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
cor_result$days_after_planting <- cor_result$date - cor_result$planting_date
cor_result$plot_color <- plyr::mapvalues(x = cor_result$environment, from = c("2017_early", "2017_late", "2018"), to = c("red", "green", "blue"))
cor_result <- cor_result[order(cor_result$date_environment),]
write.csv(x = cor_result, file = "results/lotpocv_result.csv")
cor_result <- read.csv(file = "results/lotpocv_result.csv")

#plot correlations
pdf(file = "figures/lotpocv_cv.pdf", width = 6, height = 6)
par(mar = c(5.1, 4.1, 1, 1))
plot(x = cor_result$days_after_planting, y = cor_result$cor,
     xlab = "Days after planting",
     ylab = parse(text = 'italic(r)[o:e]'),
     xlim = c(30, 67),
     ylim = c(-0.1, 0.575),
     col = cor_result$plot_color, pch = 16)
lines(x = cor_result[which(cor_result$environment == "2017_early"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2017_early"),]$cor, type = "l", col = "red")
lines(x = cor_result[which(cor_result$environment == "2017_late"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2017_late"),]$cor, type = "l", col = "green")
lines(x = cor_result[which(cor_result$environment == "2018"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2018"),]$cor, type = "l", col = "blue")
legend(x = 57, y = 0.595, legend = c("2017 early", "2017 late", "2018"), pch = 16, col = c("red", "green", "blue"), lty = 1, bty = "n")
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

cor.test(x = objModel$finalModel$y, y = objModel$finalModel$predicted)
all_results_lotpocv <- all_results
all_results_lotpocv$estimate <- objModel$finalModel$predicted
simple_model <- lm(formula = cr_severity~environment+date%in%environment, data = all_results_lotpocv)
cov_model <- lm(formula = cr_severity~environment+date%in%environment+estimate%in%date%in%environment, data = all_results_lotpocv)
anova(simple_model, cov_model)
193211/(1229139)
sqrt(193211/(1229139))

plot(x = objModel$finalModel$y, y = objModel$finalModel$predicted)

objModel$finalModel$importance

#How does accuracy change if days after planting is added to the model? This would be known in a real word situation.

# set folds to be observation dates
n_date_environment_combos <- length(unique(all_results$date_environment))
folds <- groupKFold(group = all_results$date_environment, k = n_date_environment_combos)

# set modeling parameters
objControl <- trainControl(method = "cv", number = n_date_environment_combos, index = folds)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# train model
objModel <- train(cr_severity~days_after_planting+mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                  data = all_results,
                  method='rf', 
                  trControl=objControl)

# end training
stopCluster(cl)

objModel$finalModel$importance

cor_result <- data.frame(date_environment = unique(all_results$date_environment), cor = NA)
for (i in 1:nrow(cor_result)){
  cor_result$cor[i] <- cor(x = all_results[which(all_results$date_environment == cor_result$date_environment[i]),]$cr_sev,
                           y = objModel$finalModel$predicted[which(all_results$date_environment == cor_result$date_environment[i])])
  
}

cv_cov_all_results <- all_results
cv_cov_all_results$predictions <- objModel$finalModel$predicted

cor.test(x = cv_cov_all_results$cr_severity,y = cv_cov_all_results$predictions)

cor_result$date <- as.Date(sub(pattern = ".*\ ", replacement = "", x = cor_result$date_environment))
cor_result$environment <- sub(pattern = "\ .*", replacement = "", x = cor_result$date_environment)
cor_result$planting_date <- as.Date(plyr::mapvalues(x = cor_result$environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
cor_result$days_after_planting <- cor_result$date - cor_result$planting_date
cor_result$plot_color <- plyr::mapvalues(x = cor_result$environment, from = c("2017_early", "2017_late", "2018"), to = c("red", "green", "blue"))
cor_result <- cor_result[order(cor_result$date_environment),]

#plot correlations
par(mar = c(5.1, 4.1, 1, 1))
plot(x = cor_result$days_after_planting, y = cor_result$cor,
     xlab = "Days after planting",
     ylab = "Pearson correlation coefficient",
     xlim = c(30, 67),
     ylim = c(-0.15, 0.65),
     col = cor_result$plot_color, pch = 16)
lines(x = cor_result[which(cor_result$environment == "2017_early"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2017_early"),]$cor, type = "l", col = "red")
lines(x = cor_result[which(cor_result$environment == "2017_late"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2017_late"),]$cor, type = "l", col = "green")
lines(x = cor_result[which(cor_result$environment == "2018"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2018"),]$cor, type = "l", col = "blue")
legend(x = 52, y = 0.675, legend = c("2017 early", "2017 late", "2018"), pch = 16, col = c("red", "green", "blue"), lty = 1, bty = "n")
par(mar = c(5.1, 4.1, 4.1, 2.1))

# Can the data from one environment be used to estimate the disease severity from another environment?

predict_severity2 <- function(test_df, train_df){
  
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
  
  cor_result <- data.frame(date_environment = unique(test_df$date_environment), cor = NA)
  for (i in 1:nrow(cor_result)){
    cor_result$cor[i] <- cor(x = test_df[which(test_df$date_environment == cor_result$date_environment[i]),]$cr_sev,
                             y = predictions[which(test_df$date_environment == cor_result$date_environment[i])])
    
  }
  
  cor_result <- cor_result[order(cor_result$date_environment),]
  
  # report results
  return(cor_result)
  
}

combos_environments <- expand.grid(unique(x = all_results$environment), unique(x = all_results$environment))

names(combos_environments) <- c("train", "test")
combos_environments <- combos_environments[-which(combos_environments$train == combos_environments$test),]
#combos_environments$r2 <- NA
#combos_environments$mae <- NA
#combos_environments$rmse <- NA
#combos_environments$cor <- NA

predictions_list <- list(NA, 6)

for (i in 1:6){
  
  predictions_list[[i]] <- predict_severity2(train_df = all_results[which(all_results$environment == combos_environments[i,1]),],
                            test_df = all_results[which(all_results$environment == combos_environments[i,2]),])
  
}

pdf(file = "figures/among_environment_cv.pdf", width = 6, height = 10)
par(mfrow=c(3,1))
par(mar=c(4.5,5,1,1))

temp <- predictions_list[[1]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

plot(x = temp$date_environment, y = temp$cor, col = "red", xlab = "Testing set time point", ylab = parse(text = 'italic(r)[o:e]'), ylim = c(-0.125, 0.375), pch = 16, cex.lab = 1.5, cex.axis = 1.5)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "red")

temp <- predictions_list[[2]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

points(x = temp$date_environment, y = temp$cor, col = "blue", pch = 16)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "blue")

text(x = min(temp$date_environment)+3, y = 0.35, labels = "2017 early", cex = 2)
legend(x = max(temp$date_environment)-7.5, y = -0.025, col = c("red", "blue"), lty = 1, bty = "n", pch = 16, legend = c("2017 late", "2018"), cex = 1.5)

temp <- predictions_list[[3]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

plot(x = temp$date_environment, y = temp$cor, col = "red", xlab = "Testing set time point", ylab = parse(text = 'italic(r)[o:e]'), ylim = c(-0.125, 0.375), pch = 16, cex.lab = 1.5, cex.axis = 1.5)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "red")

temp <- predictions_list[[4]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

text(x = min(temp$date_environment)+1.5, y = 0.35, labels = "2017 late", cex = 2)
legend(x = max(temp$date_environment)-4.75, y = -0.025, col = c("red", "blue"), lty = 1, bty = "n", pch = 16, legend = c("2017 early", "2018"), cex = 1.5)

points(x = temp$date_environment, y = temp$cor, col = "blue", pch = 16)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "blue")

temp <- predictions_list[[5]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

plot(x = temp$date_environment, y = temp$cor, col = "red", xlab = "Testing set time point", ylab = parse(text = 'italic(r)[o:e]'), ylim = c(-0.125, 0.445), pch = 16, cex.lab = 1.5, cex.axis = 1.5)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "red")

temp <- predictions_list[[6]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

points(x = temp$date_environment, y = temp$cor, col = "blue", pch = 16)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "blue")

text(x = min(temp$date_environment)+0.5, y = 0.415, labels = "2018", cex = 2)
legend(x = max(temp$date_environment)-3, y = 0.000, col = c("red", "blue"), lty = 1, bty = "n", pch = 16, legend = c("2017 early", "2017 late"), cex = 1.5)

dev.off()

#############################

library(rrBLUP)
library(qvalue)
#setwd("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2019_plant_genome_cr_gwas/")
source("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2019_plant_genome_cr_gwas/scripts/functions.R")
genotypes <- read.csv("../../2019_genotype_results/founder_genotypes_17mar2019_marker_defs_imputed.csv")
phenotypes <- data.frame(line = all_results$line, estimated_severity = objModel$finalModel$predicted)
phenotypes <- phenotypes[which(all_results$date_environment == "2018 2018-06-22"),]
effective_markers_threshold <- -log10(0.05/2231)

rrBLUP_manhattan <- function(input, fdr.level = 0.05, text) {
  input <- input[order(input[, 2], input[, 3]), ]
  chroms <- unique(input[, 2])
  n.chrom <- length(chroms)
  chrom.start <- rep(0, n.chrom)
  chrom.mid <- rep(0, n.chrom)
  if (n.chrom > 1) {
    for (i in 1:(n.chrom - 1)) {
      chrom.start[i + 1] <- chrom.start[i] + max(input[which(input[, 
                                                                   2] == chroms[i]), 3]) + 1
    }
  }
  x.max <- chrom.start[n.chrom] + max(input[which(input[, 
                                                        2] == chroms[n.chrom]), 3])
  plot(0, 0, type = "n", xlim = c(0, x.max), ylim = c(0, 
                                                      7), ylab = "-log(p)", xlab = text, 
       xaxt = "n")
  for (i in seq(1, n.chrom, by = 2)) {
    ix <- which(input[, 2] == chroms[i])
    chrom.mid[i] <- median(chrom.start[i] + input[ix, 
                                                  3])
    points(chrom.start[i] + input[ix, 3], input[ix, 4], 
           col = "dark blue", pch = 16)
  }
  if (n.chrom > 1) {
    for (i in seq(2, n.chrom, by = 2)) {
      ix <- which(input[, 2] == chroms[i])
      chrom.mid[i] <- median(chrom.start[i] + input[ix, 
                                                    3])
      points(chrom.start[i] + input[ix, 3], input[ix, 
                                                  4], col = "cornflowerblue", pch = 16)
    }
  }
  q.ans <- qvalue(10^-input[, 4])
  temp <- cbind(q.ans$qvalues, input[, 4])
  temp <- temp[order(temp[, 1]), ]
  #  if (temp[1, 1] < fdr.level) {
  #    temp2 <- tapply(temp[, 2], temp[, 1], mean)
  #    qvals <- as.numeric(rownames(temp2))
  #    x <- which.min(abs(qvals - fdr.level))
  #    first <- max(1, x - 2)
  #    last <- min(x + 2, length(qvals))
  #    if ((last - first) < 4) {
  #      last <- first + 3
  #    }
  #    splin <- smooth.spline(x = qvals[first:last], y = temp2[first:last], 
  #                           df = 3)
  #    lines(x = c(0, x.max), y = rep(predict(splin, x = fdr.level)$y, 
  #                                   2), lty = 2)
  #  }
  axis(side = 1, at = chrom.mid, labels = chroms)
}

data <- prepare_geno_pheno_gwas(geno = genotypes,
                                pheno = phenotypes,
                                min.MAF = 0.05,
                                max.missing = 0.1,
                                n.core = 4)

model <- GWAS(pheno = data[[2]],
              geno = data[[1]],
              plot = FALSE,
              n.core = 4,
              n.PC = 0)

png(filename = paste0("figures/gwas_qq_2017_early_2017_06_23.png"),
    width = 1000,
    height = 1000,
    pointsize = 50)
par(mar=c(5, 5, 1, 1))
qqman::qq(10^-model$estimated_severity)
dev.off()

png(filename = paste0("figures/gwas_manhattan_2017_early_2017_06_23.png"),
    width = 2000,
    height = 1000,
    pointsize = 50)
par(mar=c(4.1,4.1,1,1))
rrBLUP_manhattan(input = model, fdr.level = 0.05, text = "Linkage group")
abline(h = effective_markers_threshold, lty = 2, lwd = 5, col = "red")
legend("topleft", inset = 0, title="Significance threshold",
       c("Modified Bonferroni"),
       lty=2, lwd = 5, col = c("red"),
       horiz=FALSE, cex = 0.5)
dev.off()

#MANCOVA
cv_all_results <- all_results
cv_all_results$predictions <- objModel$finalModel$predicted

#train model with predicted covariate
categorical_cov_model <- lm(formula = cr_severity~environment*fact_date%in%environment+predictions%in%fact_date%in%environment, data = cv_all_results)

#compare covariate model to simple model
anova(categorical_model, categorical_cov_model)

69503/988955

cor.test(x = cv_all_results$cr_severity,y = cv_all_results$predictions)
hist(cor_result)

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
