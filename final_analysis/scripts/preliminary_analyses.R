setwd("/Users/ianmcnish/Google Drive File Stream/My Drive/projects/2017_btn/final_analysis")

library(sqldf)
library(ggplot2)
library(doParallel)
library(caret)
library(irr)
library(rel)

# read data
all_results <- read.csv(file = "results/image_and_field_data.csv")

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
  results <- data.frame(observed_severity = as.numeric(test_df$cr_severity),
                        predicted_severity = predictions,
                        date = as.factor(test_df$date),
                        difference = test_df$cr_severity - predictions,
                        environment = test_df$environment)
  date_environment_combos <- unique(results[, c("date", "environment")])
  date_environment_combos$slope <- NA
  date_environment_combos$intercept <- NA
  date_environment_combos$r2 <- NA
  
  for (i in 1:nrow(date_environment_combos)){
    temp <- results[which(results$date == date_environment_combos$date[i]),]
    temp <- temp[which(temp$environment == date_environment_combos$environment[i]),]
    lm_obj <- lm(formula = observed_severity ~ predicted_severity, data = temp)
    anova_obj <- anova(lm_obj)
    date_environment_combos$slope[i] <- as.numeric(lm_obj$coefficients[2]) # record slope
    date_environment_combos$intercept[i] <- as.numeric(lm_obj$coefficients[1]) # record intercept
    date_environment_combos$r2[i] <- anova_obj$`Sum Sq`[1]/sum(anova_obj$`Sum Sq`) # record r2
  }

  return_result <- list(date_environment_combos$slope,
                        date_environment_combos$intercept,
                        date_environment_combos$r2)
  
  return(return_result)
  
}

# sample 75% of data for training and 25% for testing, report accuracy and precision, repeat 40 times

set.seed(1)
repetitions <- 1
regression_slope <- list()
regression_intercept <- list()
regression_r2 <- list()

for (i in 1:repetitions){
  
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = all_results, f = list(all_results$date, all_results$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 200, replace = FALSE)
    trainDF[[ii]] <- temp[splitIndex,]
    splitIndex <- sample(x = nrow(trainDF[[ii]]), size = 100, replace = FALSE)
    testDF[[ii]] <- trainDF[[ii]][splitIndex,]
    trainDF[[ii]] <- trainDF[[ii]][-splitIndex,]
  }
  
  trainDF <- do.call(rbind.data.frame, trainDF)
  testDF <- do.call(rbind.data.frame, testDF)
  
  trainDF$cr_severity <- as.numeric(trainDF$cr_severity)
  testDF$cr_severity <- as.numeric(testDF$cr_severity)
  trainDF <- trainDF[,-which(names(trainDF) %in% c("plot_id", "line"))]
  testDF <- testDF[,-which(names(testDF) %in% c("plot_id", "line"))]
  
  result <- predict_severity(test_df = testDF, train_df = trainDF)
  
  regression_slope[i] <- result[1]
  regression_intercept[i] <- result[2]
  regression_r2[i] <- result[3]
  
}

mean(as.numeric(unlist(x = regression_slope)))
hist(as.numeric(unlist(x = regression_slope)))
t.test(as.numeric(unlist(x = regression_slope)))

mean(as.numeric(unlist(x = regression_intercept)))
hist(as.numeric(unlist(x = regression_intercept)))
t.test(as.numeric(unlist(x = regression_intercept)))

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))
t.test(as.numeric(unlist(x = regression_r2)))

# idependently resample every variable with replacement
# sample 75% of data for training and 25% for testing, report accuracy and precision, repeat 40 times

set.seed(2)
repetitions <- 1
regression_slope <- list()
regression_intercept <- list()
regression_r2 <- list()

for (i in 1:repetitions){
  
  rand_results <- data.frame(plot_id = all_results$plot_id,
                             line = all_results$line,
                             cr_severity = sample(x = all_results$cr_severity, size = nrow(all_results), replace = TRUE),
                             date = all_results$date,
                             mean_725nm = sample(x = all_results$mean_725nm, size = nrow(all_results), replace = TRUE),
                             mean_800nm = sample(x = all_results$mean_800nm, size = nrow(all_results), replace = TRUE),
                             mean_red = sample(x = all_results$mean_red, size = nrow(all_results), replace = TRUE),
                             mean_green = sample(x = all_results$mean_green, size = nrow(all_results), replace = TRUE),
                             mean_blue = sample(x = all_results$mean_blue, size = nrow(all_results), replace = TRUE),
                             environment = all_results$environment)
  
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = rand_results, f = list(rand_results$date, rand_results$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 200, replace = FALSE)
    trainDF[[ii]] <- temp[splitIndex,]
    splitIndex <- sample(x = nrow(trainDF[[ii]]), size = 100, replace = FALSE)
    testDF[[ii]] <- trainDF[[ii]][splitIndex,]
    trainDF[[ii]] <- trainDF[[ii]][-splitIndex,]
  }
  
  trainDF <- do.call(rbind.data.frame, trainDF)
  testDF <- do.call(rbind.data.frame, testDF)
  
  trainDF$cr_severity <- as.numeric(trainDF$cr_severity)
  testDF$cr_severity <- as.numeric(testDF$cr_severity)
  trainDF <- trainDF[,-which(names(trainDF) %in% c("plot_id", "line"))]
  testDF <- testDF[,-which(names(testDF) %in% c("plot_id", "line"))]
  
  result <- predict_severity(test_df = testDF, train_df = trainDF)
  
  regression_slope[i] <- result[1]
  regression_intercept[i] <- result[2]
  regression_r2[i] <- result[3]
  
}

mean(as.numeric(unlist(x = regression_slope)))
hist(as.numeric(unlist(x = regression_slope)))
t.test(as.numeric(unlist(x = regression_slope)))

mean(as.numeric(unlist(x = regression_intercept)))
hist(as.numeric(unlist(x = regression_intercept)))
t.test(as.numeric(unlist(x = regression_intercept)))

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))
t.test(as.numeric(unlist(x = regression_r2)))
