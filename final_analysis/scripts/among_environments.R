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
  date_environment_combos <- data.frame(date = unique(results[, c("date")]))
  date_environment_combos$slope <- NA
  date_environment_combos$intercept <- NA
  date_environment_combos$r2 <- NA
  
  for (i in 1:nrow(date_environment_combos)){
    temp <- results[which(results$date == date_environment_combos$date[i]),]
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

# train = 2017_early , test = 2017_late
repetitions <- 40

set.seed(3)
regression_slope <- list()
regression_intercept <- list()
regression_r2 <- list()

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2017_early"),]
  test_result <- all_results[which(all_results$environment == "2017_late"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    trainDF[[ii]] <- temp[splitIndex,]
  }
  
  all_results_split <- split(x = test_result, f = list(test_result$date, test_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    testDF[[ii]] <- temp[splitIndex,]
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))

# train = 2017_early , test = 2018

set.seed(4)

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2017_early"),]
  test_result <- all_results[which(all_results$environment == "2018"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    trainDF[[ii]] <- temp[splitIndex,]
  }
  
  all_results_split <- split(x = test_result, f = list(test_result$date, test_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    testDF[[ii]] <- temp[splitIndex,]
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))

# train = 2017_late , test = 2017_early

set.seed(5)

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2017_late"),]
  test_result <- all_results[which(all_results$environment == "2017_early"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    trainDF[[ii]] <- temp[splitIndex,]
  }
  
  all_results_split <- split(x = test_result, f = list(test_result$date, test_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    testDF[[ii]] <- temp[splitIndex,]
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))

# train = 2017_late , test = 2018

set.seed(6)

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2017_late"),]
  test_result <- all_results[which(all_results$environment == "2018"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    trainDF[[ii]] <- temp[splitIndex,]
  }
  
  all_results_split <- split(x = test_result, f = list(test_result$date, test_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    testDF[[ii]] <- temp[splitIndex,]
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))

# train = 2018 , test = 2017_early

set.seed(7)

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2018"),]
  test_result <- all_results[which(all_results$environment == "2017_early"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    trainDF[[ii]] <- temp[splitIndex,]
  }
  
  all_results_split <- split(x = test_result, f = list(test_result$date, test_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    testDF[[ii]] <- temp[splitIndex,]
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))

# train = 2018 , test = 2017_late

set.seed(8)

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2018"),]
  test_result <- all_results[which(all_results$environment == "2017_late"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    trainDF[[ii]] <- temp[splitIndex,]
  }
  
  all_results_split <- split(x = test_result, f = list(test_result$date, test_result$environment), drop = TRUE)
  
  for (ii in 1:length(all_results_split)){
    temp <- all_results_split[[ii]]
    splitIndex <- sample(x = nrow(temp), size = 100, replace = FALSE)
    testDF[[ii]] <- temp[splitIndex,]
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))

# train = 2017_early , test = 2017_early

set.seed(9)

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2017_early"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))

# train = 2017_late , test = 2017_late

set.seed(10)

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2017_late"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))

# train = 2018 , test = 2018

set.seed(11)

for (i in 1:repetitions){
  
  train_result <- all_results[which(all_results$environment == "2018"),]
  trainDF <- list()
  testDF <- list()
  
  all_results_split <- split(x = train_result, f = list(train_result$date, train_result$environment), drop = TRUE)
  
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

mean(as.numeric(unlist(x = regression_r2)))
hist(as.numeric(unlist(x = regression_r2)))
