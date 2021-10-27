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


# Can the data from one environment be used to estimate the disease severity from another environment?

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
  estimates <- predict(object=objModel$finalModel, newdata = test_df[,c("mean_red",
                                                                        "mean_green",
                                                                        "mean_blue",
                                                                        "mean_725nm",
                                                                        "mean_800nm")])
  
  #append estimates to test df
  test_df$estimates <- estimates
  
  #calculate cor results
  cor_result <- data.frame(date_environment = unique(test_df$date_environment), cor = NA)
  for (i in 1:nrow(cor_result)){
    cor_result$cor[i] <- cor(x = test_df[which(test_df$date_environment == cor_result$date_environment[i]),]$cr_sev,
                             y = estimates[which(test_df$date_environment == cor_result$date_environment[i])])
    
  }
  cor_result <- cor_result[order(cor_result$date_environment),]
  
  # report results
  return(list(test_df, cor_result))
  
}

# produce result table
combos_environments <- expand.grid(unique(x = all_results$environment), unique(x = all_results$environment))
names(combos_environments) <- c("train", "test")
combos_environments <- combos_environments[-which(combos_environments$train == combos_environments$test),]

# produce list to place results
estimates_list <- vector(mode = "list", length = 6)
cor_list <- vector(mode = "list", length = 6)
temp_list <- vector(mode = "list", length = 2)

for (i in 1:6){
  
  temp_list <- predict_severity(train_df = all_results[which(all_results$environment == combos_environments[i,1]),],
                                test_df = all_results[which(all_results$environment == combos_environments[i,2]),])
  estimates_list[[i]] <- temp_list[[1]]
  cor_list[[i]] <- temp_list[[2]]
  
}

#iniatialize result table
among_environments_result_table <- data.frame(test_environment = NA,
                                              train_environment = NA,
                                              simple_rsquared = NA,
                                              simple_rsquared_pvalue = NA,
                                              simple_correlation = NA,
                                              partial_rsquared = NA,
                                              partial_rsquared_pvalue = NA,
                                              partial_correlation = NA)

# test = 2017 early, train = 2017 late
temp <- estimates_list[[1]]
simple_model <- lm(formula = cr_severity~fact_date, data = temp)
covariate_model <- lm(formula = cr_severity~fact_date+estimates%in%fact_date, data = temp)
anova(simple_model, covariate_model)
covariate_model_anova <- anova(covariate_model)
partial_r2 <- covariate_model_anova$`Sum Sq`[2]/(covariate_model_anova$`Sum Sq`[2]+covariate_model_anova$`Sum Sq`[3])
partial_cor <- sqrt(partial_r2)
cor.test(x = temp$cr_severity, y = temp$estimates)
anova(lm(formula = cr_severity~estimates, data = temp))
caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr')
sqrt(caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr'))

# test = 2017 early, train = 2018
temp <- estimates_list[[2]]
simple_model <- lm(formula = cr_severity~fact_date, data = temp)
covariate_model <- lm(formula = cr_severity~fact_date+estimates%in%fact_date, data = temp)
anova(simple_model, covariate_model)
covariate_model_anova <- anova(covariate_model)
partial_r2 <- covariate_model_anova$`Sum Sq`[2]/(covariate_model_anova$`Sum Sq`[2]+covariate_model_anova$`Sum Sq`[3])
partial_cor <- sqrt(partial_r2)
cor.test(x = temp$cr_severity, y = temp$estimates)
anova(lm(formula = cr_severity~estimates, data = temp))
caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr')
sqrt(caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr'))

# test = 2017 late, train = 2017 late
temp <- estimates_list[[3]]
simple_model <- lm(formula = cr_severity~fact_date, data = temp)
covariate_model <- lm(formula = cr_severity~fact_date+estimates%in%fact_date, data = temp)
anova(simple_model, covariate_model)
covariate_model_anova <- anova(covariate_model)
partial_r2 <- covariate_model_anova$`Sum Sq`[2]/(covariate_model_anova$`Sum Sq`[2]+covariate_model_anova$`Sum Sq`[3])
partial_cor <- sqrt(partial_r2)
cor.test(x = temp$cr_severity, y = temp$estimates)
anova(lm(formula = cr_severity~estimates, data = temp))
caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr')
sqrt(caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr'))

# test = 2017 late, train = 2018
temp <- estimates_list[[4]]
simple_model <- lm(formula = cr_severity~fact_date, data = temp)
covariate_model <- lm(formula = cr_severity~fact_date+estimates%in%fact_date, data = temp)
anova(simple_model, covariate_model)
covariate_model_anova <- anova(covariate_model)
partial_r2 <- covariate_model_anova$`Sum Sq`[2]/(covariate_model_anova$`Sum Sq`[2]+covariate_model_anova$`Sum Sq`[3])
partial_cor <- sqrt(partial_r2)
cor.test(x = temp$cr_severity, y = temp$estimates)
anova(lm(formula = cr_severity~estimates, data = temp))
caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr')
sqrt(caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr'))

# test = 2018, train = 2017 early
temp <- estimates_list[[5]]
simple_model <- lm(formula = cr_severity~fact_date, data = temp)
covariate_model <- lm(formula = cr_severity~fact_date+estimates%in%fact_date, data = temp)
anova(simple_model, covariate_model)
covariate_model_anova <- anova(covariate_model)
partial_r2 <- covariate_model_anova$`Sum Sq`[2]/(covariate_model_anova$`Sum Sq`[2]+covariate_model_anova$`Sum Sq`[3])
partial_cor <- sqrt(partial_r2)
cor.test(x = temp$cr_severity, y = temp$estimates)
anova(lm(formula = cr_severity~estimates, data = temp))
caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr')
sqrt(caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr'))

# test = 2018, train = 2017 late
temp <- estimates_list[[6]]
simple_model <- lm(formula = cr_severity~fact_date, data = temp)
covariate_model <- lm(formula = cr_severity~fact_date+estimates%in%fact_date, data = temp)
anova(simple_model, covariate_model)
covariate_model_anova <- anova(covariate_model)
partial_r2 <- covariate_model_anova$`Sum Sq`[2]/(covariate_model_anova$`Sum Sq`[2]+covariate_model_anova$`Sum Sq`[3])
partial_cor <- sqrt(partial_r2)
cor.test(x = temp$cr_severity, y = temp$estimates)
anova(lm(formula = cr_severity~estimates, data = temp))
caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr')
sqrt(caret::R2(pred = temp$estimates, obs = temp$cr_severity, na.rm = TRUE, formula = 'corr'))

pdf(file = "figures/among_environment_cv.pdf", width = 6, height = 10)
par(mfrow=c(3,1))
par(mar=c(4.5,5,1,1))

temp <- predictions_list[[1]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

plot(x = temp$date_environment, y = temp$cor, col = "red", xlab = "Testing set time point", ylab = parse(text = 'italic(r)[OE]'), ylim = c(-0.125, 0.375), pch = 16, cex.lab = 1.5, cex.axis = 1.5)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "red")

temp <- predictions_list[[2]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

points(x = temp$date_environment, y = temp$cor, col = "blue", pch = 16)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "blue")

text(x = min(temp$date_environment)+3, y = 0.35, labels = "2017 early", cex = 2)
legend(x = max(temp$date_environment)-7.5, y = -0.025, col = c("red", "blue"), lty = 1, bty = "n", pch = 16, legend = c("2017 late", "2018"), cex = 1.5)

temp <- predictions_list[[3]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

plot(x = temp$date_environment, y = temp$cor, col = "red", xlab = "Testing set time point", ylab = parse(text = 'italic(r)[OE]'), ylim = c(-0.125, 0.375), pch = 16, cex.lab = 1.5, cex.axis = 1.5)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "red")

temp <- predictions_list[[4]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

text(x = min(temp$date_environment)+1.5, y = 0.35, labels = "2017 late", cex = 2)
legend(x = max(temp$date_environment)-4.75, y = -0.025, col = c("red", "blue"), lty = 1, bty = "n", pch = 16, legend = c("2017 early", "2018"), cex = 1.5)

points(x = temp$date_environment, y = temp$cor, col = "blue", pch = 16)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "blue")

temp <- predictions_list[[5]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

plot(x = temp$date_environment, y = temp$cor, col = "red", xlab = "Testing set time point", ylab = parse(text = 'italic(r)[OE]'), ylim = c(-0.125, 0.445), pch = 16, cex.lab = 1.5, cex.axis = 1.5)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "red")

temp <- predictions_list[[6]]
temp$date_environment <- as.Date(sub(pattern = ".*\ ", replacement = "", x = temp$date_environment))

points(x = temp$date_environment, y = temp$cor, col = "blue", pch = 16)
lines(x = temp$date_environment, y = temp$cor, type = "l", col = "blue")

text(x = min(temp$date_environment)+0.5, y = 0.415, labels = "2018", cex = 2)
legend(x = max(temp$date_environment)-3, y = 0.000, col = c("red", "blue"), lty = 1, bty = "n", pch = 16, legend = c("2017 early", "2017 late"), cex = 1.5)

dev.off()