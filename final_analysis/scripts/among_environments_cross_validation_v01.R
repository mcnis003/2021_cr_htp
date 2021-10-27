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




# test = 2017 early, train = 2017 late
mean(cor_list[[1]]$cor)
cor(x = estimates_list[[1]]$cr_severity, y = estimates_list[[1]]$estimates)
temp <- anova(lm(formula = cr_severity~date_environment+estimates%in%date_environment, data = estimates_list[[1]]))
sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
cor(x = estimates_list[[1]]$cr_severity, y = estimates_list[[1]]$estimates)-sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
# test = 2017 early, train = 2018
mean(cor_list[[2]]$cor)
cor(x = estimates_list[[2]]$cr_severity, y = estimates_list[[2]]$estimates)
temp <- anova(lm(formula = cr_severity~date_environment+estimates%in%date_environment, data = estimates_list[[2]]))
sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
cor(x = estimates_list[[2]]$cr_severity, y = estimates_list[[2]]$estimates)-sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
# test = 2017 late, train = 2017 late
mean(cor_list[[3]]$cor)
cor(x = estimates_list[[3]]$cr_severity, y = estimates_list[[3]]$estimates)
temp <- anova(lm(formula = cr_severity~date_environment+estimates%in%date_environment, data = estimates_list[[3]]))
sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
cor(x = estimates_list[[3]]$cr_severity, y = estimates_list[[3]]$estimates)-sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
# test = 2017 late, train = 2018
mean(cor_list[[4]]$cor)
cor(x = estimates_list[[4]]$cr_severity, y = estimates_list[[4]]$estimates)
temp <- anova(lm(formula = cr_severity~date_environment+estimates%in%date_environment, data = estimates_list[[4]]))
sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
cor(x = estimates_list[[4]]$cr_severity, y = estimates_list[[4]]$estimates)-sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
# test = 2018, train = 2017 early
mean(cor_list[[5]]$cor)
cor(x = estimates_list[[5]]$cr_severity, y = estimates_list[[5]]$estimates)
temp <- anova(lm(formula = cr_severity~date_environment+estimates%in%date_environment, data = estimates_list[[5]]))
sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
cor(x = estimates_list[[5]]$cr_severity, y = estimates_list[[5]]$estimates)-sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
# test = 2018, train = 2017 late
mean(cor_list[[6]]$cor)
cor(x = estimates_list[[6]]$cr_severity, y = estimates_list[[6]]$estimates)
temp <- anova(lm(formula = cr_severity~date_environment+estimates%in%date_environment, data = estimates_list[[6]]))
sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))
cor(x = estimates_list[[6]]$cr_severity, y = estimates_list[[6]]$estimates)-sqrt(temp$`Sum Sq`[2]/(temp$`Sum Sq`[2]+temp$`Sum Sq`[3]))

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