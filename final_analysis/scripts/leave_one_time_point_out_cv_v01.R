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

#######################################
# Leave-one-time-point-out cross-validation

# set folds to be observation dates
date_environment_combos <- unique(all_results$date_environment)

result_list <- list(NA)

for (i in 24:length(date_environment_combos)){
  
  train_df <- all_results[-which(all_results$date_environment == date_environment_combos[i]),]
  test_df <- all_results[which(all_results$date_environment == date_environment_combos[i]),]
  
  # set modeling parameters
  objControl <- trainControl(method = "cv", number = 3)
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  
  # train model
  objModel <- train(cr_severity~mean_725nm+mean_800nm+mean_red+mean_green+mean_blue,
                    data = train_df,
                    method='rf', 
                    trControl=objControl)
  
  # end training
  stopCluster(cl)
  
  test_df$estimates <- predict(object=objModel$finalModel, newdata = test_df[,c("mean_red",
                                                                                "mean_green",
                                                                                "mean_blue",
                                                                                "mean_725nm",
                                                                                "mean_800nm")])
  
  result_list[[i]] <- test_df
}


loocv_results <- do.call(rbind.data.frame, result_list)

# linear models
simple_model <- lm(formula = cr_severity~environment*fact_date%in%environment, data = loocv_results)
covariate_model <- lm(formula = cr_severity~environment+fact_date%in%environment+estimates%in%fact_date%in%environment, data = loocv_results)


# simple R2
regression_model <- lm(formula = cr_severity~estimates, data = loocv_results)
anova(regression_model)$`Pr(>F)`[1]
caret::R2(pred = loocv_results$estimates, obs = loocv_results$cr_severity)
cor(x = loocv_results$estimates, y = loocv_results$cr_severity)

# partial_r2
covariate_model_anova <- anova(covariate_model)
covariate_model_anova$`Sum Sq`[3]/(covariate_model_anova$`Sum Sq`[3]+covariate_model_anova$`Sum Sq`[4])
# partial_r2 p value
model_contrast <- anova(simple_model, covariate_model)
model_contrast$`Pr(>F)`[2]
#partial_cor
sqrt(covariate_model_anova$`Sum Sq`[3]/(covariate_model_anova$`Sum Sq`[3]+covariate_model_anova$`Sum Sq`[4]))

cor_result <- data.frame(date_environment = unique(all_results$date_environment), cor = NA, p = NA)
for (i in 1:nrow(cor_result)){
  testing_time_point <- loocv_results[which(loocv_results$date_environment == cor_result$date_environment[i]),]
  cor_result$cor[i] <- cor(x = testing_time_point$cr_severity,
                           y = testing_time_point$estimates)
  cor_result$p[i] <- cor.test(x = testing_time_point$cr_severity,
                              y = testing_time_point$estimates)$p.value
  
}

cor_result$date <- as.Date(sub(pattern = ".*\ ", replacement = "", x = cor_result$date_environment))
cor_result$environment <- sub(pattern = "\ .*", replacement = "", x = cor_result$date_environment)
cor_result$planting_date <- as.Date(plyr::mapvalues(x = cor_result$environment, from = c("2017_early", "2017_late", "2018"), to = c("2017-05-05", "2017-05-21", "2018-05-07")))
cor_result$days_after_planting <- cor_result$date - cor_result$planting_date
cor_result$plot_color <- plyr::mapvalues(x = cor_result$environment, from = c("2017_early", "2017_late", "2018"), to = c("red", "green", "blue"))
cor_result <- cor_result[order(cor_result$date_environment),]
write.csv(x = cor_result, file = "results/lotpocv_simple_result.csv")
#cor_result <- read.csv(file = "results/lotpocv_simple_result.csv")

#plot correlations
pdf(file = "figures/lotpocv_cv.pdf", width = 6, height = 6)
par(mar = c(5.1, 4.1, 1, 1))
plot(x = cor_result$days_after_planting, y = cor_result$cor,
     xlab = "Days after planting",
     ylab = parse(text = 'italic(r)[OE]'),
     xlim = c(30, 67),
     ylim = c(-0.13, 0.52),
     col = cor_result$plot_color, pch = 16)
lines(x = cor_result[which(cor_result$environment == "2017_early"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2017_early"),]$cor, type = "l", col = "red")
lines(x = cor_result[which(cor_result$environment == "2017_late"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2017_late"),]$cor, type = "l", col = "green")
lines(x = cor_result[which(cor_result$environment == "2018"),]$days_after_planting, y = cor_result[which(cor_result$environment == "2018"),]$cor, type = "l", col = "blue")
legend(x = 57, y = 0.545, legend = c("2017 early", "2017 late", "2018"), pch = 16, col = c("red", "green", "blue"), lty = 1, bty = "n")
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()