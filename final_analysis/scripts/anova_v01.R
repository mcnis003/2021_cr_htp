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
write.csv(x = anova_result, file = "results/preliminary_anova_table.csv", row.names = TRUE)

myColors <- c(rep(x = "red", times =14),
              rep(x = "green", times =8),
              rep(x = "blue", times =5))

#print boxplots of visually observed disease severity
pdf(file = "figures/distribution_vods.pdf", width = 10, height = 10)
par(mar = c(11, 5, 1, 1))
boxplot(formula = cr_severity~date_environment,
        data = all_results,
        las = 2,
        ylab = "Visually observed disease severity", col = myColors)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()