library(e1071)
library(ggplot2)

data <- read.csv("./lab6/nbtrain.csv")

# data$income <- as.factor(data$income)

train <- data[1:9010, ]
test <- data[9011:10010, ]

model_income <- naiveBayes(income ~ age + sex + educ, data = train)
model_income
# if actions are undependened
# P(Action | Atributes) = mul(P(atr_i | Action)) * P(Action) / P(Atributes)
# as we're predicting enum(Actions) for defined Atribures - P(Atributes) is a const so we can ignore it
# we should also use log to not use a lot of multiplication op + _eps, also this will fix having zero in the mul chain
# so: sum(log(P(atr_i | Action) + _eps)) + log(P(Action))

pred_income <- predict(model_income, newdata = test)

confusion_matrix <- table(Predicted = pred_income, Actual = test$income)
print("Confusion Matrix for Income Prediction:")
print(confusion_matrix)


overall_error <- mean(pred_income != test$income)
print("")
print("Overall Misclassification Rate:")
print(overall_error)

income_levels <- unique(test$income)
for (level in income_levels) {
  total <- sum(confusion_matrix[, level])
  correct <- confusion_matrix[level, level]
  misclass_rate <- 1 - (correct / total)
  print(paste("Misclassification Rate for", level, ":", misclass_rate))
}

# ROC Curve
#
# calculate_tpr_fpr <- function(actual, predicted_prob, threshold, class) {
#   predicted <- ifelse(predicted_prob > threshold, 1, 0)
#   actual <- ifelse(actual == class, 1, 0)  # Assuming "GT 80K" is the positive class
#   TP <- sum(predicted == 1 & actual == 1)
#   FP <- sum(predicted == 1 & actual == 0)
#   FN <- sum(predicted == 0 & actual == 1)
#   TN <- sum(predicted == 0 & actual == 0)
#   TPR <- TP / (TP + FN)
#   FPR <- FP / (FP + TN)
#   return(c(TPR, FPR))
# }
#
# thresholds <- seq(0, 1, by = 0.01)
# roc_points <- sapply(thresholds, function(thresh) {
#   calculate_tpr_fpr(test$income, pred_income[, "GT 80K"], thresh, "GT 80K")
# })
#
#
#
#
# ggplot(roc_data, aes(x = fpr, y = tpr)) +
#   geom_line(color = "blue", size = 2) +
#   geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed", size = 2) +
#   labs(title = "Receiver Operating Characteristic (ROC) Curve",
#        x = "False Positive Rate (FPR)",
#        y = "True Positive Rate (TPR)") +
#   theme_minimal(base_size = 14) +
#   theme(plot.title = element_text(face = "bold", size = 16)) +
#   annotate("text", x = 0.7, y = 0.2,
#            label = paste("ROC curve (AUC =", format(auc_value, digits = 2), ")"),
#            size = 5)


model_sex <- naiveBayes(sex ~ age + educ + income, data = train)
model_sex

pred_sex <- predict(model_sex, test)
confusion_matrix <- table(Predicted = pred_sex, Actual = test$sex)
print(confusion_matrix)

overall_error <- mean(pred_sex != test$sex)
print(paste("Overall Misclassification Rate:", overall_error))


sex_levels <- unique(test$sex)
for (sex in sex_levels) {
  total <- sum(confusion_matrix[, sex])
  correct <- confusion_matrix[sex, sex]
  misclass_rate <- 1 - (correct / total)
  print(paste("Misclassification Rate for", sex, ":", misclass_rate))
}

male_data <- subset(train, sex == "M")
female_data <- subset(train, sex == "F")
for (i in 1:4) {

  male_sample <- male_data[sample(nrow(male_data), 3500), ]
  female_sample <- female_data[sample(nrow(female_data), 3500), ]
  train_balanced <- rbind(male_sample, female_sample)
  model_balanced <- naiveBayes(sex ~ age + educ + income, data = train_balanced)
  model_balanced

  pred_balanced <- predict(model_balanced, test)
  if (i == 1) {
    balanced_confusion_matrix <- table(Predicted = pred_balanced, Actual = test$sex)
    print(balanced_confusion_matrix)
  }

  error_i <- mean(pred_balanced != test$sex)
  print(paste("Iteration", i, "Overall Misclassification Rate:", error_i))
}

