library(rpart)
library(rpart.plot)
library(ROCR)

survey <- read.csv("./lab7/survey.csv")

train <- survey[1:600, ]
test  <- survey[601:750, ]

tree_info <- rpart(MYDEPV ~ Price + Income + Age,
                   data = train,
                   method = "class",
                   control = rpart.control(xval = 3),
                   parms = list(split = "information"))

tree_info
printcp(tree_info) # prints with complexity parameter

rpart.plot(tree_info, main = "Classification Tree (Information Gain)", tweak = 1.5)

pred_train <- predict(tree_info, train, type = "class")

conf_matrix_train <- table(Predicted = pred_train, Actual = train$MYDEPV)
print("Confusion Matrix:")
print(conf_matrix_train)

accuracy_by_class <- diag(conf_matrix_train) / colSums(conf_matrix_train)
print("Accuracy by MYDEPV class:")
print(accuracy_by_class)

resub_error <- mean(pred_train != train$MYDEPV)
print(paste("Resubstitution Error Rate (Training):", resub_error))

pred_probs <- predict(tree_info, train, type = "prob")

pred_obj <- prediction(pred_probs[, "1"], train$MYDEPV)
roc_perf <- performance(pred_obj, "tpr", "fpr")
plot(roc_perf, col = "blue", lwd = 2, main = "ROC Curve (Training Data)")
abline(a = 0, b = 1, col = "gray", lty = 2)

auc_perf <- performance(pred_obj, measure = "auc")
auc_value <- auc_perf@y.values[[1]]
print(paste("Area Under the ROC Curve (AUC):", auc_value))


# GINI
tree_gini <- rpart(MYDEPV ~ Price + Income + Age,
                   data = train,
                   method = "class",
                   control = rpart.control(xval = 3),
                   parms = list(split = "gini"))
print(tree_gini)
printcp(tree_gini)


rpart.plot(tree_gini, main = "Classification Tree (Gini Index)", tweak = 1.5)

cp_table <- tree_gini$cptable
optimal_cp <- cp_table[which.min(cp_table[, "xerror"]), "CP"]
cat("Optimal CP value for pruning:", optimal_cp, "\n")
pruned_tree <- prune(tree_gini, cp = optimal_cp)

print(pruned_tree)
rpart.plot(pruned_tree, main = "Pruned Classification Tree (Gini Index)")













