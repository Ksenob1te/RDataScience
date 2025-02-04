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

pred_probs <- predict(tree_info, train, type = "matrix")





