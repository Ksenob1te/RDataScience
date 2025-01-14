library(arules)
library(arulesViz)

# how data.i make id's?
transactions <- read.transactions("lab3/AssociationRules.csv", format = "basket", sep = " ")
summary(transactions)

# from arules
# topN - amount of items
# type - absolute or relative
itemFrequencyPlot(transactions, topN = 10, type = "absolute", col = "steelblue", main = "Top 10 Frequent Items")

freq_table <- itemFrequency(transactions, type = "absolute")
freq_table <- sort(freq_table, decreasing = TRUE)
most_frequent_item <- names(freq_table[1])
print(most_frequent_item)

max_items <- max(size(transactions))
print(max_items)


# supp - minimum frequency of (itemset in transactions) - минимальный процент наборов, которые содержат найденную группу
# conf - minimum confidence of rule (X -> Y)
rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0))
length(rules)

# наборы (1, 2) -> 3 и (2, 3) -> 1 имеют разное confidence?
# conf = P(3 | (1, 2)) = P(1, 2, 3) / P(1, 2)
# conf = P(1 | (2, 3)) = P(1, 2, 3) / P(2, 3)
# ответ: да, это 2 разных правила
rules_high_conf <- subset(rules, confidence >= 0.5)
length(rules_high_conf)

library(ggplot2)
plot(rules,
     engine = "ggplot2",
     measure = c("support", "confidence"),
     shading = "lift",
     main = "Support and Confidence") +
  scale_color_gradientn(
    colors = colorRampPalette(c("white", "red"))(20),
    limits = c(0, 10),
    na.value = "blue"
  )  +
  labs(x = "Support", y = "Confidence", color = "Lift") +
  theme_minimal()



plot(rules,
     engine = "ggplot2",
     measure = c("support", "lift"),
     shading = "confidence",
     main = "Support vs Lift") +
  scale_color_gradientn(
    colors = colorRampPalette(c("white", "red"))(20),
    na.value = "blue"
  )  +
  labs(x = "Support", y = "Lift", color = "Confidence") +
  theme_minimal()


rules_10sup <- subset(rules, support >= 0.1)
inspect(rules_10sup)

library(plotly)
rules_df <- as(rules_10sup, "data.frame")
fig <- plot_ly(
  data = rules_df,
  x = ~support,
  y = ~lift,
  text = ~paste("Confidence: ", round(confidence, 2)),
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10, color = ~confidence, colorscale = "Viridis", showscale = TRUE)
)

# matrix ?
rules_10conf <- subset(rules, confidence > 0.8)
inspect(rules_10conf)

plot(rules_10conf, measure = "lift", method = "matrix", control=list(reorder='none'))


# graph
rules_3conf <- sort(rules, by = "lift")[0:3]
plot(rules_3conf, method = "graph", engine = "igraph")


# split sets and run the algorithm again
train_transactions <- transactions[1:8000]
test_transactions <- transactions[8001:10000]

# form rules on train
rules_train <- apriori(train_transactions, parameter = list(supp = 0.01, conf = 0.8))
rules_train_select <- subset(rules_train, lift > 3)
inspect(rules_train_select)

rules_test <- interestMeasure(rules_train_select, transactions = test_transactions, measure = c("support", "confidence", "lift", "count"), reuse = FALSE)
rules_train_data <- as(rules_train_select, "data.frame")

# compare test and train
for (i in 1:length(rules_train_select)) {
  print(paste("Rule:", rules_train_data$rules[i]))
  print(paste("   Train:  Conf: ", round(rules_train_data$confidence[i], digits = 2), "  Lift: ", round(rules_train_data$lift[i], digits = 2)))
  print(paste("   Test:   Conf: ", round(rules_test$confidence[i], digits = 2), "  Lift: ", round(rules_test$lift[i], digits = 2)))
}
