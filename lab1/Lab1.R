library(ggplot2)
zip_income_table <- read.table("zipIncome.txt", header = TRUE, sep = "|")
colnames(zip_income_table) <- c("zipCode", "income")
income_values <- zip_income_table$income
zip_code <- as.factor(zip_income_table$zipCode)
print(paste("mean income", mean(income_values)))
print(paste("median income", median(income_values)))
summary(zip_income_table)


ggplot(zip_income_table, aes(x = zip_code, y = income_values))  +
  geom_point() + 
  labs(x = "Zip Code", y = "Income", title = "Default") +  
  theme_minimal()

ggplot(zip_income_table, aes(x = zip_code, y = income_values))  +
  geom_bin2d(bins = 50) + 
  scale_fill_gradientn(colors = c("blue","lightblue", "lightgreen", "yellow", "orange")) +
  labs(x = "Zip Code", y = "Income", title = "Beautiful") +  
  theme_minimal()



filtered_data <- zip_income_table[income_values > 7000 & income_values < 200000,]

print(paste("ommited mean income", mean(filtered_data$income)))
print(paste("ommited median income", median(filtered_data$income)))

filtered_income <- filtered_data$income
filtered_zip <- filtered_data$zipCode
ggplot(filtered_data, aes(x = as.factor(filtered_zip), y = filtered_income))  +
  geom_point(size = 5, alpha = 0.1) + 
  scale_y_continuous(trans = "log2") +
  labs(x = "Zip Code", y = "Income", title = "Default ommited") +  
  theme_minimal()

ggplot(filtered_data, aes(x = as.factor(filtered_zip), y = filtered_income))  +
  geom_bin2d(bins = 50) + 
  scale_y_continuous(trans = "log2") +
  scale_fill_gradientn(colors = c("blue","lightblue", "lightgreen", "yellow", "orange")) +
  labs(x = "Zip Code", y = "Income", title = "Beautiful ommited") +  
  theme_minimal()

ggplot(filtered_data, aes(x = as.factor(filtered_zip), y = filtered_income))  +
  geom_point(aes(color = as.factor(filtered_zip)), size = 3, alpha = 0.1, position = "jitter") + 
  geom_boxplot(aes(group = as.factor(filtered_zip)), alpha = 0.1, outlier.size=-Inf) +  
  scale_y_continuous(trans = "log2") +
  labs(x = "Zip Code", y = "Income", title = "Default ommited") +  
  theme_minimal()
