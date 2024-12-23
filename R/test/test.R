library(ggplot2)
test <- read.table("zipIncome.txt", header = TRUE, sep = "|")
income_values <- test$meanhouseholdincome
test$zip_prefixes <- as.factor(test$zip_prefixes)
summary(test)
incomeliners <- income_values[income_values > 0]
mean_test <- mean(incomeliners)
print(paste("Средний доход домохозяйства:", mean_test))
median_test <- median(incomeliners)
print(paste("Средний доход домохозяйства:", median_test))
outliers <- income_values[income_values <= 0]
print(outliers)

#вычисления для графа
result <- aggregate(test$meanhouseholdincome ~ test$zip_prefix, data = test, FUN = mean)
print(result)

ggplot(test, aes(x = factor(zip_prefixes), y = meanhouseholdincome)) +
  geom_point(position = "jitter", alpha = 0.2) + 
  scale_y_log10() +  
  labs(x = "Zip Code", y = "Log10 of Mean Household Income") +  
  theme_minimal()  # Use a minimal theme


#вернуть boxplot(вернуть, как интерпритирорвать)
ggplot(test, aes(x = factor(zip_prefixes), y = meanhouseholdincome)) +
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.size = 2, alpha = 0.5) +  
  scale_y_log10() + 
  labs(x = "Zip Code", y = "Log10 of Mean Household Income") +  
  theme_minimal()  

#наложение графиков
ggplot(test, aes(x = factor(zip_prefixes), y = meanhouseholdincome)) +
  geom_point(position = "jitter", alpha = 0.2) +  
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.size = 2, alpha = 0.5) +  
  scale_y_log10() +  
  labs(x = "Zip Code", y = "Log10 of Mean Household Income") +  
  theme_minimal()  

#нарисовать циклоиду
r <- 1
t <- seq(0, 4*pi, length.out = 1000)  # изменяется от 0 до 4*pi 
x <- r * (t - sin(t))
y <- r * (1 - cos(t))
plot(x, y, type = 'l', col = 'blue', lwd = 2, xlab = 'x', ylab = 'y', main = 'Циклоида')
grid()

