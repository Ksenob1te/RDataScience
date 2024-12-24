library(tibble)
library(ggplot2)

load(file = "income_elec_state.Rdata")
file_data <- rownames_to_column(income_elec_state, var="state")
# set.seed(0)
kmeans_result <- kmeans(file_data[, -1], centers = 10)
file_data$cluster <- as.factor(kmeans_result$cluster)

centers_df <- as.data.frame(kmeans_result$centers)
centers_df$cluster <- as.factor(1:10)

ggplot(file_data, aes(x = elec, y = income, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = centers_df, shape = 8, size = 8) +
  labs(title = "K-means Clustering", x = "Income", y = "Electricity Usage")

# ---------------------------------
# find k wss (within sum of squares)
test_len <- 20

wss <- numeric(length=test_len)
for(i in 1:test_len) {
  wss[i] <- kmeans(file_data[, 2:3], center = i, nstar = 25)$tot.withinss # total within-cluster sum of squares (euclidean metric)
}
# ---------------------------------
# find the best k
max_value <- max(wss)
min_value <- min(wss)
norm_wss <- (wss - min_value) / (max_value - min_value) * test_len
print(norm_wss)
par(mfrow = c(1, 2)) 
plot(1:test_len, norm_wss, type = "b", pch = 16, frame = TRUE, xlab = "cluster amount", ylab = "normalized * amount within-cluster sum of squares")

derivatives <- diff(norm_wss) / diff(1:test_len)
print(derivatives)
best_k_old <- which.min(abs(derivatives + 1))
print(paste("Best K found: ", best_k_old))
# ---------------------------------
#log10
data_log <- file_data
data_log[, 2:3] <- log10(file_data[, 2:3])
print(data_log)
# ---------------------------------
# find new k wss (within sum of squares)
wss <- numeric(length=test_len)
for(i in 1:test_len) {
  wss[i] <- kmeans(data_log[, 2:3], center = i, nstar = 25)$tot.withinss # total within-cluster sum of squares (euclidean metric)
}
# ---------------------------------
# find new best k
max_value <- max(wss)
min_value <- min(wss)
norm_wss <- (wss - min_value) / (max_value - min_value) * test_len
print(norm_wss)

plot(1:test_len, norm_wss, type = "b", pch = 16, frame = TRUE, xlab = "cluster amount", ylab = "normalized * amount within-cluster sum of squares")
par(mfrow = c(1, 1))
derivatives <- diff(norm_wss) / diff(1:test_len)
print(derivatives)
best_k <- which.min(abs(derivatives + 1))
print(paste("Best new K found: ", best_k))
# ---------------------------------
kmeans_result <- kmeans(file_data[, 2:3], centers = best_k_old)
file_data$cluster <- as.factor(kmeans_result$cluster)

centers_df <- as.data.frame(kmeans_result$centers)
centers_df$cluster <- as.factor(1:best_k_old)

ggplot(file_data, aes(x = elec, y = income, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = centers_df, shape = 8, size = 8) +
  labs(title = "K-means old Clustering", x = "Income", y = "Electricity Usage")


kmeans_log_result <- kmeans(data_log[, 2:3], centers = best_k, nstart = 25)

data_log$cluster <- as.factor(kmeans_log_result$cluster)

centers_df <- as.data.frame(kmeans_log_result$centers)
centers_df$cluster <- as.factor(1:best_k)

ggplot(data_log, aes(x = elec, y = income, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = centers_df, shape = 8, size = 8) +
  labs(title = "K-means new clustering", x = "Income", y = "Electricity Usage")

# ---------------------------------
summary(income_elec_state)



