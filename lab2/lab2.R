library(tibble)
library(ggplot2)
library(maps)
library(dplyr)


find_best_k <- function(somedata) {
  test_len <- 20
  
  wss <- numeric(length=test_len)
  # ---------------------------------
  # find k wss (within sum of squares)
  for(i in 1:test_len) {
    wss[i] <- kmeans(somedata[, 2:3], center = i, nstar = 25)$tot.withinss # total within-cluster sum of squares (euclidean metric)
  }
  # ---------------------------------
  # find the best k
  max_value <- max(wss)
  min_value <- min(wss)
  norm_wss <- (wss - min_value) / (max_value - min_value) * test_len
  plot(1:test_len, norm_wss, type = "b", pch = 16, frame = TRUE, xlab = "cluster amount", ylab = "normalized * amount within-cluster sum of squares")
  
  derivatives <- diff(norm_wss) / diff(1:test_len)
  found_k <- which.min(abs(derivatives + 1))
  return(found_k)
  # ---------------------------------
}


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

par(mfrow = c(1, 2)) 
best_k_old <- find_best_k(file_data)
print(paste("Best K found: ", best_k_old))
# ---------------------------------
#log10
data_log <- file_data
data_log[, 2:3] <- log10(file_data[, 2:3])
# ---------------------------------
best_k <- find_best_k(data_log)
par(mfrow = c(1, 1)) 
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
iqr_income <- IQR(data_log$income) # Interquartile Range
iqr_elec <- IQR(data_log$elec)

income_thresholds <- quantile(data_log$income, c(0.25, 0.75)) + c(-1.5, 1.5) * iqr_income
elec_thresholds <- quantile(data_log$elec, c(0.25, 0.75)) + c(-1.5, 1.5) * iqr_elec

outliers <- with(
  data_log,
  income < income_thresholds[1] | income > income_thresholds[2] |
  elec < elec_thresholds[1] | elec > elec_thresholds[2]
                 )

cleaned_data <- data_log[!outliers, ]
cleaned_best_k <- find_best_k(cleaned_data)

cleaned_kmeans_result <- kmeans(cleaned_data[, 2:3], centers = cleaned_best_k, nstart = 25)
cleaned_data$cluster <- as.factor(cleaned_kmeans_result$cluster)

centers_df <- as.data.frame(cleaned_kmeans_result$centers)
centers_df$cluster <- as.factor(1:cleaned_best_k)

ggplot(cleaned_data, aes(x = elec, y = income, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = centers_df, shape = 8, size = 8) +
  labs(title = "K-means new clustering", x = "Income", y = "Electricity Usage")

# ---------------------------------
us_map <- map_data("state")
cleaned_data$state <- tolower(state.name[match(cleaned_data$state, state.abb)])

map_data_merged <- us_map %>%
  left_join(cleaned_data, by = c("region" = "state"))

ggplot(map_data_merged, aes(long, lat, group = group, fill = as.factor(cluster))) +
  geom_polygon(color = "white") +  # Draw state borders
  scale_fill_viridis_d(name = "Cluster") +  # Use colorblind-friendly palette
  theme_minimal() +
  labs(title = "Clustered U.S. Map", x = "Longitude", y = "Latitude") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
# ---------------------------------

