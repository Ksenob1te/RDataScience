library(tibble)
library(ggplot2)
library(dendextend)

load(file = "income_elec_state.Rdata")
file_data <- rownames_to_column(income_elec_state, var = "state")

data_log <- file_data
data_log[, 2:3] <- log10(file_data[, 2:3])

hierarchical_clustering <- function(data, method) {
  dist_matrix <- dist(data[, 2:3]) # Euclidean distance
  
  hc <- hclust(dist_matrix, method = method)
  
  plot(hc, main = paste("Dendrogram using", method, "linkage"), xlab = "", sub = "")
  
  clusters <- cutree(hc, k = 4)
  data$cluster <- as.factor(clusters)
  return(data)
}

linkage_methods <- c("single", "complete", "average", "centroid")
# complete - maximal inter-cluster dissimilarity (use largest)
# single - minimal inter-cluster dissimilarity (use lowest)
# average - mean inter-cluster dissimilarity (use average)
# centroid - dissimilarity between centroids

plot_list <- list()
i <- 0
par(mfrow = c(2, 2))
for (method in linkage_methods) {
  rdata <- hierarchical_clustering(data_log, method)
  print(rdata)
  i <-i + 1
  plot_list[[i]] = ggplot(rdata, aes(x = elec, y = income, color = cluster)) +
    geom_point(size = 3) +
    labs(title = paste("Hierarchical Clustering -", method, "Linkage"), x = "Electricity Usage", y = "Income") +
    theme_minimal()
}
library(gridExtra)
grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 2)


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


plot_list <- list()
i <- 0
par(mfrow = c(2, 2))
for (method in linkage_methods) {
  rdata <- hierarchical_clustering(cleaned_data, method)
  print(rdata)
  i <-i + 1
  plot_list[[i]] = ggplot(rdata, aes(x = elec, y = income, color = cluster)) +
    geom_point(size = 3) +
    labs(title = paste("Hierarchical Clustering -", method, "Linkage"), x = "Electricity Usage", y = "Income") +
    theme_minimal()
}
library(gridExtra)
grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 2)