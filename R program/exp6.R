library(ggplot2)

data <- read.csv("~/saniya/data.csv")

k <- 3

plot_kmeans <- function(data, kmeans_result) {
  plot_data <- data
  plot_data$cluster <- as.factor(kmeans_result$cluster)
  
  centroids <- as.data.frame(kmeans_result$centers)
  centroids$cluster <- as.factor(1:k)  
  ggplot(plot_data, aes(x = X, y = Y, color = cluster)) +
    geom_point(size = 3) +
    geom_point(data = centroids, aes(x = X, y = Y), 
               color = "black", size = 5, shape = 4) +  
    labs(title = "K-means Clustering", x = "X", y = "Y") +
    theme_minimal() +
    theme(legend.title = element_blank())
}

set.seed(123)

for (i in 1:5) {
  kmeans_result <- kmeans(data, centers = k, nstart = 25)
  
  cluster_points_list <- vector("list", k)
  
  print(paste("Iteration:", i))
  print("Centroids:")
  print(kmeans_result$centers)
  
  for (j in 1:k) {
    cluster_points <- data[kmeans_result$cluster == j, ]
    ordered_pairs <- paste0("(", cluster_points$X, ", ", cluster_points$Y, ")")
    cluster_points_list[[j]] <- ordered_pairs
    
    cat(paste("Points in Cluster", j, ":\n"))
    print(ordered_pairs)
    cat("\n")
  }
  
  centroids <- as.data.frame(kmeans_result$centers)
  centroids$cluster <- as.factor(1:k)  
  centroids$points <- I(cluster_points_list)  
  
  print("Centroids with Points:")
  print(centroids)
  
  plot_kmeans(data, kmeans_result)
}

plot_kmeans(data, kmeans_result)