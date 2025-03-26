euclidean_distance <- function(a, b) {
  sum((a - b)^2)
}

data_points <- read.csv("~/saniya/data.csv", header = FALSE)
k <- as.numeric(readline(prompt = "Enter the number of centroids (k): "))
centroids <- data.frame(v1 = numeric(k), v2 = numeric(k))

for(i in 1:k) {
  cat("Enter coordinates for centroid", i, ":\n")
  centroids$v1[i] <- as.numeric(readline(prompt = paste("v1 for centroid", i, ": ")))
  centroids$v2[i] <- as.numeric(readline(prompt = paste("v2 for centroid", i, ": ")))
}
cluster_assignment <- rep(0, nrow(data_points))

iteration <- 1
while (TRUE) {
  for (i in 1:nrow(data_points)) {
    distances <- sapply(1:k, function(j) euclidean_distance(data_points[i, ], centroids[j, ]))
    cluster_assignment[i] <- which.min(distances)
  }
  
  new_centroids <- centroids
  for (i in 1:k) {
    if (any(cluster_assignment == i)) {
      new_centroids[i, ] <- colMeans(data_points[cluster_assignment == i, , drop = FALSE])
    }
  }
  
  cat("\nIteration:", iteration, "\n")
  
  for (i in 1:k) {
    cat("Cluster", i, "points:\n")
    print(data_points[cluster_assignment == i, , drop = FALSE])
    cat("\n")
  }
  
  cat("Centroids:\n")
  print(new_centroids)
  
  if (all(new_centroids == centroids)) {
    cat("\nFinal centroids:\n")
    print(new_centroids)
    break
  }
  
  centroids <- new_centroids
  iteration <- iteration + 1
}

plot(data_points[, 1], data_points[, 2], col = cluster_assignment, pch = 19, xlab = "x", ylab = "y", main = paste("K means"))
points(new_centroids[, 1], new_centroids[, 2], col = "blue", pch = 4, cex = 2)


