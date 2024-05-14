X <- genre_data[, sapply(genre_data, is.numeric)] # X is for 
scaled_dataX <- scale(X)

preprocess_pipelineX <- list(
  step_scale = preProcess(X, method = c("center", "scale"))
)

scaled_dataX <- predict(preprocess_pipelineX$step_scale, X)

k <- 10  # Number of clusters
kmeans_resultX <- kmeans(scaled_dataX, centers = k, nstart = 10)
genre_data$cluster_label <- kmeans_resultX$cluster

song_cluster_pipeline <- list(
  steps = list(
    step_scale = preprocess_pipeline$step_scale
  )
)

# Perform PCA for dimensionality reduction
pca_resultX <- prcomp(scaled_dataX, n.comp = 2)
pca_componentsX <- as.data.frame(pca_resultX$x[, 1:2])

projectionX <- data.frame(x = pca_componentsX[, 1], y = pca_componentsX[, 2])
projectionX$title <- genre_data$name
projectionX$cluster <- genre_data$cluster_label

ggplot(projectionX, aes(x = x, y = y, color = factor(cluster))) +
  geom_point() +
  labs(color = "Cluster", title = "PCA Visualization of Genre Clusters") +
  theme_minimal()


diss_matrix <- dist(scaled_dataX)
silhouette_score <- cluster.stats(diss_matrix, kmeans_resultX$cluster)$avg.silwidth
print(paste("Silhouette Score:", silhouette_score))




k <- 20  # Number of clusters
kmeans_resultY <- kmeans(scaled_dataY, centers = k, nstart = 10)
train_data$cluster_label <- kmeans_resultY$cluster

song_cluster_pipelineY <- list(
  steps = list(
    step_scale = preprocess_pipeline$step_scale
  )
)

# Perform PCA for dimensionality reduction
pca_resultY <- prcomp(scaled_dataY, n.comp = 2)
pca_componentsY <- as.data.frame(pca_resultY$x[, 1:2])

projectionY <- data.frame(x = pca_componentsY[, 1], y = pca_componentsY[, 2])
projectionY$title <- train_data$name
projectionY$cluster <- train_data$cluster_label

ggplot(projectionY, aes(x = x, y = y, color = factor(cluster))) +
  geom_point() +
  labs(color = "Cluster", title = "PCA Visualization of Song Clusters") +
  theme_minimal()

diss_matrixY <- dist(scaled_dataY)
silhouette_scoreY <- cluster.stats(diss_matrixY, kmeans_resultY$cluster)$avg.silwidth
print(paste("Silhouette Score:", silhouette_scoreY))
