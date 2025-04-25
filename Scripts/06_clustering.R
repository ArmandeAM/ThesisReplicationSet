# 6. Clustering Data 

# 6.1 Prepare clustering data
prepare_cluster_data <- function(data, vars) {
  # Subset to only the selected clustering variables
  cluster_data <- data %>% select(all_of(vars))
  # Identify and record the rows with complete data
  complete_idx <- which(complete.cases(cluster_data))
  # Remove incomplete rows, then standardise each variable
  cluster_data_clean <- cluster_data[complete_idx, ]
  cluster_data_scaled <- scale(cluster_data_clean)
  list(data_scaled = cluster_data_scaled, complete_idx = complete_idx)
}

# 6.2 Plot the Elbow Method
plot_elbow <- function(scaled_data, max_k = 10, nstart = 25) {
  wss <- sapply(1:max_k, function(k) { # compute total within-cluster sum of squares for k = 1…max_k
    kmeans(scaled_data, centers = k, nstart = nstart)$tot.withinss
  })
  df_wss <- data.frame(k = 1:max_k, wss = wss) # plot WSS vs number of clusters to locate the “elbow”
  p <- ggplot(df_wss, aes(x = k, y = wss)) +
    geom_line() + geom_point() +
    labs(title = "Elbow Method for Optimal k", x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares") +
    theme_minimal()
  print(p)
}

# 6.3 Plot distributions of the scaled cluster variables
plot_cluster_distributions <- function(scaled_data) {
  df <- as.data.frame(scaled_data) # reshape scaled data to long form for faceted histograms
  df_long <- pivot_longer(df, cols = everything(), names_to = "Variable", values_to = "Value")
  p <- ggplot(df_long, aes(x = Value)) + # plot each variable’s distribution on its own panel
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    facet_wrap(~ Variable, scales = "free") +
    theme_minimal() +
    labs(title = "Distributions of Scaled Cluster Data Variables")
  print(p)
}

# 6.4. Evaluate silhouette for a range of k values
evaluate_silhouette <- function(scaled_data, k_range = 2:6, nstart = 25) {
  for (k in k_range) {
    km <- kmeans(scaled_data, centers = k, nstart = nstart) # run k-means and compute silhouette widths to assess cluster cohesion
    sil <- silhouette(km$cluster, dist(scaled_data))
    cat("k =", k, " | Average Silhouette =", round(mean(sil[, 3]), 2), "\n")
  }
}

# 6.5 Run k-means for a specified number of clusters
run_kmeans_for_k <- function(scaled_data, k, nstart = 25) {
  kmeans(scaled_data, centers = k, nstart = nstart)
}

# 6.6 Merge the cluster assignments back into the main dataset
assign_clusters <- function(data, complete_idx, cluster_vector, colname) {
  data[[colname]] <- NA
  data[[colname]][complete_idx] <- cluster_vector
  return(data)
}

# 6.7 Ward-initiated k-means: Use hierarchical clustering (Ward’s method) for initial centroids then run k-means
run_ward_kmeans <- function(scaled_data, k) { # use Ward’s method to get initial cluster assignments
  dist_mat <- dist(scaled_data, method = "euclidean")
  hc_ward <- hclust(dist_mat, method = "ward.D2")
  clusters_ward <- cutree(hc_ward, k = k)
  centers_init <- aggregate(scaled_data, by = list(cluster = clusters_ward), FUN = mean) # compute cluster centroids from the Ward grouping for k-means initialisation
  centers_init <- centers_init[, -1]  # Remove the grouping column
  km_ward <- kmeans(scaled_data, centers = centers_init, nstart = 1, iter.max = 100)
  return(km_ward)
}

# Our final clustering is needed for the SEM 

# Define the variables to use in clustering
cluster_vars <- c("healthShortage", "healthInequity", 
                  "trustOnline", "trustMedSci", "trustPolPub", 
                  "benefits", "SocialProtection")

# Prepare the data for clustering
cluster_prep <- prepare_cluster_data(Barometre_2021, cluster_vars)
scaled_data <- cluster_prep$data_scaled
complete_idx <- cluster_prep$complete_idx

# Plot the elbow method to decide on the number of clusters
plot_elbow(scaled_data, max_k = 10)

# Plot the distributions of the scaled cluster data variables
plot_cluster_distributions(scaled_data)

# Deciding on k = 3 based on the above analyses:
set.seed(123)
km3 <- run_kmeans_for_k(scaled_data, k = 3, nstart = 25)

# Inspect cluster centers and sizes:
print(as.data.frame(km3$centers))
print(table(km3$cluster))

# Merge the cluster assignments back into Barometre_2021 (naming the column "cluster_k3")
Barometre_2021 <- assign_clusters(Barometre_2021, complete_idx, km3$cluster, "cluster_k3")

# Testing k=4 
set.seed(123)
km4 <- run_kmeans_for_k(scaled_data, k = 4, nstart = 25)

# Inspect cluster centers and sizes:
print(as.data.frame(km4$centers))
print(table(km4$cluster))

Barometre_2021 <- assign_clusters(Barometre_2021, complete_idx, km4$cluster, "cluster_k4")

# --- Ward-based (Hybrid) Clustering ---

# Run Ward-initiated k-means for k = 3
km_ward3 <- run_ward_kmeans(scaled_data, k = 3)
print(as.data.frame(km_ward3$centers))
print(table(km_ward3$cluster))
Barometre_2021 <- assign_clusters(Barometre_2021, complete_idx, km_ward3$cluster, "cluster_ward_k3")

# Check: Internal validity

int_crit <- intCriteria(as.matrix(scaled_data), km_ward3$cluster,
                        c("Calinski_Harabasz"))
