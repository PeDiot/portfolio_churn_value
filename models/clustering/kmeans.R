# ---------- Unsupervised classification: K-means----------

# ----- Description

  # 1. Define the number of clusters (k)

  # 2. Initialize k centroids by randomly.

  # 3. Assignment Step: assign each observation to the closest centroid (center-point) 
  # by calculating least squared euclidean distance between centroids and observations. 
  # (least squared euclidean distance between assigned center and observation should be minimum than other centers).

  # 4. Update Step: Calculate the new means as centroids for new clusters.

  # 5. Repeat both assignment and update step (i.e. steps 3 & 4) 
  # until convergence (minimum total sum of square) or maximum iteration is reached.


# ----- Setup 

dir <- "./models/clustering/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/clustering/"

load(paste(data_path, "train_test_data_clust.RData", sep = "/"))

library(tidyverse)
library(ggplot2)
library(factoextra)

theme_set(theme_minimal()) 

# ----- Visualization

cor(x = data_clust_tr$Churn_Risk, 
    y = data_clust_tr$CLTV)

data_clust_tr %>%
  ggplot(aes(x = CLTV, y = Churn_Risk)) +
  geom_point()

data_clust_tr %>%
  ggplot(aes(x = Churn_Risk)) +
  geom_density()

data_clust_tr %>%
  ggplot(aes(x = CLTV)) +
  geom_density()

scaled_data <- data_clust_tr %>%
  select(c(CLTV, Churn_Risk)) %>%
  scale() %>%
  as.data.frame()

cor(x = scaled_data$Churn_Risk, 
    y = scaled_data$CLTV)

scaled_data %>%
  ggplot(aes(x = CLTV, y = Churn_Risk)) +
  geom_point(alpha = .5) +
  ggtitle("Standardize churn risk and CLTV")


# ----- Apply K-means

set.seed(123)

n_centers <- 4

# on scaled data 
res.km <- kmeans(
  scaled_data,
  centers = n_centers, 
  nstart = 25
)
res.km$cluster

fviz_cluster(
  res.km, 
  data = scaled_data,
  palette = "Set2", 
  geom = "point",
  ellipse.type = "convex",
  main = "Cluster plot on scaled data", 
  ggtheme = theme_minimal()
)

# on raw data 
res.km2 <- kmeans(
  data_clust_tr %>%
    select(c(CLTV, Churn_Risk)),
  centers = n_centers, 
  nstart = 25
)

fviz_cluster(
  res.km2, 
  data = data_clust_tr %>%
    select(c(CLTV, Churn_Risk)),
  stand = F, 
  palette = "Set2", 
  geom = "point",
  ellipse.type = "convex", 
  main = "Cluster plot on unscaled data", 
  ggtheme = theme_minimal()
)

# ----- Optimal number of clusters 

# --- Elbow method
system.time(
  elbow <- fviz_nbclust(scaled_data, 
               kmeans, 
               method = "wss") +
    labs(subtitle = "Elbow method")
)

# --- Silhouette method
system.time(
  silhouette <- fviz_nbclust(scaled_data, 
               kmeans, 
               method = "silhouette") +
    labs(subtitle = "Silhouette method")
)
  
# --- Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
system.time(
  gap_stat <- fviz_nbclust(scaled_data, 
               kmeans, 
               nstart = 25,  
               method = "gap_stat", 
               nboot = 50) +
    labs(subtitle = "Gap statistic method")
)

num_clust_plot <- ggpubr::ggarrange(
  elbow,
  silhouette, 
  gap_stat, 
  ncol = 2, 
  nrow = 2
)

ggsave(filename = paste(backup_path, "num_clust_plot.png", sep = ""),
       plot = num_clust_plot, 
       width = 10, 
       height = 6)

n_centers <- 3

res.km3 <- kmeans(
  scaled_data,
  centers = n_centers, 
  nstart = 25
)

res.km3$centers

fviz_cluster(
  res.km3, 
  data = scaled_data,
  palette = "Set2", 
  geom = "point",
  ellipse.type = "convex",
  main = "Cluster plot on scaled data", 
  ggtheme = theme_minimal()
)


# ----- Predictions on test data 

predict.kmeans <- function(object,
                           newdata,
                           method = c("centers", "classes")) {
  method <- match.arg(method)
  
  centers <- object$centers
  ss_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters <- apply(ss_by_center, 1, which.min)
  
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    best_clusters
  }

}

scaled_data_te <- data_clust_te %>%
  select(c(CLTV, Churn_Risk)) %>%
  scale() %>%
  as.data.frame()

clust_preds <- predict.kmeans(object = res.km, 
               newdata = scaled_data_te, 
               method = "classes")
