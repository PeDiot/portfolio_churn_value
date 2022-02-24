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


# ----- Setup -----

dir <- "./models/clustering/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/clustering/"

load(paste(data_path, "train_test_data_clust.RData", sep = "/"))
load(paste(data_path, "clust_data.RData", sep = "/"))

library(tidyverse)
library(plyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)

theme_set(theme_minimal()) 

# predictions on unseen data
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


# ----- k-means on churn risk and customer value -----

# Scale data -----
scaled_data <- data_clust_tr %>%
  select(c(CLTV, Churn_Risk)) %>%
  scale() %>%
  as.data.frame()

set.seed(123)

# Data Viz -----

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


# k-means -----

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

# on unscaled data 
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

# NOT RUN {

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

# }


# Predictions on test data -----

scaled_data_te <- data_clust_te %>%
  select(c(CLTV, Churn_Risk)) %>%
  scale() %>%
  as.data.frame()

clust_preds <- predict.kmeans(object = res.km, 
               newdata = scaled_data_te, 
               method = "classes")


# Fit k-means to the whole data set -----

load(paste(data_path, "final_data.RData", sep = "/"))

scaled_data <- final_data %>%
  select(c(CLTV, churn_risk)) %>%
  scale() %>%
  as.data.frame()

set.seed(123)
n_centers <- 4

res.km <- kmeans(
  scaled_data,
  centers = n_centers, 
  nstart = 25
)

p <- fviz_cluster(
  res.km, 
  data = scaled_data,
  palette = "Set2", 
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)
p

# most representative individuals (parangons)
  # instance having the smallest euclidean distance from the cluster's centroid
  # repeat for each cluster

data.matrix <- scaled_data %>%
  mutate(cluster = res.km$cluster) 
cluster_col <- which(colnames(data.matrix) == "cluster")

candidates <- dlply(data.matrix, "cluster", function(data) {
  dists <- colSums(laply(data[, -cluster_col], function(x) (x-mean(x))^2))
  rownames(data)[dists == min(dists)]
}) 

parangons <- lapply(
  names(candidates), 
  function(c_name) {candidates[[c_name]]}
) %>% unlist()

para_data <- scaled_data %>%
  mutate(cluster = res.km$cluster %>%
           as.factor()) %>%
  rownames_to_column(var = "CustomerID") %>%
  filter(CustomerID %in% as.numeric(parangons))

final_data[parangons, ] %>% View()


# eclust -----

system.time(
  e.km <- eclust(x = scaled_data, 
                 FUNcluster = "kmeans", 
                 k.max = 5, 
                 nboot = 50, 
                 verbose = TRUE) 
)

e.km$centers

fviz_cluster(
  e.km, 
  data = scaled_data,
  palette = "Set2", 
  geom = "point",
  ellipse.type = "convex",
  main = "Cluster plot on scaled data", 
  ggtheme = theme_minimal()
) +
  geom_point(data = e.km$centers %>% as.data.frame(), 
             aes(x = CLTV, y = Churn_Risk), 
             size = 10, 
             shape = 4)

e.km2 <- eclust(x = scaled_data, 
                FUNcluster = "kmeans", 
                k = 4)

fviz_cluster(
  e.km2, 
  data = scaled_data,
  palette = "Set2", 
  geom = "point",
  ellipse.type = "convex",
  main = "Cluster plot on scaled data", 
  ggtheme = theme_minimal()
) +
  geom_point(data = e.km2$centers %>% as.data.frame(), 
             aes(x = CLTV, y = Churn_Risk), 
             size = 10, 
             shape = 4)



# ----- k-means on the whole data set after MCA -----

# Data Viz -----

colnames(clust_dat) <- colnames(clust_dat) %>% 
  str_remove(pattern = " ")

visualize_axes <- function(ax1 = "Dim1", ax2 = "Dim2"){
  
  clust_dat %>%
    select(c(ax1, ax2)) %>%
    ggplot(aes_string(x = ax1, 
                      y = ax2)) +
    geom_point(alpha = .5)
  
}

ggpubr::ggarrange(
  visualize_axes(), 
  visualize_axes(ax1 = "Dim3", ax2 = "Dim4"), 
  visualize_axes(ax1 = "Dim5", ax2 = "Dim6"), 
  visualize_axes(ax1 = "Dim7", ax2 = "Dim8"), 
  visualize_axes(ax1 = "Dim9", ax2 = "Dim10"),
  ncol = 3, nrow = 2
)

# Scale data  -----

scaled_dat <- clust_dat %>%
  select(-Customer_ID) %>%
  mutate(Monthly_Charges = scale(Monthly_Charges))

# Optimal number of clusters -----

# --- Elbow method
system.time(
  elbow <- fviz_nbclust(scaled_dat, 
                        kmeans, 
                        method = "wss") +
    labs(subtitle = "Elbow method")
)

# --- Silhouette method
system.time(
  silhouette <- fviz_nbclust(scaled_dat, 
                             kmeans, 
                             method = "silhouette") +
    labs(subtitle = "Silhouette method")
)

num_clust_plot <- ggpubr::ggarrange(
  elbow,
  silhouette, 
  ncol = 2
); num_clust_plot

# Apply k-means ----- 

n_centers <- 2

res.km <- kmeans(
  scaled_dat,
  centers = n_centers, 
  nstart = 25
)

choose.vars <- c("Dim1", "Monthly_Charges")
fviz_cluster(
  res.km, 
  choose.vars = choose.vars,
  data = scaled_dat,
  palette = "Set2", 
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)
