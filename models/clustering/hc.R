# --------- Unsupervised classification: Hierarchical Clustering ----------

# ----- Setup -----

dir <- "./models/clustering/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/clustering/"

load(paste(data_path, "train_test_data_clust.RData", sep = "/"))

library(tidyverse)
library(ggplot2)

library(dendextend)
library(JLutils)
library(factoextra)

library(stats)

theme_set(theme_minimal()) 

# scale the data
scaled_data <- data_clust_tr %>%
  select(c(CLTV, Churn_Risk)) %>%
  scale() %>%
  as.data.frame()

# distance matrix
d <- dist(scaled_data)

set.seed(123)

method <- "complete"

# ----- hclust -----

# clustering tree

hc <- hclust(d, method)

plot(hc, 
     main = "Dendrogram",
     xlab = " ", 
     labels = FALSE, 
     hang = -1)

# optimal number of clusters
inertie <- sort(hc$height, decreasing = TRUE)
plot(inertie[1:10], 
     type = "s", 
     xlab = "Number of clusters", 
     ylab = "Inertia")

plot(hc,
     labels = FALSE, 
     main = "Partitioning in 3, 4 or 5 cluster",
     xlab = "", 
     ylab = "", 
     sub = "", 
     axes = FALSE, 
     hang = -1)
rect.hclust(hc, 3, border = "green3")
rect.hclust(hc, 4, border = "red3")
rect.hclust(hc, 5, border = "blue3")


ggplot(color_branches(hc, k = 4), 
       labels = FALSE)

fviz_dend(hc, 
          k = 3, 
          show_labels = FALSE, 
          rect = TRUE)

k_opti <- best.cutree(hc, min = 2, max = 10) 

# tree cut

clusterCut <- cutree(hc, 3)

clust_plot_3 <- scaled_data %>%
  mutate(cluster = as.factor(clusterCut)) %>%
  ggplot(aes(
    x = CLTV, 
    y = Churn_Risk,
    color = cluster
  )) +
  geom_point(alpha = .5) 

clusterCut <- cutree(hc, 4)

clust_plot_4 <- scaled_data %>%
  mutate(cluster = as.factor(clusterCut)) %>%
  ggplot(aes(
    x = CLTV, 
    y = Churn_Risk,
    color = cluster
  )) +
  geom_point(alpha = .5) 


ggpubr::ggarrange(
  clust_plot_3, 
  clust_plot_4, 
  ncol = 2
)

k_best <- 4
clusterCut <- cutree(hc, k_best)


# Use hcut() which compute hclust and cut the tree
k <- 4
hc.cut <- hcut(scaled_data, 
               k = k,
               hc_method = method)
# Visualize dendrogram
dend <- fviz_dend(hc.cut, 
          show_labels = FALSE, 
          rect = TRUE, 
          palette = "Set2")
# Visualize cluster
clust_plot <- fviz_cluster(hc.cut, 
                           geom = "point",
                           ellipse.type = "convex", 
                           palette = "Set2")

ggpubr::ggarrange(dend, clust_plot, ncol = 2)






