# --------- Unsupervised classification: Hierarchical Clustering ----------

# ----- Setup 

dir <- "./models/clustering/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/clustering/"

load(paste(data_path, "train_test_data_clust.RData", sep = "/"))

library(tidyverse)
library(ggplot2)
library(ggdendro)
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

# ----- hclust 

method <- "complete"
hc <- hclust(d, method)

plot(hc, 
     main = "Dendrogram",
     xlab = " ", 
     labels = FALSE, 
     hang = -1)

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


