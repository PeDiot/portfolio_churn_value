# ---------- Unsupervised classification ----------

dir <- "./models/clustering/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"

load(paste(data_path, "train_test_data_clust.RData", sep = "/"))

library(tidyverse)
library(ggplot2)
library(factoextra)

theme_set(theme_minimal()) 

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
  scale()

scaled_data %>%
  as.data.frame() %>%
  ggplot(aes(x = CLTV, y = Churn_Risk)) +
    geom_point()

n_centers <- 4

set.seed(123)
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
  ggtheme = theme_minimal()
)

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
  ggtheme = theme_minimal()
)
