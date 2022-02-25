# --------- Unsupervised classification: Hierarchical Clustering ----------

# ----- Setup -----

dir <- "./models/clustering/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/clustering/"

load(file = paste0(backup_path, "res.mca.RData"))
load(paste(data_path, "telco_cleaned.RData", sep = "/"))

library(tidyverse)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Apply HCP -----

# NOT RUN {
  system.time(
    res.hcpc <- HCPC(res = res.mca2, 
                     nb.clust = -1, 
                     metric = "euclidean", 
                     method = "ward") 
  )
  
  save(res.hcpc, 
       file = paste0(backup_path, "res.hcpc.RData"))

# }

  
load(file = paste0(backup_path, "res.hcpc.RData"))

# Cluster visualization -----
cluster_viz <- function(axes){
  fviz_cluster(res.hcpc,
               repel = TRUE,  
               axes = axes, 
               geom = "point", 
               show.clust.cent = TRUE, 
               palette = "jco",         
               ggtheme = theme_minimal())
}

axes <- list(c(1, 2), 
             c(3, 4), 
             c(5, 6), 
             c(7, 8), 
             c(9, 10))
ggpubr::ggarrange(plotlist = lapply(axes, 
                                    cluster_viz), 
                  ncol = 3, nrow = 2, 
                  common.legend = T)


# Cluster description -----

res.hcpc$desc.var$category

# parangons
para1 <- res.hcpc$desc.ind$para$`1` %>%
  names()
para2 <- res.hcpc$desc.ind$para$`2` %>%
  names()
para3 <- res.hcpc$desc.ind$para$`3` %>%
  names()

# bronze customers
cleaned_data[para1, ] %>%
  View()

# silver / high value customers
cleaned_data[para2, ] %>%
  View()

# gold / loyal customers
cleaned_data[para3, ] %>%
  View()

desc.dat <- cleaned_data %>%
  mutate(cluster = res.hcpc$data.clust$clust)

# number of customers in each cluster
table(desc.dat$cluster) %>%
  prop.table()

# quanti variables per cluster

dens_by_cluster <- function(var){
  desc.dat %>%
    ggplot() +
    geom_histogram(aes_string(x = var,
                              fill = "cluster",
                              color = "cluster"),
                 alpha = .5, 
                 bins = 30) +
    theme(legend.position = "none") +
    facet_grid(~cluster)
}


quanti_vars <- c("Monthly_Charges", 
                 "Total_Charges", 
                 "Churn_Score")

lapply(quanti_vars, 
       dens_by_cluster)

# cat variables per cluster

barplot_by_cluster <- function(var){
  desc.dat %>%
    ggplot() +
    geom_bar(aes_string(x = var, 
                        fill = "cluster"), 
             alpha = .7) +
    coord_flip() +
    labs(x = "", 
         y = "", 
         title = var) +
    theme(legend.position = "top", 
          title = element_text(size = 10))
}

cat_vars <- cleaned_data %>%
  select_if(is.factor) %>%
  select(-c("City", 
            "Zip_Code", 
            "Gender",
            "Churn_Reason")) %>%
  colnames() 

ggpubr::ggarrange(plotlist = lapply(cat_vars, 
                                    barplot_by_cluster), 
                  common.legend = T) 

