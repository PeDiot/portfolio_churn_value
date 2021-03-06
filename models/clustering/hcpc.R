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
library(ggsci)
library(FactoMineR)
library(factoextra)
library(ggdendro)

theme_new <- function() {
  theme_minimal() %+replace%
    theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14), 
          title = element_text(size = 14), 
          legend.position = "bottom", 
          legend.title = element_blank(), 
          legend.text = element_text(size = 14))
}
theme_set(theme_new())

# Apply HCP with k-means consolidation -----

# NOT RUN {
  system.time(
    res.hcpc <- HCPC(res = res.mca2, 
                     nb.clust = -1, 
                     metric = "euclidean", 
                     method = "ward") 
  )

  res.hcpc$call$bw.before.consol
  res.hcpc$call$bw.after.consol
  
  save(res.hcpc, 
       file = paste0(backup_path, "res.hcpc.RData"))

# }

  
load(file = paste0(backup_path, "res.hcpc.RData"))


# Dendrogram -----

n_clust <- res.hcpc$call$t$nb.clust


dend <- fviz_dend(x = res.hcpc, 
                  k = n_clust, 
                  show_labels = F, 
                  palette = "jco", 
                  color_labels_by_k = T, 
                  rect = T)

# Inertia -----

btw_inertia <- res.hcpc$call$t$inert.gain[1:20]
btw_inertia_cum <- cumsum(btw_inertia)
text <- paste0(round(btw_inertia_cum[3]*100, 2), "% cumulated inertia \nfor 3 clusters")

data.frame(btw_inertia) %>% 
  mutate(cumul_inertia = cumsum(btw_inertia)) %>%
  ggplot(aes(x = 1:20)) + 
  geom_bar(aes(y = btw_inertia), 
           stat = "identity", 
           fill = "steelblue", 
           alpha = 0.6) +
  geom_vline(aes(xintercept = 3), 
             linetype = "dashed", 
             size = .7) +
  geom_label(aes(x = 6, 
                 y = .15, 
                 label = text), 
             size = 4.5) +
  labs(title = "", 
       x = "Number of clusters", 
       y = "Inertia") +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16))

# Cluster visualization -----
cluster_viz <- function(axes){
  fviz_cluster(res.hcpc,
               repel = TRUE,  
               axes = axes, 
               geom = "point", 
               show.clust.cent = TRUE, 
               palette = "jco",    
               ellipse = T, 
               alpha = .4, 
               main = "", 
               ggtheme = theme_minimal()) +
    theme(legend.position = "bottom", 
          legend.title = element_blank(), 
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14))
}

axes <- list(c(3, 4), 
             c(5, 6), 
             c(7, 8), 
             c(9, 10))
ggpubr::ggarrange(plotlist = lapply(axes, 
                                    cluster_viz), 
                  ncol = 2, nrow = 2, 
                  common.legend = T, 
                  legend = "bottom")

cluster_viz(axes = c(1, 2))

clust_prop <- res.hcpc$data.clust %>%
  pull(clust) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  select(Freq) %>%
  mutate(Freq = 100*Freq) %>%
  t()
colnames(clust_prop) <- c("Cluster 1", "Cluster 2", "Cluster 3")
rownames(clust_prop) <- "%"

save(clust_prop, 
     file = paste0(backup_path, "cluster_prop.RData"))

# Cluster description -----

desc_var <- res.hcpc$desc.var$category
save(desc_var, 
     file = paste0(backup_path, 
                   "hcpc_desc_var.Rdata"))

vars <- c(res.mca2$call$X %>% 
            colnames(), 
          "Tenure_Months", 
          "Churn_Value", 
          "CLTV") 

# parangons
para1 <- res.hcpc$desc.ind$para$`1` %>%
  names()
para2 <- res.hcpc$desc.ind$para$`2` %>%
  names()
para3 <- res.hcpc$desc.ind$para$`3` %>%
  names()

# bronze customers
para1_dat <- cleaned_data[para1, vars] 

# silver / high value customers
para2_dat <- cleaned_data[para2, vars]

# gold / loyal customers
para3_dat <- cleaned_data[para3, vars] 

save(para1_dat, 
     para2_dat, 
     para3_dat, 
     file = paste0(backup_path, "parangons_dat.RData"))

# number of customers in each cluster

desc.dat <- cleaned_data %>%
  mutate(cluster = res.hcpc$data.clust$clust)

table(desc.dat$cluster) %>%
  prop.table()

# quanti variables per cluster

boxplot_by_cluster <- function(var){
  desc.dat %>%
    ggplot() +
    geom_boxplot(aes_string(x = "cluster",
                            y = var, 
                            fill = "cluster",
                            color = "cluster"),
                 alpha = .5) +
    coord_flip() +
    scale_fill_jco() +
    scale_color_jco() +
    labs(title = "", 
         x = "", 
         y = var) +
    theme(legend.position = "none",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14))
}


quanti_vars <- c("Monthly_Charges", 
                 "CLTV", 
                 "Churn_Score")

lapply(quanti_vars, 
       boxplot_by_cluster)

boxplot_by_cluster(var = "Monthly_Charges")

# cat variables per cluster

barplot_by_cluster <- function(var){
  desc.dat %>%
    select(c(var, cluster)) %>%
    group_by_(var, "cluster") %>%
    dplyr::summarise(count = n()) %>% 
    group_by_(var) %>%
    mutate(perc = count/sum(count)) %>%
    ggplot(aes_string(x = var, 
                      y = "perc",
                      fill = "cluster")) +
    geom_bar(position="fill", 
             stat="identity",
             alpha = .6) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_jco() +
    scale_color_jco() +
    labs(x = "", 
         y = "", 
         title = var) 
}

cat_vars <- cleaned_data %>%
  select_if(is.factor) %>%
  select(-c("City", 
            "Zip_Code", 
            "Gender",
            "Churn_Reason")) %>%
  colnames() 

barplot_by_cluster(var = "Churn_Label") +
  ggtitle("")

ggpubr::ggarrange(plotlist = lapply(list("Senior_Citizen", 
                                         "Dependents"), 
                                    barplot_by_cluster), 
                  common.legend = T, 
                  legend = "bottom", 
                  nrow = 2) 

ggpubr::ggarrange(plotlist = lapply(list("Multiple_Lines", 
                                         "Internet_Service", 
                                         "Online_Security",
                                         "Online_Backup",
                                         "Device_Protection",
                                         "Tech_Support",
                                         "Streaming_TV",
                                         "Streaming_Movies"), 
                                    barplot_by_cluster), 
                  common.legend = T, 
                  legend = "bottom") 

ggpubr::ggarrange(plotlist = lapply(list("Contract",
                                         "Paperless_Billing",
                                         "Payment_Method",
                                         "Churn_Label"), 
                                    barplot_by_cluster), 
                  common.legend = T, 
                  legend = "bottom")
