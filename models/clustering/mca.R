# ----------- Multiple Correspondance Analysis -----------

# ----- Setup -----

dir <- "./models/clustering/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/clustering/"

load(paste(data_path, "telco_cleaned.RData", sep = "/"))

library(tidyverse)
library(plyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)

theme_set(theme_minimal()) 


# data -----

vars <- c("Senior_Citizen", 
          "Dependents", 
          "Internet_Service", 
          "Online_Security",  
          "Online_Backup",
          "Device_Protection",
          "Tech_Support",
          "Streaming_TV", 
          "Streaming_Movies",
          "Contract", 
          "Paperless_Billing",
          "Payment_Method", 
          "Monthly_Charges")

mca_dat <- cleaned_data %>%
  select(all_of(vars))

# MCA ---

# NOT RUN {

  res.mca <- MCA(X = mca_dat, quanti.sup = 13)
  
  fviz_screeplot(res.mca, 
                  addlabels = T, 
                  ylim = c (0, 25))
  
  eig.val <- get_eigenvalue(res.mca) %>%
    as.data.frame() %>%
    mutate(ncp = 1:nrow(.))
  
  # number of components to keep (80% cumulative variance) 
  ncp <- eig.val %>%
    filter(cumulative.variance.percent >= 80) %>%
    head(1) %>%
    pull(ncp)
  
  res.mca$quanti.sup
  
  res.mca2 <- MCA(X = mca_dat, 
                  quanti.sup = 13, 
                  ncp = ncp, 
                  graph = F)
  
  save(res.mca2, 
       file = paste0(backup_path, "res.mca.RData"))
  
  coord.mca <- res.mca2$ind$coord %>%
    as.data.frame()
  
  clust_dat <- coord.mca %>%
    mutate(Monthly_Charges = cleaned_data$Monthly_Charges,
           Customer_ID = cleaned_data$CustomerID)
  
  save(clust_dat, 
       file = paste0(data_path, "clust_data.RData"))
  
# }
  
load(file = paste0(backup_path, "res.mca.RData"))


# Analysis -----

axes <- c(9, 10)

## biplot -----
fviz_mca_biplot(res.mca2, 
                axes = axes,
                geom.ind = "point", 
                geom.var = c("point", "text"), 
                repel = TRUE,
                alpha = .2, 
                ggtheme = theme_minimal())

## variables -----
fviz_mca_var(res.mca2,
             axes = axes,
             choice = "mca.cor", 
             repel = TRUE,
             ggtheme = theme_minimal())

# Dim1: additionnal services + internet service
# Dim2: type of contract, payment method



## categories -----
fviz_mca_var(res.mca2,
             axes = axes,
             repel = TRUE,
             ggtheme = theme_minimal())

## quality of representation -----
ggpubr::ggarrange(
  fviz_cos2(res.mca2, 
            choice = "var", 
            fill = "purple",
            color = "purple",
            axes = 1), 
  fviz_cos2(res.mca2, 
            choice = "var", 
            axes = 2), 
  nrow = 2
)

## contribution -----
ggpubr::ggarrange(
  fviz_contrib(res.mca2, 
            choice = "var", 
            fill = "purple",
            color = "purple",
            axes = 1), 
  fviz_contrib(res.mca2, 
            choice = "var", 
            axes = 2), 
  nrow = 2
)
