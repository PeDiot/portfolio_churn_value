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
library(Factoshiny)

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

## screeplot -----

fviz_screeplot(res.mca2, 
               geom = "line", 
               linecolor = "steelblue", 
               addlabels = T, 
               ylim = c (0, 25), 
               main = "")

mca_eig_data <- res.mca2$eig %>%
  as.data.frame() %>%
  mutate(Dimensions = str_sub(rownames(.), -2, -1) %>%
           as.numeric()) %>%
  dplyr::rename(perc_var = `percentage of variance`,
         cum_perc_var = `cumulative percentage of variance`)

scree_plot <- fviz_eig(res.mca2, ncp = 12,
                       addlabels = TRUE, hjust = -0.3,
                       geom = "line") +
  geom_line(size = 1, color = "steelblue") +
  geom_line(
    data = mca_eig_data[1:12, ], 
    aes(x = Dimensions, y = cum_perc_var/5),
    size = .8, 
    color = "purple",
    linetype = "dashed", 
    alpha = .7
  ) +
  geom_vline(xintercept = 10,
             color = "black",
             linetype = "dashed") +
  geom_label(
    data = mca_eig_data[10, ], 
    aes(
      x = Dimensions - 1.5,
      y = 17,
      label = paste(round(cum_perc_var, 1),"% \ncumulated variance", sep = "")
    ),
    size = 4,
    color = "black",
    alpha = .7
  ) +
  scale_y_continuous(
    name = "% variance explained",
    sec.axis = sec_axis(~.*5, name = "Cumulative % variance")
  ) +
  ggtitle(" ") +
  theme(axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(color = "steelblue", size = 16),
        axis.title.y.right = element_text(color = "purple", size = 16), 
        axis.text.y = element_text(color = "steelblue", size = 16),
        axis.text.y.right = element_text(color = "purple", size = 16))


axes <- c(1, 2)

## biplot -----
fviz_mca_biplot(res.mca2, 
                axes = axes,
                geom.ind = "point", 
                geom.var = c("point", "text"),
                repel = TRUE,
                alpha = .2, 
                ggtheme = theme_minimal())

ggpubr::ggarrange(
  plotlist = lapply(list(c(1, 2), 
                         c(3, 4), 
                         c(5, 6), 
                         c(7, 8), 
                         c(9, 10)), 
                    function(axes){
                      fviz_mca_biplot(res.mca2, 
                                      axes = axes,
                                      geom.ind = "point", 
                                      geom.var = c("point", "text"),
                                      repel = TRUE,
                                      alpha = .2, 
                                      ggtheme = theme_minimal())
                    }), 
  nrow = 2, 
  ncol = 3
)

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
ncp <- res.mca2$call$ncp

ggpubr::ggarrange(
  plotlist = lapply(seq(1, ncp), 
                    function(ax){
                      fviz_cos2(res.mca2, 
                                choice = "var", 
                                axes = ax) 
                    }), 
  nrow = 2, ncol = 5
)

## contribution -----
ggpubr::ggarrange(
  plotlist = lapply(seq(9, 10), 
                    function(ax){
                      p <- fviz_contrib(res.mca2, 
                                        choice = "var", 
                                        alpha = .5, 
                                        axes = ax) 
                      p +
                        ggtitle(paste("Dim", ax)) +
                        theme(axis.text = element_text(size = 12))
                    }), 
  nrow = 1, ncol = 2
)
