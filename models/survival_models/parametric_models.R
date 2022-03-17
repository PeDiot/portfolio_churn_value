# Doc -----
  # https://www.ms.uky.edu/~mai/Rsurv.pdf
  # https://www.rdocumentation.org/packages/survival/versions/3.2-7/topics/predict.survreg
  # https://www.r-bloggers.com/2019/06/parametric-survival-modeling/


# Exponential model
# Weibull model

# Setup ----------

## paths -----
dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

## packages -----
library(tidyverse)
library(data.table)
library(ggplot2)
library(tidyr)

library(survival)
library(flexsurv)     # for parametric survival models 
library(survminer)  
library(survcomp)     # for concordance index
library(muhaz)        # for kernel density estimator
library(lmtest)       # for LR test 

library(parallel)
library(doParallel)

theme_set(theme_minimal())

## Cox model -----
load(paste0(backup_path, "cox_final.RData"))
cox_formula <- final_cox$formula

## data -----
load(file = paste0(data_path, "telco_cleaned.RData"))
load(file = paste0(data_path, "train_test_data.RData"))

survdata_tr <- data.table(data_train)
survdata_te <- data.table(data_test)

## maximum number of months -----

max_num_months <- cleaned_data %>%
  pull(Tenure_Months) %>%
  max()

## functions -----

fit_surv_models <- function(
  survdata, 
  dists, 
  formula
){
  "Train multiple parametric survival models."
  
  res <- lapply(dists, 
                function(d){
                  flexsurvreg(
                    formula = formula, 
                    data = survdata, 
                    dist = d
                  ) 
                })
  names(res) <- dists
  res
  
}

visualize_estimated_hazards <- function(
  models, 
  dists_long,
  kernel_haz = NULL, 
  conf.int = F
){
  "Plot estimated hazard function for different models."
  
  parametric_haz <- vector(mode = "list", 
                           length = length(dists_long))
  for (i in 1:length(models)){
    parametric_haz[[i]] <- summary(models[[i]],
                                   type = "hazard", 
                                   ci = conf.int) %>%
      as.data.frame() %>%
      mutate(method = rep(dists_long[i], max_num_months))
  }
  
  parametric_haz <- rbindlist(parametric_haz)
  
  if ( !(is.null(kernel_haz)) ) {
    parametric_haz <- rbind(kernel_haz, parametric_haz)
    parametric_haz[, method := factor(method,
                                      levels = c("Kernel density",
                                                 dists_long))]
  }
  p <- ggplot(
    data = parametric_haz,
    aes(x = time, 
        y = est, 
        col = method, 
        linetype = method)
  ) +
    geom_line(size = .7) +
    scale_colour_brewer(palette = "Set2") +
    labs(x = "Number of months", 
         y = "Hazard", 
         title = "Estimated hazard function for different parametric models") +
    theme(legend.position = "none")
  
  if (conf.int == T) {
    p <- p +
      geom_ribbon(aes(x = time, 
                      ymin = lcl, 
                      ymax = ucl, 
                      col = method, 
                      fill = method), 
                  alpha = .2) +
      scale_fill_brewer(palette = "Set2") +
      theme(legend.position = "none")
  }

  plotly::ggplotly(p) 
  
}

predict_hazard <- function(mod, new_dat){
  "Return predicted hazard function for each individual for a given parametric model."
  
  preds <- predict(mod, 
                   newdata = new_dat, 
                   se.fit = T, 
                   conf.int = T, 
                   type = "hazard", 
                   times = max_num_months) 
    preds$CustomerID <- custIDs
  
  return(preds)
  
}

predict_survival <- function(mod, new_dat){
  "Return predicted survival function for each individual for a given parametric model."
  
  preds <- predict(mod, 
                   newdata = new_dat, 
                   se.fit = T, 
                   conf.int = T, 
                   type = "survival", 
                   times = 1:max_num_months)
  
  custIDs <- lapply(new_dat$CustomerID, 
                   function(id){ rep(id, max_num_months) }) %>%
    unlist()
  
  preds %>%
    unnest(cols = .pred) %>%
    mutate(CustomerID = custIDs)
  
}

compute_c_index <- function(time, event, preds){
  
  concordance.index(x = preds %>%
                      group_by(CustomerID) %>%
                      summarise(haz = mean(.pred)) %>%
                      pull(haz), 
                    surv.time = time, 
                    surv.event = event)
}


# Kernel density estimation -----

kernel_haz_tr <- muhaz(
  times=survdata_tr$Tenure_Months,
  delta=survdata_tr$Churn_Value, 
  min.time = 0, 
  max.time = max_num_months
)
kernel_haz_tr <- data.table(time = kernel_haz_tr$est.grid,
                            est = kernel_haz_tr$haz.est,
                            method = "Kernel density")


# Parametric estimation -----

dists <- c("exp", "weibull", "gompertz", "gamma", 
           "lognormal", "llogis", "gengamma")
dists_long <- c("Exponential", "Weibull (AFT)",
                "Gompertz", "Gamma", "Lognormal", "Log-logistic",
                "Generalized gamma")

## without covariates -----

fits <- fit_surv_models(survdata = survdata_tr,
                        dists = dists, 
                        formula = Surv(Tenure_Months, Churn_Value) ~ 1)

visualize_estimated_hazards(models = fits, 
                            dists_long = dists_long, 
                            kernel_haz = kernel_haz_tr)
# Notes: 
  # it looks like the chosen parametric forms do not fit the data
  # as the natural risk (kernel density) seems to be convex,
  # which is not the case for the chosen models

## with covariates ----------

parametric_fits <- fit_surv_models(survdata = survdata_tr,
                                   dists = dists, 
                                   formula = cox_formula) ; parametric_fits

### Predictions -----

# NOT RUN {

  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)  
  clusterExport(cl = cl, 
                varlist = list("parametric_fits", 
                               "predict_hazard", 
                               "survdata_tr", 
                               "max_num_months"),
                envir = environment()) 
  system.time(
    haz_preds_tr <- c(parLapply(cl,
                                X = parametric_fits,
                                fun = predict_hazard, 
                                new_dat = survdata_tr))
  )
  names(haz_preds_tr) <- dists
  stopCluster(cl)
  
  save(haz_preds_tr, 
       file = paste0(backup_path, "param_haz_pred_tr.RData"))
  
#} 
  
# NOT RUN {
  
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)  
  clusterExport(cl = cl, 
                varlist = list("parametric_fits", 
                               "predict_hazard", 
                               "survdata_te", 
                               "max_num_months"),
                envir = environment()) 
  system.time(
    haz_preds_te <- c(parLapply(cl,
                                X = parametric_fits,
                                fun = predict_hazard, 
                                new_dat = survdata_te))
  )
  names(haz_preds_te) <- dists
  stopCluster(cl)
  
  save(haz_preds_te, 
       file = paste0(backup_path, "param_haz_pred_te.RData"))
  
#} 

# MODEL COMPARISON -----
  
load(file = paste0(backup_path, "wei_predictions.RData"))
load(file = paste0(backup_path, "exp_predictions.RData"))

## C-index -----

exp.perftr <- compute_c_index(time = data_train$Tenure_Months, 
                              event = data_train$Churn_Value, 
                              preds = exp.haz.predstr) ; exp.perftr$c.index

exp.perfte <- compute_c_index(time = data_test$Tenure_Months, 
                              event = data_test$Churn_Value, 
                              preds = exp.haz.predste) ; exp.perfte$c.index

wei.perftr <- compute_c_index(time = data_train$Tenure_Months, 
                              event = data_train$Churn_Value, 
                              preds = wei.haz.predstr) ; wei.perftr$c.index

wei.perfte <- compute_c_index(time = data_test$Tenure_Months, 
                              event = data_test$Churn_Value, 
                              preds = wei.haz.predste) ; wei.perfte$c.index

## Visualize estimated risk -----

visualize_estimated_hazards(models = parametric_fits[c("exp", "weibull")], 
                            dists_long = c("Exponential", "Weibull"), 
                            conf.int = T)

visualize_estimated_hazards(models = parametric_fits, 
                            dists_long = dists_long, 
                            kernel_haz = kernel_haz_tr)

visualize_estimated_hazards(models = parametric_fits, 
                            dists_long = dists_long, 
                            conf.int = T)


