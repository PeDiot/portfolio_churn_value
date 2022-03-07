# Tutorial: https://luminwin.github.io/randomForestSRC/articles/getstarted.html

# Setup -------------------------------------------------------------------

## paths -----
dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

## packages -----
library(tidyverse)
library(survival)
library(survminer)    # for ggsurvplot
library(survcomp)
library(randomForestSRC)
library(ggplot2)

theme_set(theme_minimal())

##  data -----
load(file = paste(data_path, "train_test_data.RData", sep = ""))
load(file = paste(backup_path, "cox_final.RData", sep = ""))

trainIDs <- data_train %>%
  pull(CustomerID)
testIDs <- data_test %>%
  pull(CustomerID)


# Model -------------------------------------------------------------------

## Fit the model -----

# NOT RUN {

  formula <- cox_final$formula
  
  system.time(
    rsf <- rfsrc(
      formula = formula, 
      data = data_train
    )
  )
  
  system.time(
    tune.rsf <- tune(
      formula = formula, 
      data = data_train,
      doBest = TRUE
    )
  )
  
  rsf.best <- rfsrc(
    formula = formula, 
    data = data_train,
    nodesize = tune.rsf$optimal["nodesize"],
    mtry = tune.rsf$optimal["mtry"]
  )
  
  save(rsf.best, 
       file = paste(backup_path, "rsf.Rdata", sep = "/"))
  
# }
  
load(file = paste(backup_path, "rsf.Rdata", sep = "/"))

## Importance of variables -----

imp <- vimp(object = rsf.best)
data.frame(imp = imp$importance) %>%
  arrange(desc(imp)) 

vars <- rsf.best$xvar.names
plot.variable(x = rsf.best, 
              xvar.names = vars, 
              partial = T)

## Quality of estimation -----

get.cindex(time = data_train$Tenure_Months,
           censoring = data_train$Churn_Value, 
           predicted = rsf.best$predicted.oob)

get.cindex(time = data_train$Tenure_Months,
           censoring = data_train$Churn_Value, 
           predicted = rsf.best$predicted)


# Risk and survival prediction --------------------------------------------

pred_tr <- predict(object = rsf.best)
pred_te <- predict(object = rsf.best, 
                       newdata = data_test)

surv_tr <- pred_tr$survival
rownames(surv_tr) <- trainIDs
surv_te <- pred_te$survival
rownames(surv_te) <- testIDs

custID <- trainIDs[1000] %>%
  as.character()

churn_time <- data_train %>%
  filter(CustomerID == custID) %>%
  pull(Tenure_Months)

data.frame(t = 1:72, 
           surv = surv_tr[custID, ]) %>%
  ggplot(aes(x = t,
             y = surv)) +
  geom_step(color = "#2E9FDF", 
            size = .8) +
  geom_vline(xintercept = churn_time, 
             linetype = "dashed", 
             size = .7) +
  labs(x = "Number of months", 
       y = "Survival", 
       title = paste("Estimated survival for customer", custID), 
       subtitle = paste("Observed duration =", churn_time))



