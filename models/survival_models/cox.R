# C-index: the probability that, for a pair of randomly chosen comparable samples, 
# the sample with the higher risk prediction will experience an event (churn) before the other sample

# Setup -------------------------------------------------------------------

## paths ------
dir <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/"
setwd(paste0(dir, "models/survival_models/")) 
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

## packages ------
library(tidyverse)
library(data.table)
library(survival)
library(survminer)    # for ggsurvplot
library(survcomp)
library(muhaz)        # for kernel density estimator
library(ggplot2)
library(plotly)

theme_set(theme_minimal())
source(paste0(dir, "colors.R"))

## data ------
load(file = paste(data_path, "train_test_data.RData", sep = ""))

# Training -------------------------------------------------------------------

## kernel density estimation -----

survdata <- data.table(data_train)
kernel_haz_est <- muhaz(
  times=survdata$Tenure_Months,
  delta=survdata$Churn_Value, 
  min.time = 0, 
  max.time = 72
)
kernel_haz <- data.table(time = kernel_haz_est$est.grid,
                         est = kernel_haz_est$haz.est,
                         method = "Kernel density")
haz_plot <- kernel_haz %>%
  ggplot(aes(x = time, 
             y = est)) +
  geom_line(size = .7) +
  labs(x = "Number of months", 
       y = "Churn risk", 
       title = "Observed churn risk", 
       subtitle = "Kernel density") ; haz_plot

## Univariate Cox models -----

# NOT RUN {
  
  # with Monthly_Charges
  cox_monthly_charges <- coxph(
    formula = Surv(Tenure_Months, Churn_Value) ~ Monthly_Charges, 
    data = data_train
  )
  summary(cox_monthly_charges)
  cox.zph(cox_monthly_charges)
  cox_monthly_charges_risk <- predict(cox_monthly_charges, type = "risk")
  
  cox_monthly_charges$concordance
  
  C <- cox_monthly_charges$concordance   # C-index ~ 50.2% (non-informative model : random predictions)
  C["concordance"]
  
  # with Total_Charges
  cox_total_charges <- coxph(
    formula = Surv(Tenure_Months, Churn_Value) ~ Total_Charges, 
    data = data_train
  )
  summary(cox_total_charges)
  cox.zph(cox_total_charges)
  C <- cox_total_charges$concordance    # C-index ~ 87.1%
  C["concordance"]
  
  cox_total_charges_pred <- predict(
    cox_total_charges, 
    newdata = data_test, 
    type = "risk"
  )
  C_test <- concordance.index(
    x = cox_total_charges_pred,
    surv.time = data_test$Tenure_Months,
    surv.event = data_test$Churn_Value
  )
  C_test$c.index

# }


## Multivariate models ------

data_train_multivar <- data_train %>%
  dplyr::select(-c(
    CustomerID, City, Gender, Zip_Code, Multiple_Lines, 
    Churn_Label, Churn_Score, CLTV, Churn_Reason
  ))

# remove Multiple_Lines due to collinearity with Phone_Service

### With every variables -----

# NOT RUN {
  
  system.time(
    cox_multivar <- coxph(
      formula = Surv(Tenure_Months, Churn_Value) ~ ., 
      data = data_train_multivar
    )
  )
  
  save(cox_multivar, file = paste(backup_path, "cox_multivar.Rdata", sep = ""))

# }

load(paste(backup_path, "cox_multivar.Rdata", sep = ""))

summary(cox_multivar)
stargazer::stargazer(
  cox_multivar,
  type = "text"
)
C <- cox_multivar$concordance          # C-index ~ 93.16%
C["concordance"]

cox_haz_plt <- ggforest(cox_multivar, main = "", fontsize = 1)
cox_haz_plt     

# remove variables with no significant influence on the churn hazard 

### With significant variables -----

# NOT RUN {

  cox_signif_var <- update(
    cox_multivar,
    . ~ . - Latitude - Longitude - Senior_Citizen - Partner - 
      Device_Protection - Streaming_TV - Streaming_Movies - Paperless_Billing - Total_Charges
  ) 
  
  save(cox_signif_var, file = paste(backup_path, "cox_signif_var.Rdata", sep = ""))

# }

load(paste(backup_path, "cox_signif_var.Rdata", sep = ""))

#### Testing proportional Hazards assumption -----

test.ph <- cox.zph(fit = cox_signif_var)
print(test.ph)
ggcoxzph(test.ph)                             # non-proportional hazards 

cox_hp_tab <- test.ph$table
colnames(cox_hp_tab) <- c("Chisq", "Df", "pvalue") 
save(cox_hp_tab, 
     file = paste0(backup_path, "cox_hp_table.RData"))

#### Testing influential observations -----

ggcoxdiagnostics(fit = cox_signif_var,
                 type = , 
                 linear.predictions = TRUE)


sum <- summary(cox_signif_var) ; sum
C <- cox_signif_var$concordance          # C-index ~ 93.15%
C["concordance"]

ggforest(cox_signif_var, main = "", fontsize = 1.5)

lr_test <- lmtest::lrtest(cox_signif_var)

cox_metrics <- list(
  data.frame(Model = cox_signif_var$loglik[2],
             Constrained = cox_signif_var$loglik[1], 
             pvalue = lr_test$`Pr(>Chisq)`[2]) %>%
    `rownames<-`(""),
  data.frame(Statistic = sum$logtest["test"],
             Df = sum$logtest["df"],
             pvalue = sum$logtest["pvalue"]) %>%
    `rownames<-`(""), 
  data.frame(C_index = cox_signif_var$concordance["concordance"],
             Std = cox_signif_var$concordance["std"]) %>%
    `rownames<-`("")
)
names(cox_metrics) <- c("lrtest", "logranktest", "concordance")
                          
save(cox_metrics, 
     file = paste0(backup_path, "cox_metrics.Rdata"))

# Prediction --------------------------------------------------------------

risk_pred_tr <- predict(
  cox_signif_var, 
  type = "risk", 
  se.fit = T
)

risk_pred_te <- predict(
  cox_signif_var, 
  newdata = data_test, 
  type = "risk", 
  se.fit = T
)

c_index_tr <- concordance.index(x = risk_pred_tr$fit, 
                                surv.time = data_train$Tenure_Months, 
                                surv.event = data_train$Churn_Value)
c_index_tr$c.index

c_index_te <- concordance.index(x = risk_pred_te$fit, 
                  surv.time = data_test$Tenure_Months, 
                  surv.event = data_test$Churn_Value)
c_index_te$c.index

# Final Cox model ---------------------------------------------------------

load(file = paste(data_path, "telco_cleaned.RData", sep = "/"))

## Fit model -----

# NOT RUN {
  formula <- Surv(Tenure_Months, Churn_Value) ~ Dependents + Phone_Service + 
    Internet_Service + Online_Security + Online_Backup + Tech_Support + 
    Contract + Payment_Method + Monthly_Charges
  
  final_cox <- coxph(
    formula = formula, 
    data = cleaned_data
  )
  
  save(final_cox, 
       file = paste(backup_path, "cox_final.RData", sep = "/")) 
#}
  
load(file = paste(backup_path, "cox_final.RData", sep = "/"))

summary(final_cox)

## Data viz -----

risk_preds <- predict(final_cox, 
                      se.fit = T)

risk_plot <- data.frame(num_months = cleaned_data$Tenure_Months, 
           risk = risk_preds$fit, 
           se = risk_preds$se.fit) %>%
  mutate(lower_bound = risk - 1.96*se, 
         upper_bound = risk + 1.96*se) %>%
  group_by(num_months) %>%
  summarise(across(-se,
                   mean,
                   na.rm = T)) %>%
  ggplot(aes(x = num_months, 
             y = risk)) +
  geom_line(size = .8, 
            color = risk_col) +
  geom_ribbon(aes(ymin = lower_bound, 
                  ymax = upper_bound), 
              fill = "grey70", 
              alpha = .3) +
  labs(x = "Number of months", 
       y = "", 
       title = "Churn hazard function", 
       subtitle = "with 95% confidence interval") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        title = element_text(size = 14))

surv_plt <- ggsurvplot(fit = survfit(final_cox), 
                       data = cleaned_data, 
                       palette = surv_col, 
                       conf.int = T, 
                       ggtheme = theme_minimal())
surv_plt <- surv_plt$plot +
  labs(x = "Number of months",
       y = "", 
       title = "Customer survival probability", 
       subtitle = "with 95% confidence interval") +
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        title = element_text(size = 14))

cumhaz_plt <- ggsurvplot(fit = survfit(final_cox), 
                         data = cleaned_data, 
                         fun = "cumhaz", 
                         palette = "steelblue", 
                         conf.int = T, 
                         ggtheme = theme_minimal())

cumhaz_plt<- cumhaz_plt$plot +
  labs(x = "Number of months", 
       y = "", 
       title = "Cumulative churn hazard", 
       subtitle = "with 95% confidence interval") +
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        title = element_text(size = 14)) 

ggpubr::ggarrange(risk_plot, 
          surv_plt, 
          cumhaz_plt, 
          nrow = 2, ncol = 2) 
