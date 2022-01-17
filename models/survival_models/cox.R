# ------------------- Cox Proportional Hazards Model ------------------- 

# C-index: the probability that, for a pair of randomly chosen comparable samples, 
# the sample with the higher risk prediction will experience an event (churn) before the other sample

# ---------- SET UP ----------

# ----- paths -----
dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/models/backup/survival/"

# ----- packages -----
library(tidyverse)
library(survival)
library(survminer)    # for ggsurvplot
library(ggplot2)

theme_set(theme_minimal())

# ----- data -----
load(file = paste(data_path, "train_test_data.RData", sep = ""))


# ---------- MODELS ---------- 

# NOT RUN {

# --- univariate models ---

# with Monthly_Charges
cox_monthly_charges <- coxph(
  formula = Surv(Tenure_Months, Churn_Value) ~ Monthly_Charges, 
  data = data_train
)
summary(cox_monthly_charges)
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
C <- cox_total_charges$concordance    # C-index ~ 87.1%
C["concordance"]

# }

# --- multivariate models ---

data_train_multivar <- data_train %>%
  dplyr::select(-c(
    CustomerID, City, Gender, Zip_Code, Multiple_Lines, 
    Churn_Label, Churn_Score, CLTV, Churn_Reason
  ))

# remove Multiple_Lines due to collinearity with Phone_Service


# NOT RUN {

# with every variables

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

# NOT RUN {

cox_signif_var <- update(
  cox_multivar,
  . ~ . - Latitude - Longitude - Senior_Citizen - Partner - 
    Device_Protection - Streaming_TV - Streaming_Movies - Paperless_Billing
) 

save(cox_signif_var, file = paste(backup_path, "cox_signif_var.Rdata", sep = ""))

# }

load(paste(backup_path, "cox_signif_var.Rdata", sep = ""))

summary(cox_signif_var)
C <- cox_signif_var$concordance          # C-index ~ 93.15%
C["concordance"]

ggforest(cox_signif_var, main = "", fontsize = 1)


# ---------- ESTIMATION ---------- 

# --- functions ---

get_conditional_survival <- function(survfit_obj, id){
  # Return number of months, conditional survival and confidence interval for a given individual
  
  surv <- survfit_obj$surv %>%
    as.data.frame() %>%
    dplyr::select(id) 
  num_months <- as.numeric(rownames(surv))
  surv_lower <- survfit_obj$lower %>%
    as.data.frame() %>%
    pull(id) 
  surv_upper <- survfit_obj$upper %>%
    as.data.frame() %>%
    pull(id) 
  surv <- surv %>%
    pull(id) 
  
  data.frame(
    num_months = num_months, 
    surv_lower = ifelse(is.na(surv_lower), 0, surv_lower), 
    surv = surv, 
    surv_upper = ifelse(is.na(surv_upper), 0, surv_upper)
  )
}

plot_conditional_survival <- function(
  surv_data, 
  id, 
  is_train = TRUE
){
  # Return a plot of conditional survival with confidence interval for a given individual
  
  color <- ifelse(
    test = isTRUE(is_train), 
    yes = "#2E9FDF", 
    no = "#BE7970"
  )
  
  surv_data %>%
    ggplot(aes_string(
      x = "num_months", 
      y = "surv"
    )) +
    geom_line(color = color, size = 1) +
    geom_ribbon(
      aes_string(ymin = "surv_lower", ymax = "surv_upper"), 
      alpha = .2
    ) +
    labs(
      title = paste("Estimated conditional survival for individual", id), 
      x = "Number of months", 
      y = "Survival"
    )
}

# --- train samples ---

ggsurvplot(
  fit = survfit(cox_multivar),
  data = data_train_multivar, 
  palette = "#2E9FDF",
  ggtheme = theme_minimal()
) 

cox_signif_var_survfit_tr <- survfit(
  cox_signif_var, 
  newdata = data_train
)

train_ids <- rownames(data_train)

plot_list <- lapply(
  
  train_ids[1:4], 
  
  function(id){
    surv_data <- get_conditional_survival(
      survfit_obj = cox_signif_var_survfit_tr, 
      id = id
    )
    plot_conditional_survival(surv_data, id)
  }
) 

ggpubr::ggarrange(plotlist = plot_list, nrow = 2, ncol = 2)

# --- test samples ---

cox_signif_var_survfit_te <- survfit(
  cox_signif_var, 
  newdata = data_test
)

test_ids <- rownames(data_test)

plot_list <- lapply(
  
  test_ids[1:4], 
  
  function(id){
    surv_data <- get_conditional_survival(
      survfit_obj = cox_signif_var_survfit_te, 
      id = id
    )
    plot_conditional_survival(surv_data, id, is_train = FALSE)
  }
) 

ggpubr::ggarrange(plotlist = plot_list, nrow = 2, ncol = 2)

get_conditional_survival(
  survfit_obj = cox_signif_var_survfit_te, 
  id = test_ids[100]
) %>% View()






