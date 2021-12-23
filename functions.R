# ---------- Bookdown functions ----------

# --- Tables' layout ---

if (knitr::is_latex_output()) {
  mykable <- function(tab, transp = FALSE, digits =2, title=NULL, font_size = NULL, scale_tab = F,...){
    if( transp ){
      if(scale_tab == F){
        tab %>% t() %>% kable(caption=title, digits = digits, booktabs=TRUE, format = "latex",...) %>%
          kable_styling(full_width = F, position = "center", 
                        latex_options = c("striped", "condensed", "HOLD_position"),
                        font_size =  font_size)
      } else {
        tab %>% t() %>% kable(caption=title, digits = digits, booktabs=TRUE, format = "latex",...) %>%
          kable_styling(full_width = F, position = "center", 
                        latex_options = c("striped", "condensed", "HOLD_position","scale_down"),
                        font_size =  font_size)
      }
      
    } else {
      if(scale_tab == F){
        tab %>% kable(caption=title, digits = digits, booktabs=TRUE,...) %>%
          kable_styling(full_width = F, position = "center", 
                        latex_options = c("striped", "condensed", "HOLD_position"),
                        font_size =  font_size)
      } else {
        tab %>% kable(caption=title, digits = digits, booktabs=TRUE,...) %>%
          kable_styling(full_width = F, position = "center", 
                        latex_options = c("striped", "condensed", "HOLD_position","scale_down"),
                        font_size =  font_size)
      }
    }
  }
} else {
  mykable <- function(tab, transp = FALSE, digits = 2, title=NULL, font_size = NULL, ...){
    if(transp){
      tab %>% t() %>% kable(caption=title, digits = digits,...) %>%
        kable_styling(full_width = F, position = "center",
                      bootstrap_options = c("striped", "condensed"))  
    } else {
      tab %>% kable(caption=title, digits = digits, ...) %>%
        kable_styling(full_width = F, position = "center",
                      bootstrap_options = c("striped", "condensed"))
    }
  }
}

# --- Kaplan-Meier curves ---

plot_km <- function(
  treatment,
  surv_data, 
  risk_table = T, 
  font_size = 12
){
  km <- survfit(
    formula = formula( paste("Surv(Tenure_Months, Churn_Value) ~ ", treatment, sep = "") ), 
    data = surv_data
  )
  surv_plot <- ggsurvplot(
    km, 
    data = surv_data, 
    fun = "pct", 
    conf.int = T, 
    risk.table = risk_table, 
    ggtheme = theme_minimal(base_size = font_size), 
    palette = "Set2", 
    legend.labs = surv_data %>% 
      pull(treatment) %>%
      levels(), 
    legend.title = treatment
  )
  return (surv_plot)
}
