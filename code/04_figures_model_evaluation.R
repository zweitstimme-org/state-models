# 04_figures_model_evaluation.R
# ------------------------------------------------------------------------------
# Purpose:  Produce model evaluation figures (MAE and RMSE by lead time and
#           predictor set) from the leave-one-out evaluation in step 2.
# Inputs:   data/output/model/model_bayes_eval.RDS (from 02_estimate_model.R)
# Outputs:  results/figures/model/fig_eval_bayes_mae.pdf
#           results/figures/model/fig_eval_bayes_rmse.pdf
# Run:      After step 2 (and optionally step 4 in run_everything.R).
# ------------------------------------------------------------------------------

if (basename(getwd()) == "code") setwd("..")
ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "packages.R"))
ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "functions.R"))
library("ggrepel")
RESULTS_FIG_MODEL <- file.path(ROOT, "results", "figures", "model")
DATA_OUT_MODEL <- file.path(ROOT, "data", "output", "model")
dir.create(RESULTS_FIG_MODEL, showWarnings = FALSE, recursive = TRUE)

# Load evaluation metrics (MAE, RMSE, bias, coverage by lead and predictor set)
eval_bayes <- readRDS(file.path(DATA_OUT_MODEL, "model_bayes_eval.RDS"))

# --- MAE and RMSE figures (log-ratio model only) ---

  # Map predictor-set codes to display labels
  eval_bayes <- eval_bayes %>%
    mutate(predictors = factor(predictors, level = c("all","polls", "fund"), labels = c("Polls + Fund.", "Polls", "Fund.") ))

  
  
lead_levels <- paste(sort(unique(as.numeric(eval_bayes$lead[eval_bayes$model_type == "logratio"]))), "Tage")
plot_data <- filter(eval_bayes, model_type == "logratio") %>%
    mutate(lead = paste(lead, "Tage"),
      lead = factor(lead, levels = lead_levels)) 

  ggplot(plot_data,
aes(x = mae, y = lead, col = predictors, shape = predictors)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_text_repel(aes(label = paste(round(mae, 2))),
                    nudge_y = 0.4,   
                    nudge_x = 0,    
                    size = 4,        
                    arrow = arrow(length = unit(0.001, "npc")),  
                    point.padding = 0.5) +  
    theme_minimal() +
    theme(
      text = element_text(family = "Helvetica", size = 12, color = "black"),
      axis.text = element_text(size = 14),
      # panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.minor.y = element_line(color = "gray", size = 0.1),
      panel.grid.major.y = element_line(color = "gray", size = 0.5),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_color_grey() + 
    scale_x_continuous(breaks = seq(0, max(plot_data$mae), by = 1),
                       minor_breaks = seq(0, max(plot_data$mae), by = 0.5)) +
    coord_flip() +
    labs(
      title = "Forecast Evaluation for State Elections 2024",
      subtitle = "Forecast based on Bayesian Linear Regression",
      x = "Mean Absolute Error",
      y = "Lead Time"
    )
  
  
  ggsave(filename = file.path(RESULTS_FIG_MODEL, "fig_eval_bayes_mae.pdf"), height = 6, width = 6*1.5)

# --- RMSE by lead and predictor set (same layout as MAE) ---
ggplot(plot_data,
      aes(x = rmse, y = lead, col = predictors, shape = predictors)) +
    geom_point(size = 5, alpha = 0.8) +
    geom_text_repel(aes(label = paste(round(rmse, 2))), 
                    nudge_y = 0.4,   
                    nudge_x = 0,    
                    size = 4,        
                    arrow = arrow(length = unit(0.001, "npc")),  
                    point.padding = 0.5) +  
    theme_minimal() +
    theme(
      text = element_text(family = "Helvetica", size = 12, color = "black"),
      axis.text = element_text(size = 14),
      # panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.minor.y = element_line(color = "gray", size = 0.1),
      panel.grid.major.y = element_line(color = "gray", size = 0.5),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_color_grey() + 
  scale_x_continuous(breaks = seq(0, max(plot_data$rmse), by = 1),
                     minor_breaks = seq(0, max(plot_data$rmse), by = 0.5)) +
    coord_flip() +
    labs(
      title = "Forecast Evaluation for State Elections 2024",
      subtitle = "Forecast based on Bayesian Linear Regression",
      x = "Root Mean Square Error",
      y = "Lead Time"
    )
  
  ggsave(filename = file.path(RESULTS_FIG_MODEL, "fig_eval_bayes_rmse.pdf"), height = 6, width = 6*1.5)
  