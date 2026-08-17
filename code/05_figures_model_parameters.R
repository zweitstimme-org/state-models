# 05_figures_model_parameters.R
# ------------------------------------------------------------------------------
# Purpose:  Plot posterior distributions of model coefficients (log-ratio
#           specification) by lead time and predictor set, with credible intervals.
# Inputs:   data/output/model/model_bayes.RDS (from 02_estimate_model.R)
# Outputs:  results/figures/model/fig_eval_bayes_par.pdf
# Run:      After step 2 (estimate model).
# ------------------------------------------------------------------------------

if (basename(getwd()) == "code") setwd("..")
ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "packages.R"))
ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "functions.R"))
RESULTS_FIG_MODEL <- file.path(ROOT, "results", "figures", "model")
DATA_OUT_MODEL <- file.path(ROOT, "data", "output", "model")
dir.create(RESULTS_FIG_MODEL, showWarnings = FALSE, recursive = TRUE)

# Load fitted Stan model objects (lr = log-ratio, lm = linear)
res <- readRDS(file.path(DATA_OUT_MODEL, "model_bayes.RDS"))
res_lr <- res[["lr"]]

# --- Reshape posterior draws to long format for plotting ---
extract_posterior_draws_long <- function(res_list) {
  draws_list <- list()
  for (name in names(res_list)) {
    draws_df <- as.data.frame(res_list[[name]]) %>%
      mutate(model = name)
    draws_long <- draws_df %>%
      pivot_longer(cols = -model, names_to = "parameter", values_to = "value")
    draws_list[[name]] <- draws_long
  }
  return(bind_rows(draws_list))
}

# Extract draws, parse model name (e.g. "8_all" -> lead + predictor set), label parameters (lead days dynamic)
posterior_draws_long_df <- extract_posterior_draws_long(res_lr) %>%
    separate(model, c("lead","model")) %>%
    mutate(model = ifelse(is.na(model),"polls",model)) %>%
    mutate(
      parameter = factor(case_when(
                grepl("^pollslr_[0-9]+$", parameter) ~ "Latent Support",
                grepl("^fed_trends_lr_[0-9]+$", parameter) ~ "Trend Federal Polls",
                parameter %in% c("votesharelr_l1") ~ "Vote Share last election",
                parameter %in% c("pm") ~ "Prime Minister",
                parameter %in% c("gov") ~ "Government Party",
                parameter %in% c("new_party") ~ "New Party",
                parameter %in% c("(Intercept)") ~ "Intercept",
                parameter %in% c("sigma") ~ "Sigma",
                TRUE ~ parameter),
                levels = rev(c("Latent Support","Intercept","Vote Share last election",
                           "Prime Minister", "Government Party", "New Party","Trend Federal Polls", "Sigma"))),
      type = factor(case_when(
              parameter == "Latent Support" ~ "Polls",
              TRUE ~ "Fundamentals"), levels = c("Polls", "Fundamentals")),
      lead = factor(paste(lead, "Tage"), levels = paste(sort(unique(as.numeric(lead))), "Tage")),
      model = factor(model, levels = c("all","polls", "fund"), labels = c("Polls + Fund.", "Polls", "Fund."))
    )

# Summary: 1%–99% and 1/6–5/6 quantiles and median per (model, lead, parameter)
summary_stats <- posterior_draws_long_df %>%
    group_by(model, lead, parameter, type) %>%
    summarize(
      lower = quantile(value, 0.01),
      upper = quantile(value, 0.99),
      lower2 = quantile(value, 1/6),
      upper2 = quantile(value, (1-1/6)),
      median = median(value),
      .groups = 'drop'
    )

# --- Violin plot + credible intervals by parameter, lead, and model ---
ggplot(posterior_draws_long_df, aes(x = parameter, y = value, fill = lead)) +
   geom_violin(trim = TRUE, alpha = 0.1, position = position_dodge(width = 0.9), scale = "width") +
    geom_linerange(data = summary_stats, aes(y = median, ymin = lower, ymax = upper, color = lead),
                    position = position_dodge(width = 0.9), linewidth = 0.8) +
    geom_linerange(data = summary_stats, aes(y = median, ymin = lower2, ymax = upper2, color = lead),
                    position = position_dodge(width = 0.9), linewidth = 2) +
    geom_point(data = summary_stats, aes(y = median, color = lead),
                   position = position_dodge(width = 0.9), size = 2) +
    coord_flip() +
    facet_grid(~ model, scales = "free_y") + 
    theme_minimal() +
    scale_fill_grey() +
    scale_color_grey() +
    theme(
      text = element_text(family = "Helvetica", size = 12, color = "black"),
      axis.text = element_text(size = 12),
      axis.text.y = element_text(margin = margin(r = 10)),  # Increase space between y-axis labels
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.minor.x = element_line(color = "gray", size = 0.25),
      panel.grid.major.x = element_line(color = "gray", size = 0.25),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      panel.border = element_rect(color = "gray", fill = NA, size = 0.5),  # Adding borders around facets
      legend.position = "bottom"
    ) +
    labs(
      title = "Posterior Distributions with 95% Credible Intervals by Model",
      x = "",
      y = "Posterior Distribution",
      fill = "Lead Time",
      color = "Lead Time"
    )
ggsave(filename = file.path(RESULTS_FIG_MODEL, "fig_eval_bayes_par.pdf"), width = 12, height = 6)

