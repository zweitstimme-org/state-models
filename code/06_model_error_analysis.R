# 06_model_error_analysis.R
# ------------------------------------------------------------------------------
# Purpose:  Analyse leave-one-out prediction errors: absolute error (AE) vs
#           vote share, by lead time and model type (log-ratio vs linear).
#           Highlights large errors and optionally joins structural data.
# Inputs:   data/output/model/model_bayes_errors.RDS (from 02_estimate_model.R)
#           data/output/05_full_data.RData (for state, year, etc. in labels)
# Outputs:  Figures and tables in memory; can be extended to save plots.
# Run:      After step 2; called from run_everything.R as step 5 (Error analysis).
# ------------------------------------------------------------------------------

if (basename(getwd()) == "code") setwd("..")
ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "packages.R"))
ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "functions.R"))
DATA_OUT_MODEL <- file.path(ROOT, "data", "output", "model")

# Load error data (party, elec_ind, lead, predictors, dv, fit, ae) and join full data for labels
load(file.path(ROOT, "data", "output", "05_full_data.RData"))
data_structural <- full_data
df_error <- readRDS(file.path(DATA_OUT_MODEL, "model_bayes_errors.RDS"))
df_error <- left_join(df_error, data_structural)

# --- Plot: absolute error vs vote share, full model only (predictors == "all") ---
# Facets: lead time (rows) x model type (columns: lr = log-ratio, lm = linear). Points with AE > 0.1 labelled.
df_error %>%
  filter(predictors == "all") %>%
  ggplot(aes(x = voteshare, y = ae)) +
  geom_point() +
  geom_smooth() +
  geom_text(
    data = df_error %>% filter(predictors == "all", ae > 0.1),
    aes(label = paste(state, year, party, sep = " ")),
    vjust = -0.5,
    hjust = 0.5,
    check_overlap = TRUE
  ) +
  facet_grid(lead ~ dv) +
  theme_bw()

# --- Optional: list cases with large errors (log-ratio, full model, AE > 0.1) ---
# df_error %>%
#   filter(predictors == "all", dv == "lr", ae > 0.1) %>%
#   select(party, elec_ind, lead, voteshare, fit, ae, starts_with("polls_"), pm, gov)


