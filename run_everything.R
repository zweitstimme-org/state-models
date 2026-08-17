###########################
# Run full pipeline: data → model → validation → evaluation → error analysis → forecast → forecast figures
# Run from project root:  Rscript run_everything.R
###########################

# ------------------------------------------------------------------------------
# MANUAL INPUTS (update these files by hand when new data are available)
# ------------------------------------------------------------------------------
#
#   data/input/01_state-election-results.csv   State election results (Landtagswahlen)
#   data/input/02_state-cabinets.csv           State cabinet/coalition data (PM, gov participation)
#   data/input/03_federal-election-results.csv Federal election results (Bundestag)
#
# After editing, run this script to rebuild all derived data, re-estimate the model,
# run a short validation, and produce forecasts for the elections defined below.
#
# ------------------------------------------------------------------------------
# LEADS: election date − last poll Stand (per land), not calendar today
# ------------------------------------------------------------------------------
# Lead days are set from ELECTIONS_TO_FORECAST and each land's newest poll: e.g. if ST's
# last poll is 38 days before the election, we use 38. Daily re-runs without new polls
# keep the same lead. We train/evaluate on these exact lead days and pick the matching model.
# ONLY_ALL_MODELS: if TRUE (default), estimate only "<lead>_all" per lead.
# ONLY_POLLS_MODELS: if TRUE, estimate only "<lead>_polls" per lead (predictor = poll only; no fundamentals or federal trends).
# When ONLY_POLLS_MODELS is TRUE, ONLY_ALL_MODELS is ignored and only polls-only models are run.

# Website / production forecasts match BW/RP 2026: exact-lead polls-only models.
ONLY_ALL_MODELS <- FALSE
ONLY_POLLS_MODELS <- TRUE

# ------------------------------------------------------------------------------
# MODEL SPECIFICATION(S) TO ESTIMATE
# ------------------------------------------------------------------------------
# FALSE = only log-ratio (logit vote share, predictions in [0,1]). TRUE = also estimate
# linear specification (raw vote share). Forecasts and figures use log-ratio only.

ESTIMATE_LINEAR <- FALSE

# ------------------------------------------------------------------------------
# ELECTIONS TO FORECAST (edit this list to add/remove elections)
# ------------------------------------------------------------------------------
# Format: "state_electiondate" (state = 2-letter land code, date = YYYY-MM-DD).
ELECTIONS_TO_FORECAST <- c(
  "st_2026-09-06",   # Sachsen-Anhalt 2026-09-06
  "mv_2026-09-20",   # Mecklenburg-Vorpommern 2026-09-20
  "be_2026-09-20"    # Berlin 2026-09-20
)

# Set TRUE to skip step 1 and use existing data/output
# From shell: Rscript -e 'SKIP_BUILD_DATA <- FALSE; source("run_everything.R")' to force rebuild
if (!exists("SKIP_BUILD_DATA")) SKIP_BUILD_DATA <- TRUE

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
if (basename(getwd()) %in% c("code", "data")) setwd("..")
ROOT <- getwd()
options(script_root = ROOT, elections_to_forecast = ELECTIONS_TO_FORECAST)
DATA_OUT        <- file.path(ROOT, "data", "output")
DATA_OUT_MODEL  <- file.path(ROOT, "data", "output", "model")
DATA_OUT_FORECAST <- file.path(ROOT, "data", "output", "forecast")
FORECAST_DATA_PATH <- file.path(DATA_OUT, "forecast_data.RData")

# Lead days = election − last poll Stand (not calendar today). Saved so 01_build_data and 02 use them.
source(file.path(ROOT, "code", "auxilary", "functions.R"))
polls_csv <- file.path(DATA_OUT, "01_state-polls.csv")
lead_days <- lead_days_from_last_polls(
  ELECTIONS_TO_FORECAST,
  api_base = Sys.getenv("POLLING_API_BASE", "https://api.fasttrack29.com"),
  polls_csv = if (file.exists(polls_csv)) polls_csv else NULL
)
save(lead_days, file = file.path(DATA_OUT, "lead_days.RData"))
elections_to_forecast <- ELECTIONS_TO_FORECAST
save(elections_to_forecast, file = file.path(DATA_OUT, "elections_to_forecast.RData"))
message("Lead days (from last-poll Stand): ", paste(lead_days, collapse = ", "))

# ------------------------------------------------------------------------------
# 1. Build data (skip if SKIP_BUILD_DATA is TRUE)
# ------------------------------------------------------------------------------
if (!SKIP_BUILD_DATA) {
  message("\n========== 1. BUILD DATA ==========")
  message("Inputs (update manually when needed):")
  message("  ", file.path(ROOT, "data", "input", "01_state-election-results.csv"))
  message("  ", file.path(ROOT, "data", "input", "02_state-cabinets.csv"))
  message("  ", file.path(ROOT, "data", "input", "03_federal-election-results.csv"))
  options(keep_env = TRUE)  # so packages.R does not rm() and wipe 01's env
  source(file.path(ROOT, "code", "01_build_data.R"), local = new.env())
  options(keep_env = FALSE)
} else {
  message("\n========== 1. BUILD DATA (skipped) ==========")
}

# ------------------------------------------------------------------------------
# 2. Estimate model
# ------------------------------------------------------------------------------
if (!exists("ONLY_ALL_MODELS")) ONLY_ALL_MODELS <- TRUE
if (!exists("ONLY_POLLS_MODELS")) ONLY_POLLS_MODELS <- FALSE
# Lead days from file (set above; 01 saves them with full_data so 02 can load from 05_full_data.RData)
load(file.path(DATA_OUT, "lead_days.RData"))
message("\n========== 2. ESTIMATE MODEL ==========")
message("Lead days to model/evaluate: ", paste(lead_days, collapse = ", "))
message("Only _all models: ", ONLY_ALL_MODELS, "; Only polls models: ", ONLY_POLLS_MODELS)
Sys.setenv(MODEL_LEADS = paste(lead_days, collapse = ","))
Sys.setenv(MODEL_ONLY_ALL = if (ONLY_ALL_MODELS) "1" else "0")
Sys.setenv(MODEL_POLLS_ONLY = if (ONLY_POLLS_MODELS) "1" else "0")
# Run estimate in a fresh R process so ROOT/packages.R env issues don't occur when sourced
est_script <- file.path(getwd(), "code", "02_estimate_model.R")
status_est <- system2("Rscript", est_script, stdout = TRUE, stderr = TRUE)
if (!is.null(attr(status_est, "status")) && attr(status_est, "status") != 0L) {
  cat(paste(status_est, collapse = "\n"), "\n")
  stop("Estimate model failed with status ", attr(status_est, "status"))
}

# ------------------------------------------------------------------------------
# 3. Model validation (sanity check) — only if step 2 produced eval
# ------------------------------------------------------------------------------
message("\n========== 3. MODEL VALIDATION ==========")
eval_path <- file.path(DATA_OUT_MODEL, "model_bayes_eval.RDS")
if (!file.exists(eval_path)) {
  message("No model_bayes_eval.RDS found; skipping validation (step 2 may have run with no eval variants).")
} else {
  suppressPackageStartupMessages(library("dplyr"))
  eval_df <- readRDS(eval_path)
  best_rmse <- min(eval_df$rmse, na.rm = TRUE)
  best_mae  <- eval_df$mae[which.min(eval_df$rmse)]
  coverage_ok <- eval_df %>% dplyr::filter(!is.na(coverage)) %>% dplyr::pull(coverage)
  message("Best RMSE (vote share %): ", round(best_rmse, 2))
  message("Best MAE (vote share %):  ", round(best_mae, 2))
  if (best_mae > 6) warning("MAE > 6 percentage points for best model; consider checking data or specification.")
  if (any(coverage_ok < 0.7 | coverage_ok > 1)) warning("Some coverage outside [0.7, 1]; intervals may be mis-calibrated.")
  message("Validation done.\n")
}

# ------------------------------------------------------------------------------
# 4. Evaluation — show evaluation table, then run scripts 04 and 05 (figures)
# ------------------------------------------------------------------------------
message("\n========== 4. EVALUATION ==========")
if (!file.exists(eval_path)) {
  message("No model_bayes_eval.RDS found; skipping evaluation.")
} else {
  suppressPackageStartupMessages(library("dplyr"))
  eval_df <- readRDS(eval_path)
  eval_display <- eval_df %>%
    dplyr::mutate(lead = paste(lead, "Tage"), predictors = ifelse(is.na(predictors), "polls", predictors)) %>%
    dplyr::select(lead, predictors, model_type, mae, rmse, bias, coverage, dplyr::any_of("mean_interval_pp")) %>%
    dplyr::arrange(rmse)
  message("Evaluation by lead and predictor set (MAE, RMSE, bias, coverage; mean_interval_pp = mean interval width in pp):")
  print(eval_display)
  message("")
  message("4a. Model evaluation figures (04_figures_model_evaluation.R)...")
  options(keep_env = TRUE)
  source(file.path(ROOT, "code", "04_figures_model_evaluation.R"), local = new.env())
  options(keep_env = FALSE)
  message("Model evaluation figures done.")
}
# 4b. Model parameter figures (05) — only if model_bayes.RDS exists
model_path_4 <- file.path(DATA_OUT_MODEL, "model_bayes.RDS")
if (file.exists(model_path_4)) {
  message("4b. Model parameter figures (05_figures_model_parameters.R)...")
  options(keep_env = TRUE)
  source(file.path(ROOT, "code", "05_figures_model_parameters.R"), local = new.env())
  options(keep_env = FALSE)
  message("Model parameter figures done.\n")
}

# ------------------------------------------------------------------------------
# 5. Error analysis — run script 06 (only if step 2 produced errors)
# ------------------------------------------------------------------------------
message("\n========== 5. ERROR ANALYSIS ==========")
errors_path <- file.path(DATA_OUT_MODEL, "model_bayes_errors.RDS")
if (!file.exists(errors_path)) {
  message("No model_bayes_errors.RDS found; skipping error analysis.")
} else {
  options(keep_env = TRUE)
  source(file.path(ROOT, "code", "06_model_error_analysis.R"), local = new.env())
  options(keep_env = FALSE)
  message("Error analysis done.\n")
}

# ------------------------------------------------------------------------------
# 6. Forecasts — only if model was estimated (model_bayes.RDS exists)
# ------------------------------------------------------------------------------
message("\n========== 6. FORECASTS ==========")
ROOT <- getOption("script_root")
if (is.null(ROOT)) ROOT <- getwd()
ELECTIONS_TO_FORECAST <- getOption("elections_to_forecast")
if (is.null(ELECTIONS_TO_FORECAST)) stop("ELECTIONS_TO_FORECAST lost (e.g. after step 5). Set options(elections_to_forecast = ...) at top of run_everything.R.")
DATA_OUT <- file.path(ROOT, "data", "output")
FORECAST_DATA_PATH <- file.path(DATA_OUT, "forecast_data.RData")
model_path <- file.path(ROOT, "data", "output", "model", "model_bayes.RDS")
if (!file.exists(model_path)) {
  message("No model_bayes.RDS found; skipping forecasts. Run step 2 with at least one model variant.")
} else {
  message("Elections to forecast: ", paste(ELECTIONS_TO_FORECAST, collapse = ", "))
  elections_to_forecast <- ELECTIONS_TO_FORECAST
  save(elections_to_forecast, file = file.path(DATA_OUT, "elections_to_forecast.RData"))
  if (!file.exists(FORECAST_DATA_PATH)) {
    message("Forecast data not yet built. Running code/03_forecast.R (builds forecast data + state forecasts).")
  }
  path_03 <- file.path(ROOT, "code", "03_forecast.R")
  options(keep_env = TRUE, script_root = ROOT)  # script_root survives packages.R rm(list=ls())
  source(path_03, local = FALSE)
  options(keep_env = NULL, script_root = NULL)
  ROOT <- getwd()
  DATA_OUT <- file.path(ROOT, "data", "output")
  DATA_OUT_MODEL <- file.path(ROOT, "data", "output", "model")
  DATA_OUT_FORECAST <- file.path(ROOT, "data", "output", "forecast")
  message("")
}

# ------------------------------------------------------------------------------
# 7. Forecast figures
# ------------------------------------------------------------------------------
message("\n========== 7. FORECAST FIGURES ==========")
ROOT <- getOption("script_root")
if (is.null(ROOT)) ROOT <- getwd()
DATA_OUT_FORECAST <- file.path(ROOT, "data", "output", "forecast")
if (!file.exists(file.path(DATA_OUT_FORECAST, "fcst_state.Rdata"))) {
  message("No fcst_state.Rdata found; skipping forecast figures. Run step 6 first.")
} else {
  options(keep_env = TRUE)
  source(file.path(ROOT, "code", "07_figures_forecast.R"), local = new.env())
  options(keep_env = FALSE)
  message("Forecast figures done.\n")
}

message("\n========== DONE ==========")
message("Outputs:")
message("  Data:     ", DATA_OUT)
message("  Model:    ", DATA_OUT_MODEL)
message("  Forecast: ", DATA_OUT_FORECAST)
message("Figures/tables: results/figures/model, results/figures/forecast, results/tables/forecast")

