#!/usr/bin/env Rscript
###########################
# Run full forecast pipeline from project root.
# For Docker / Render cron: build data -> estimate model -> forecast data -> forecast.
# Requires: data/input/01_state-election-results.csv, 02_state-cabinets.csv, 03_federal-election-results.csv
###########################

ROOT <- getwd()
if (basename(ROOT) %in% c("code", "data")) {
  setwd("..")
  ROOT <- getwd()
}

message("=== state-models pipeline (ROOT: ", ROOT, ") ===\n")

# Ensure output dirs exist
dir.create(file.path(ROOT, "data", "input"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(ROOT, "data", "output"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(ROOT, "9_full-data", "output"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(ROOT, "5_federal-elections"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(ROOT, "10_models", "output", "mdl"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(ROOT, "11_forecast-data", "output"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(ROOT, "12_forecast", "output"), showWarnings = FALSE, recursive = TRUE)

# Wire federal results for 11_forecast-data (expects 5_federal-elections/federal-election-results.csv)
fed_in  <- file.path(ROOT, "data", "input", "03_federal-election-results.csv")
fed_out <- file.path(ROOT, "5_federal-elections", "federal-election-results.csv")
if (file.exists(fed_in) && !file.exists(fed_out)) {
  file.copy(fed_in, fed_out, overwrite = FALSE)
  message("Copied federal results to 5_federal-elections/ for forecast-data step.\n")
}

# 0) Elections to forecast and lead days (needed before build data)
API_BASE <- Sys.getenv("POLLING_API_BASE", "https://api.fasttrack29.com")
source(file.path(ROOT, "code", "auxilary", "functions.R"))
MAX_MONTHS_AHEAD <- as.integer(Sys.getenv("FORECAST_MAX_MONTHS_AHEAD", "12"))
if (is.na(MAX_MONTHS_AHEAD) || MAX_MONTHS_AHEAD <= 0L) MAX_MONTHS_AHEAD <- 12L

# Optional manual override (comma-separated): "bw_2026-03-08,rp_2026-03-22"
manual_elections_env <- Sys.getenv("ELECTIONS_TO_FORECAST", "")
if (nzchar(manual_elections_env)) {
  elections_to_forecast <- unique(trimws(strsplit(manual_elections_env, ",")[[1]]))
} else {
  elections_to_forecast <- get_elections_to_forecast(api_base = API_BASE, max_months_ahead = MAX_MONTHS_AHEAD)
}

# Fallback: if API returns none, use next known ST + MV + BE elections (within 12 months as of Feb 2026)
if (!nzchar(manual_elections_env) && length(elections_to_forecast) == 0L) {
  fallback <- c("st_2026-09-06", "mv_2026-09-20", "be_2026-09-20")
  # Keep only elections within horizon window
  today <- as.Date(Sys.Date())
  window_end <- today + lubridate::period(MAX_MONTHS_AHEAD, units = "months")
  fallback_dates <- as.Date(sub(".*_", "", fallback))
  keep <- which(!is.na(fallback_dates) & fallback_dates >= today & fallback_dates <= window_end)
  elections_to_forecast <- fallback[keep]
  if (length(elections_to_forecast) > 0L) {
    message(
      "API returned no elections; falling back to: ",
      paste(elections_to_forecast, collapse = ", "),
      " (ST 06.09.2026, MV 20.09.2026, BE 20.09.2026)."
    )
  }
}
save(elections_to_forecast, file = file.path(ROOT, "data", "output", "elections_to_forecast.RData"))
if (length(elections_to_forecast) == 0L) {
  message("No elections to forecast (future state elections within ", MAX_MONTHS_AHEAD, " months). API: ", API_BASE, "/v1/elections")
  message("Continuing pipeline without forecast step; estimating model on default lead times (2, 14, 60).")
}
if (length(elections_to_forecast) > 0L) {
  message("Elections to forecast (future, <= ", MAX_MONTHS_AHEAD, " months): ", paste(elections_to_forecast, collapse = ", "), "\n")
  # Lead = election − last poll Stand (not calendar today), so daily re-runs without new polls stay put.
  polls_csv <- file.path(ROOT, "data", "output", "01_state-polls.csv")
  lead_days <- lead_days_from_last_polls(
    elections_to_forecast,
    api_base = API_BASE,
    polls_csv = if (file.exists(polls_csv)) polls_csv else NULL
  )
  stands <- attr(lead_days, "stand_by_land")
  if (length(stands)) {
    message(
      "Stand (last poll) by land: ",
      paste(names(stands), format(stands), sep = "=", collapse = ", ")
    )
  }
} else {
  lead_days <- c(2, 14, 60)
}
save(lead_days, file = file.path(ROOT, "data", "output", "lead_days.RData"))
# Always align MODEL_LEADS with Stand-anchored leads (override stale calendar-based env).
Sys.setenv(MODEL_LEADS = paste(lead_days, collapse = ","))
message("Lead days (from last-poll Stand): ", paste(lead_days, collapse = ", "), "\n")

# 1) Build data (state polls API, federal polls, leads, full_data; uses lead_days.RData)
message("--- 1. Build data ---")
source(file.path(ROOT, "code", "01_build_data.R"))

# 2) Copy full_data so 10_models, 11_forecast-data, 12_forecast find it under 9_full-data/output
full_from <- file.path(ROOT, "data", "output", "05_full_data.RData")
if (!file.exists(full_from)) stop("Build data did not produce ", full_from)
for (f in c("full-data.RData", "full_data.RData")) {
  file.copy(full_from, file.path(ROOT, "9_full-data", "output", f), overwrite = TRUE)
}
message("Wired full_data to 9_full-data/output/.\n")

# 3) Estimate model (lead days from elections; only _all variant per lead)
if (!nzchar(Sys.getenv("MODEL_ONLY_ALL"))) Sys.setenv(MODEL_ONLY_ALL = "1")
message("--- 2. Estimate model ---")
# Inherit the process environment (do NOT pass Sys.getenv() into system2's env=
# argument — values with spaces break argv on some hosts).
status_10 <- system2(
  "Rscript",
  file.path(ROOT, "code", "02_estimate_model.R"),
  stdout = "",
  stderr = ""
)
if (!is.null(status_10) && status_10 != 0L) {
  stop("Model estimation (02_estimate_model.R) failed with status ", status_10)
}

# 4) Forecast data + state forecasts (one script: builds forecast_data, then runs forecasts)
if (length(elections_to_forecast) > 0L) {
  message("--- 3. Forecast (data + state forecasts) ---")
  status_fcst <- system2(
    "Rscript",
    file.path(ROOT, "code", "03_forecast.R"),
    stdout = "",
    stderr = ""
  )
  if (!is.null(status_fcst) && status_fcst != 0L) {
    stop("Forecast (code/03_forecast.R) failed with status ", status_fcst)
  }
}

message("\n=== Pipeline finished successfully. ===")
message("Outputs: data/output/, data/output/forecast/, data/output/model/")
