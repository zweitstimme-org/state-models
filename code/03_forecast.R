# 03_forecast.R — Build forecast data and run state election forecasts.
# Part 1: Prepare forecast data (elections_to_forecast, leads, full_data) → forecast_data.RData
# Part 2: Run state forecasts (model + forecast_data) → fcst_state.Rdata

###########################
# Prepare environment
###########################

if (basename(getwd()) == "code") setwd("..")
# Use getwd() for first source so we don't depend on ROOT (packages.R may wipe env when keep_env is unset)
source(file.path(getwd(), "code", "auxilary", "packages.R"))
ROOT <- getOption("script_root")
if (is.null(ROOT)) ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "functions.R"))
DATA_OUT <- file.path(ROOT, "data", "output")
DATA_IN  <- file.path(ROOT, "data", "input")
DATA_OUT_MODEL <- file.path(ROOT, "data", "output", "model")
DATA_OUT_FORECAST <- file.path(ROOT, "data", "output", "forecast")
dir.create(DATA_OUT_FORECAST, showWarnings = FALSE, recursive = TRUE)

###########################
# Part 1: Forecast data
###########################

load(file.path(DATA_OUT, "05_full_data.RData"))
if (!exists("lead_days", inherits = FALSE)) {
  lead_days <- as.integer(sort(unique(gsub("^polls_([0-9]+)$", "\\1", names(full_data)[grepl("^polls_[0-9]+$", names(full_data))]))))
  if (length(lead_days) == 0L) lead_days <- c(2L, 14L, 60L)
}
federal_results <- read.csv(file.path(DATA_IN, "03_federal-election-results.csv"))

# Elections to forecast (from API via run_pipeline.R/run_everything.R; horizon configurable there)
elections_to_forecast_path <- file.path(DATA_OUT, "elections_to_forecast.RData")
if (!file.exists(elections_to_forecast_path)) {
  stop("Run the pipeline from run_everything.R or run_pipeline.R so elections_to_forecast.RData is created (API v1/elections).")
}
load(elections_to_forecast_path)
if (length(elections_to_forecast) == 0L) {
  stop("No elections to forecast. Nothing to do.")
}

# Parse elec_ind ("bw_2026-03-08") -> state, electiondate
elections_df <- data.frame(elec_ind = elections_to_forecast, stringsAsFactors = FALSE) %>%
  mutate(
    state = sub("_.*", "", elec_ind),
    electiondate = as.Date(sub(".*_", "", elec_ind))
  )
get_l1 <- function(st, ed) {
  x <- full_data$electiondate[full_data$state == st & full_data$electiondate < ed]
  if (length(x)) max(x) else NA
}
elections_df <- elections_df %>%
  rowwise() %>%
  mutate(electiondate_l1 = get_l1(state, electiondate)) %>%
  ungroup()

# State polls from build step (data/output/01_state-polls.csv); column is "land"
state_polls_path <- file.path(DATA_OUT, "01_state-polls.csv")
if (!file.exists(state_polls_path)) stop("Missing ", state_polls_path, " — run 01_build_data.R first.")
current_polls <- read.csv(state_polls_path, stringsAsFactors = FALSE)
current_polls$date <- as.Date(current_polls$date)
current_polls <- current_polls %>% filter(land %in% unique(elections_df$state))

# Forecast leads: one row per (state, party, electiondate); voteshare_l1 from full_data
forecast_leads <- elections_df %>%
  tidyr::crossing(party = c("cdu", "fdp", "gru", "lin", "bsw", "spd", "oth", "afd")) %>%
  left_join(
    full_data %>% select(state, party, electiondate, voteshare) %>%
      rename(electiondate_l1 = electiondate, voteshare_l1 = voteshare),
    by = c("state", "party", "electiondate_l1")
  ) %>%
  mutate(year = lubridate::year(electiondate), voteshare = NA)

# Lagged state vote share NA when party didn't exist at l1 (e.g. AfD, BSW); set to 0 like in 01_build_data
forecast_leads <- forecast_leads %>%
  mutate(voteshare_l1 = case_when(is.na(voteshare_l1) ~ 0, TRUE ~ voteshare_l1))

dir.create(file.path(ROOT, "data", "output"), showWarnings = FALSE, recursive = TRUE)

# Make dates
forecast_leads$electiondate <- ymd(forecast_leads$electiondate)
forecast_leads$electiondate_l1 <- ymd(forecast_leads$electiondate_l1)
current_polls$date <- ymd(current_polls$date)

# Lead / Stand = last state poll included (not calendar today). Daily re-runs without new polls keep the same lead.
stand_by_land <- last_poll_dates_by_land(current_polls)
if (!length(stand_by_land)) {
  stop("No state poll dates found in ", state_polls_path, " for lands: ", paste(unique(elections_df$state), collapse = ", "))
}
forecast_leads <- forecast_leads %>%
  mutate(
    stand_date = as.Date(unname(stand_by_land[state])),
    lead_days = as.numeric(electiondate - stand_date),
    date_as_of = stand_date
  )
if (any(is.na(forecast_leads$stand_date))) {
  bad <- unique(forecast_leads$state[is.na(forecast_leads$stand_date)])
  stop("Missing last-poll Stand for land(s): ", paste(bad, collapse = ", "))
}
message(
  "Forecast Stand (last poll) by land: ",
  paste(names(stand_by_land), format(stand_by_land), sep = "=", collapse = ", ")
)
message(
  "Lead days by election: ",
  paste(
    unique(paste0(forecast_leads$elec_ind, "=", as.integer(forecast_leads$lead_days))),
    collapse = ", "
  )
)
# For figures / single-election convenience: max Stand among forecast states
stand_date <- max(forecast_leads$stand_date, na.rm = TRUE)
forecast_leads <- filter(forecast_leads, date_as_of >= min(current_polls$date, na.rm = TRUE))

# Polls at each state's Stand (one value; fill all lead columns so the chosen model has predictors)
for (ld in lead_days) forecast_leads[[paste0("polls_", ld)]] <- NA_real_
forecast_leads$calculated <- FALSE
for (i in 1:nrow(forecast_leads)) {
  if (forecast_leads$calculated[i]) next
  if (is.na(forecast_leads$electiondate_l1[i])) next  # need previous election for DLM
  sd_i <- forecast_leads$stand_date[i]
  p <- get_latent_support_land(current_polls, forecast_leads$party[i], forecast_leads$state[i], sd_i, forecast_leads$electiondate_l1[i], "party", "date", "poll_share", "land")
  for (ld in lead_days) forecast_leads[[paste0("polls_", ld)]][i] <- p
  forecast_leads$calculated[i] <- TRUE
}
forecast_leads <- forecast_leads %>% select(-calculated)

###########################
# Calculate federal leads
###########################
# Federal polls from pipeline (01_build_data.R), same API as state polls — no coalitions package
federal_polls_path <- file.path(DATA_OUT, "03_federal-polls.csv")
if (!file.exists(federal_polls_path)) {
  stop("Missing ", federal_polls_path, " — run 01_build_data.R (or full pipeline) first.")
}
wahlrecht <- read.csv(federal_polls_path, stringsAsFactors = FALSE) %>%
  select(date, party, poll_share) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(date)) %>%
  unique()

# Federal polls at each state's Stand (same asof as state predictors; no post-Stand federal drift)
for (ld in lead_days) forecast_leads[[paste0("fed_polls_", ld)]] <- NA_real_
forecast_leads$calculated <- FALSE
for (i in 1:nrow(forecast_leads)) {
  if (forecast_leads$calculated[i]) next
  if (is.na(forecast_leads$electiondate_l1[i])) next  # need previous election for DLM
  sd_i <- forecast_leads$stand_date[i]
  f <- get_latent_support(wahlrecht, forecast_leads$party[i], sd_i, forecast_leads$electiondate_l1[i], "party", "date", "poll_share")
  for (ld in lead_days) forecast_leads[[paste0("fed_polls_", ld)]][i] <- f
  forecast_leads$calculated[i] <- TRUE
}
forecast_leads <- forecast_leads %>% select(-calculated)

############################
# Add lagged federal vote share
############################

forecast_leads$fed_electiondate_l1 <- NA

for (i in 1:nrow(forecast_leads)) {
  print(i)
  (subset_results <- federal_results %>% filter(electiondate < forecast_leads$electiondate[i]))
  if(nrow(subset_results) > 0) (forecast_leads$fed_electiondate_l1[i] <- (subset_results %>%
                                                                           filter(electiondate == max(subset_results$electiondate)))$electiondate %>% unique)
}

forecast_leads$fed_electiondate_l1 <- forecast_leads$fed_electiondate_l1 %>% as_date

federal_results$electiondate <- as_date(federal_results$electiondate)

# Merge the lagged vote share via the days/weeks/months electiondate var
forecast_leads <- federal_results %>% select(electiondate, party, vote_share) %>%
  dplyr::rename(fed_vote_share = vote_share,
                fed_electiondate_l1 = electiondate) %>%
  merge(forecast_leads, by = c("fed_electiondate_l1", "party"), all.y = T)

# BSW: no l1 federal result; where we have current BSW federal polling, use Lin's l1 fed share for fed_trend
lin_fed <- forecast_leads %>% filter(party == "lin") %>% select(state, electiondate, fed_vote_share) %>% rename(fed_vote_share_lin = fed_vote_share)
forecast_leads <- forecast_leads %>%
  left_join(lin_fed, by = c("state", "electiondate")) %>%
  mutate(
    has_bsw_fed_polls = (party == "bsw" & !is.na(!!sym(paste0("fed_polls_", lead_days[1L]))) & (!!sym(paste0("fed_polls_", lead_days[1L]))) > 0),
    fed_vote_share = case_when(party == "bsw" & is.na(fed_vote_share) & has_bsw_fed_polls ~ fed_vote_share_lin, TRUE ~ fed_vote_share)
  ) %>%
  select(-fed_vote_share_lin, -has_bsw_fed_polls)
# Lagged federal vote share NA when party didn't exist at l1 (e.g. AfD, or BSW without fed polls); set to 0
forecast_leads <- forecast_leads %>%
  mutate(fed_vote_share = case_when(is.na(fed_vote_share) ~ 0, TRUE ~ fed_vote_share))

# Fill lagged federal vote share for other
forecast_leads <- forecast_leads %>%
  group_by(state) %>%
  mutate(total_fed_vote_share = sum(fed_vote_share, na.rm = T)) %>%
  mutate(fed_vote_share = case_when(party == "oth" ~ 100 - total_fed_vote_share,
                                    TRUE ~ fed_vote_share)) %>%
  ungroup()

# Function for log-ratio with correction if value 0 (handles NA for forecast rows)
log_ratio <- function(x, corre_fct = 0.01){
  x[!is.na(x) & x < 0] <- 0
  x[!is.na(x) & x == 0] <- x[!is.na(x) & x == 0] + corre_fct
  return(log(x / (1 - x)))
}

# Add elections to forecast for ltw (use states from elections_to_forecast)
forecast_states <- unique(elections_df$state)
forecast_data <- full_data %>%
  filter(state %in% forecast_states) %>%
  group_by(state) %>%
  filter(electiondate == max(electiondate)) %>%
  ungroup %>%
  dplyr::select(state, year, party, voteshare) %>%
  mutate(year = lubridate::year(Sys.Date())) %>%
  dplyr::rename(voteshare_l1 = voteshare) %>%
  left_join(forecast_leads, .) %>%
  mutate("election_type" = "forecast") %>%
  mutate(
    # gov/pm from current cabinets (data/input/02_state-cabinets.csv) when available;
    # fall back to known east-state coalitions used for SN/BB/TH in older runs.
    gov = case_when(
      state == "st" & party %in% c("cdu", "spd", "fdp") ~ 1,
      state == "mv" & party %in% c("spd", "lin") ~ 1,
      state == "be" & party %in% c("cdu", "spd") ~ 1,
      state == "bw" & party %in% c("gru", "cdu") ~ 1,
      state == "rp" & party %in% c("spd", "gru", "fdp") ~ 1,
      state == "sn" & party %in% c("gru", "spd", "cdu") ~ 1,
      state == "bb" & party %in% c("gru", "spd", "cdu") ~ 1,
      state == "th" & party %in% c("lin", "spd", "gru") ~ 1,
      TRUE ~ 0
    ),
    pm = case_when(
      state == "st" & party == "cdu" ~ 1,
      state == "mv" & party == "spd" ~ 1,
      state == "be" & party == "cdu" ~ 1,
      state == "bw" & party == "gru" ~ 1,
      state == "rp" & party == "spd" ~ 1,
      state == "sn" & party == "cdu" ~ 1,
      state == "bb" & party == "spd" ~ 1,
      state == "th" & party == "lin" ~ 1,
      TRUE ~ 0
    ),
    elec_ind = paste(state, electiondate, sep = "_")
  ) %>%
  mutate(elec_ind = paste(state, electiondate, sep = "_"))
# pollsNA_L = 0 and fed_trend / fed_trends_lr per lead day
for (ld in lead_days) forecast_data[[paste0("pollsNA_", ld)]] <- 0L
log_fed_lr <- function(fp, fv) {
  fp <- ifelse(fp == 0, 0.000001, fp)
  fv <- ifelse(fv == 0, 0.000001, fv)
  log(fp / (100 - fp)) - log(fv / (100 - fv))
}
for (ld in lead_days) {
  forecast_data[[paste0("fed_trend_", ld)]] <- forecast_data[[paste0("fed_polls_", ld)]] - forecast_data$fed_vote_share
  forecast_data[[paste0("fed_trends_lr_", ld)]] <- log_fed_lr(forecast_data[[paste0("fed_polls_", ld)]], forecast_data$fed_vote_share)
}
forecast_data <- forecast_data %>%
  mutate("pmXgov" = pm * gov, "voteshare" = voteshare/100, "fed_vote_share" = fed_vote_share/100)
for (ld in lead_days) {
  forecast_data[[paste0("polls_", ld)]] <- forecast_data[[paste0("polls_", ld)]] / 100
  forecast_data[[paste0("fed_polls_", ld)]] <- forecast_data[[paste0("fed_polls_", ld)]] / 100
  forecast_data[[paste0("fed_trend_", ld)]] <- forecast_data[[paste0("fed_trend_", ld)]] / 100
}
forecast_data <- forecast_data %>%
  mutate(
    votesharelr = log_ratio(voteshare),
    votesharelr_l1 = log_ratio(voteshare_l1)
  )
for (ld in lead_days) forecast_data[[paste0("pollslr_", ld)]] <- log_ratio(forecast_data[[paste0("polls_", ld)]])

# Save forecast data (for downstream scripts and run_everything.R)
save(forecast_data, file = file.path(ROOT, "data", "output", "forecast_data.RData"))

###########################
# Part 2: State forecasts (only for model variants that were estimated)
###########################

# elections_to_forecast already in memory from Part 1
# forecast_data, full_data already in memory

# Paths for Part 2 (use paths relative to working directory so they work when sourced from run_everything)
root_p2 <- if (basename(getwd()) == "code") dirname(getwd()) else getwd()
model_path <- file.path(root_p2, "data", "output", "model", "model_bayes.RDS")
forecast_dir <- file.path(root_p2, "data", "output", "forecast")

# Load model estimates
res <- readRDS(model_path)
lr_variants <- names(res[["lr"]])
if (length(lr_variants) == 0L) stop("No lr models in model_bayes.RDS; run step 2 with MODEL_VARIANTS set.")

# One forecast per election: use model whose lead is closest to this election's lead_days (e.g. 8_all or 8_polls).
LEAD_GRID <- as.numeric(sub("_.*", "", lr_variants))
closest_lead_variant <- function(lead_days_val) {
  v <- LEAD_GRID[which.min(abs(LEAD_GRID - lead_days_val))]
  idx <- which(sub("_.*", "", lr_variants) == as.character(v))
  if (length(idx) == 0L) return(paste0(v, "_all"))
  lr_variants[idx[1L]]
}

fcst_ci_list <- list()
fcst_draws_list <- list()
for (elc in elections_to_forecast) {
  st <- sub("_.*", "", elc)
  ed <- as.Date(sub(".*_", "", elc))
  sd_el <- stand_by_land[[st]]
  if (is.null(sd_el) || is.na(sd_el)) {
    warning("No Stand for ", st, "; skipping forecast ", elc)
    next
  }
  lead_el <- as.numeric(ed - as.Date(sd_el))
  variant <- closest_lead_variant(lead_el)
  if (!variant %in% lr_variants) next
  fcst_ci_list[[elc]] <- generate_forecast_data(election_id = elc, model = res[["lr"]][[variant]]) %>%
    mutate(lead_days_used = as.integer(sub("_.*", "", variant)), stand_date = as.Date(sd_el))
  fcst_draws_list[[elc]] <- get_posterior_draws(election_id = elc, model = res[["lr"]][[variant]]) %>%
    mutate(lead_days_used = as.integer(sub("_.*", "", variant)), stand_date = as.Date(sd_el))
}
fcst_ci <- bind_rows(fcst_ci_list)
fcst_draws <- bind_rows(fcst_draws_list)

if (nrow(fcst_ci) > 0L) {
  validate_forecast_sanity(fcst_ci, major_parties = c("cdu", "spd", "gru", "afd"))
}

# Save last-election results (main parties + oth) and state polls used in forecast so
# figures use the same definition (oth = 100% - sum(main)) without re-deriving from raw CSVs.
last_election_results <- elections_df %>%
  distinct(state, electiondate_l1) %>%
  left_join(
    full_data %>% select(state, electiondate, party, voteshare),
    by = c("state" = "state", "electiondate_l1" = "electiondate")
  ) %>%
  filter(!is.na(party)) %>%
  mutate(
    state_display = map_state_names(state),
    party_name = map_party_names(party),
    share = as.numeric(voteshare)
  ) %>%
  select(state = state_display, party_name, share)
state_polls_for_figure <- current_polls

# Persist model type so figure filenames match (polls vs all)
polls_only <- (Sys.getenv("MODEL_POLLS_ONLY", "0") == "1")
model_type_suffix <- if (polls_only) "polls" else "all"
save_objects <- c("fcst_ci", "fcst_draws", "stand_date", "last_election_results", "state_polls_for_figure", "model_type_suffix")
save(list = save_objects, file = file.path(forecast_dir, "fcst_state.Rdata"))
