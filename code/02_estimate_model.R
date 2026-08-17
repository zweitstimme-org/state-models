#' ---
#' title: "Development Model"
#' date: " 2024"
#' ---


###########################
# Prepare environment
###########################

if (basename(getwd()) == "code") setwd("..")
ROOT <- getwd()
# Source packages in child env so rm(list=ls()) there does not wipe ROOT (when this script is sourced with local = new.env())
# Packages are still attached to the search path by require()
source(file.path(ROOT, "code", "auxilary", "packages.R"), local = new.env())
source(file.path(ROOT, "code", "auxilary", "functions.R"))
DATA_OUT <- file.path(ROOT, "data", "output")
DATA_OUT_MODEL <- file.path(ROOT, "data", "output", "model")
dir.create(DATA_OUT_MODEL, showWarnings = FALSE, recursive = TRUE)

# Prepare Data =====

# Load Data
load(file.path(DATA_OUT, "05_full_data.RData"))
if (!exists("lead_days", inherits = FALSE)) {
  lead_days <- as.integer(sort(unique(gsub("^polls_([0-9]+)$", "\\1", names(full_data)[grepl("^polls_[0-9]+$", names(full_data))]))))
  if (length(lead_days) == 0L) lead_days <- c(2L, 14L, 60L)
}

# Elections for evaluation: require non-missing polls for all lead days and actual results (exclude forecast rows with vote_share NA)
elec_eval_cols <- paste0("pollsNA_", lead_days)
elec_eval <- full_data %>%
  filter(!is.na(voteshare), year > 2010) %>%
  filter(if_all(all_of(elec_eval_cols), ~ . == 0)) %>%
  select(elec_ind) %>%
  distinct() %>%
  pull()

# Override imput_poll to use dynamic lead days (full_data has polls_10, polls_24, etc.)
imput_poll <- function(dat) {
  ld_cols <- as.integer(sort(unique(gsub("^polls_([0-9]+)$", "\\1", names(dat)[grepl("^polls_[0-9]+$", names(dat))]))))
  for (ld in ld_cols) {
    pcol <- paste0("polls_", ld)
    nacol <- paste0("pollsNA_", ld)
    if (!pcol %in% names(dat) || !nacol %in% names(dat)) next
    m <- lm(as.formula(paste(pcol, "~ voteshare_l1")), data = dat[dat[[nacol]] == 0, ])
    dat[[pcol]][dat[[nacol]] == 1] <- predict(m, newdata = dat[dat[[nacol]] == 1, c("voteshare_l1"), drop = FALSE]) +
      rnorm(sum(dat[[nacol]] == 1), mean = 0, sd = sigma(m))
  }
  return(dat)
}

# Training data: exclude "oth" and forecast rows (vote_share NA); forecast elections must not be used in training
# Include oth (Sonstige) in training so the model learns to predict it
data_filter <- filter(full_data, !is.na(voteshare))
names(data_filter)

## Functions ====

# Function to mask results and select
mask_and_select <- function(sel = "nw_2022-05-15", dv="voteshare", dat = full_data){
  
  # Step 1: Extract the election date of the selected election
  date <- dat %>% filter(elec_ind == sel) %>% pull(electiondate) %>% unique()
  
  # Step 2: Filter the data to include only elections that occurred before the selected election date
  # and include the selected election itself
  dat <- dat %>% filter(electiondate < date | elec_ind == sel)
  
  # Step 3: Extract the vote share data and relevant columns (party, state, election index, and year)
  dat_voterest <- dat %>% 
    filter(elec_ind == sel) %>% 
    dplyr::select(!!dv,voteshare, party, state, elec_ind, year)
  
  # Step 4: Assign a unique identifier (pid) to each row in the extracted data
  dat_voterest$pid <- 1:nrow(dat_voterest)
  
  # Step 5: Mask the vote share results of the selected election by replacing them with NA
  dat <- dat %>% mutate(
    !!dv := case_when(elec_ind == sel ~ NA_real_, TRUE ~ eval(parse(text = dv)))
  )
  
  # Step 6: Return a list containing the masked data and the extracted vote share data
  return(list("dat_masked" = dat, "dat_results" = dat_voterest))
  
}

# Function to run one iteration of the loop for processing election data with Bayesian linear regression using stan_glm
# Options that affect fit and reported intervals: prior_scale, prior_aux_rate (stan_glm), alpha (quantiles for lwr/upr).
process_election_bayesstan <- function(elec, dat=data_structural, dv="voteshare", impute_polls=T,
                                      filter_basedon=list("days"="pollsNA_days==0",
                                                          "weeks"="pollsNA_weeks==0",
                                                          "months"="pollsNA_months==0"),
                                      predictor_sets = list(
                                        "months" = c("polls_months", "voteshare_l1", "pm", "gov"),
                                        "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov"),
                                        "days" = c("polls_days", "voteshare_l1", "pm", "gov")
                                      ), alpha = 1/6,  # nominal interval: 1/6 => 83%, 1/2 => 50%
                                      prior_aux_rate = 2,
                                      prior_scale = 5,  # normal(0, prior_scale) on coefs
                                      dv_inv  = NULL) {

  dat_list <- mask_and_select(sel = elec, dat = dat, dv= dv)
  dat_masked <- dat_list$dat_masked

  if(impute_polls){
    dat_masked <- imput_poll(dat_masked)
  }

  all_res_dat <- list()
  print(elec)

  for (name in names(predictor_sets)) {

    bayes_lm_form <- as.formula(paste(dv, "~", paste(predictor_sets[[name]], collapse = " + ")))

    if(!is.null(filter_basedon[[name]])){
      dat_masked_filter <-  dat_masked %>% filter(eval(parse(text = filter_basedon[[name]])))
    } else{
      dat_masked_filter <-  dat_masked
    }

    mdl <- tryCatch(
      stan_glm(bayes_lm_form, data = dat_masked_filter,
               family = gaussian(),
               prior = normal(0, prior_scale),           # uses prior_scale
               prior_intercept = normal(0, prior_scale),
               prior_aux = exponential(rate = prior_aux_rate),  # uses prior_aux_rate
               # 4 chains × 1000 post-warmup = 4000 posterior simulations
               chains = 4, iter = 2000, cores = 4,
               seed = 238523862),
      error = function(e) {
        warning("Skipping ", name, " for ", elec, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(mdl)) {
      res_dat <- dat_list$dat_results %>% mutate(fit = NA_real_, lwr = NA_real_, upr = NA_real_, model_id = paste0("model_", name))
      all_res_dat[[name]] <- res_dat
      next
    }

    pred_dat <- dat_masked_filter %>% filter(elec_ind == elec)
    pred_x <- pred_dat %>% select(predictor_sets[[name]])

    pred_sim <- posterior_predict(mdl,pred_x)

    if(!is.null(dv_inv)){
      pred_sim <- dv_inv(pred_sim)
    }

    # Point estimate = median; interval uses alpha (alpha/2 and 1-alpha/2 quantiles)
    pred_fit <- apply(pred_sim, 2, quantile, probs = 0.5)
    pred_lwr <- apply(pred_sim, 2, quantile, probs = alpha/2)   # uses alpha
    pred_upr <- apply(pred_sim, 2, quantile, probs = 1-(alpha/2))
    # Normalize so predictions sum to 1 per election (evaluate final prediction after normalization)
    scale <- 1 / sum(pred_fit)
    pred_fit <- pred_fit * scale
    pred_lwr <- pred_lwr * scale
    pred_upr <- pred_upr * scale

    pred_dat <- cbind(pred_dat, fit = pred_fit, lwr = pred_lwr, upr = pred_upr)
    res_dat <- pred_dat %>% select(party, fit, lwr, upr) %>%
      left_join(dat_list$dat_results, ., by = join_by("party"))

    res_dat$model_id <- paste0("model_", name)
    all_res_dat[[name]] <- res_dat
  }

  combined_res_dat <- do.call(rbind, all_res_dat)
  return(combined_res_dat)
}


# Function to fit Bayesian linear models for each predictor set (no leave-one-out)
# Options that affect fit: prior_scale, prior_aux_rate (passed to stan_glm).
est_election_bayesstan <- function(dat=data_structural, dv="voteshare", impute_polls=T,
                                       filter_basedon=list("days"="pollsNA_days==0",
                                                           "weeks"="pollsNA_weeks==0",
                                                           "months"="pollsNA_months==0"),
                                       predictor_sets = list(
                                         "months" = c("polls_months", "voteshare_l1", "pm", "gov"),
                                         "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov"),
                                         "days" = c("polls_days", "voteshare_l1", "pm", "gov")
                                       ),
                                       prior_aux_rate = 2,
                                       prior_scale = 5) {

  if(impute_polls){
    dat <- imput_poll(dat)
  }

  res_mdl <- list()

  for (name in names(predictor_sets)) {

    bayes_lm_form <- as.formula(paste(dv, "~", paste(predictor_sets[[name]], collapse = " + ")))

    if(!is.null(filter_basedon[[name]])){
      dat_filter <-  dat %>% filter(eval(parse(text = filter_basedon[[name]])))
    } else{
      dat_filter <-  dat
    }

    mdl <-  stan_glm(bayes_lm_form, data = dat_filter,
                     family = gaussian(),
                     prior = normal(0, prior_scale),           # uses prior_scale
                     prior_intercept = normal(0, prior_scale),
                     prior_aux = exponential(rate = prior_aux_rate),  # uses prior_aux_rate
                     # 4 chains × 1000 post-warmup = 4000 posterior simulations
                     chains = 4, iter = 2000, cores = 4,
                     seed = 238523862)

    res_mdl[[name]] <- mdl
  }

  return(res_mdl)
}


# Estimation model ======
# Lead = days until election. Models are trained per lead day (from full_data / lead_days).
# MODEL_LEADS env overrides; else use lead_days from 05_full_data.RData.
# MODEL_POLLS_ONLY=1 => only "<lead>_polls" (predictor = poll only); else "<lead>_all" (polls + fed trends + l1 + pm/gov).
leads_env <- Sys.getenv("MODEL_LEADS", "")
if (nzchar(leads_env)) {
  leads <- as.character(trimws(strsplit(leads_env, ",")[[1]]))
} else {
  leads <- as.character(lead_days)
}
polls_only <- (Sys.getenv("MODEL_POLLS_ONLY", "0") == "1")
variant_suffix <- if (polls_only) "polls" else "all"
MODEL_VARIANTS <- paste0(leads, "_", variant_suffix)
if (length(MODEL_VARIANTS) == 0L) MODEL_VARIANTS <- paste0(lead_days[1L], "_", variant_suffix)
# Whether to also estimate linear (raw vote share) spec; FALSE = log-ratio only
ESTIMATE_LINEAR <- if (exists("ESTIMATE_LINEAR", envir = .GlobalEnv, inherits = FALSE)) {
  get("ESTIMATE_LINEAR", envir = .GlobalEnv)
} else FALSE
message("Lead days: ", paste(leads, collapse = ", "), " => variants: ", paste(MODEL_VARIANTS, collapse = ", "), " (polls_only: ", polls_only, ")")
message("Log-ratio: yes; Linear: ", if (ESTIMATE_LINEAR) "yes" else "no")

# Full configs: _all = polls + federal trends + l1 + pm/gov; _polls = poll only (no fundamentals)
filter_basedon_all <- setNames(as.list(paste0("pollsNA_", leads, "==0")), paste0(leads, "_all"))
filter_basedon_polls <- setNames(as.list(paste0("pollsNA_", leads, "==0")), paste0(leads, "_polls"))
predictor_sets_lr_all <- setNames(
  lapply(leads, function(L) c(paste0("pollslr_", L), paste0("fed_trends_lr_", L), "votesharelr_l1", "pm", "gov")),
  paste0(leads, "_all")
)
predictor_sets_lr_polls <- setNames(
  lapply(leads, function(L) paste0("pollslr_", L)),
  paste0(leads, "_polls")
)
predictor_sets_lm_all <- setNames(
  lapply(leads, function(L) c(paste0("polls_", L), paste0("fed_trend_", L), "voteshare_l1", "pm", "gov")),
  paste0(leads, "_all")
)
predictor_sets_lm_polls <- setNames(
  lapply(leads, function(L) paste0("polls_", L)),
  paste0(leads, "_polls")
)
filter_basedon <- if (polls_only) filter_basedon_polls[MODEL_VARIANTS] else filter_basedon_all[MODEL_VARIANTS]
predictor_sets_lr <- if (polls_only) predictor_sets_lr_polls[MODEL_VARIANTS] else predictor_sets_lr_all[MODEL_VARIANTS]
predictor_sets_lm <- if (polls_only) predictor_sets_lm_polls[MODEL_VARIANTS] else predictor_sets_lm_all[MODEL_VARIANTS]
# Tighter coefficient prior for polls-only (prior_scale) narrows intervals; prior_aux same for both.
prior_aux_rate <- 2
prior_scale <- if (polls_only) 1 else 5   # normal(0, prior_scale) on coefs; 1 much tighter than 5
# Same nominal interval for all: 5/6 (83%). High empirical coverage => intervals are conservative/wide.
alpha_eval <- 1/6
message("prior_aux rate: ", prior_aux_rate, ", prior_scale: ", prior_scale, ", alpha_eval: ", alpha_eval, " (polls_only: ", polls_only, ")")

# Log-ratio (always estimated)
res_lr <- est_election_bayesstan(dat = data_filter, dv = "votesharelr",
                                 filter_basedon = filter_basedon,
                                 predictor_sets = predictor_sets_lr,
                                 prior_aux_rate = prior_aux_rate,
                                 prior_scale = prior_scale)

# Linear (optional)
if (ESTIMATE_LINEAR) {
  res_lm <- est_election_bayesstan(dat = data_filter, dv = "voteshare",
                                   filter_basedon = filter_basedon,
                                   predictor_sets = predictor_sets_lm,
                                   prior_aux_rate = prior_aux_rate,
                                   prior_scale = prior_scale)
}

# Save (forecasts use lr only)
model_bayes <- list(lr = res_lr)
if (ESTIMATE_LINEAR) model_bayes[["lm"]] <- res_lm
saveRDS(model_bayes, file = file.path(DATA_OUT_MODEL, "model_bayes.RDS"))

# --- Leave-one-out evaluation ---

# Log-ratio evaluation (always)
all_res_lr <- do.call(rbind, lapply(elec_eval, function(elec) {
  process_election_bayesstan(elec, dat = data_filter, dv = "votesharelr",
                             dv_inv = function(x) exp(x) / (1 + exp(x)),
                             filter_basedon = filter_basedon,
                             predictor_sets = predictor_sets_lr,
                             prior_aux_rate = prior_aux_rate,
                             prior_scale = prior_scale,
                             alpha = alpha_eval)
}))
# Coverage: fraction of holdout outcomes inside the nominal interval (alpha_eval: 1/6 = 83%, 1/2 = 50% for polls-only).
# Empirical coverage often exceeds 83% because intervals are posterior predictive (parameter
# + residual uncertainty); with default prior_aux and parameter uncertainty they tend to be wide.
res_eval_agg_lr <- all_res_lr %>%
  group_by(model_id) %>%
  dplyr::summarise(mae = mean(abs(voteshare*100 - fit*100), na.rm = T),
                   rmse = sqrt(mean((voteshare*100 - fit*100)^2, na.rm = T)),
                   bias = mean(voteshare*100 - fit*100, na.rm = T),
                   coverage = mean(lwr < voteshare & voteshare < upr, na.rm = T),
                   mean_interval_pp = mean((upr - lwr)*100, na.rm = T)) %>%
  arrange(rmse) %>%
  separate(model_id, c(NA,"lead","predictors")) %>%
  mutate(predictors = ifelse(is.na(predictors),"polls", predictors),
         model_type = "logratio")

# Linear evaluation (only if linear was estimated)
if (ESTIMATE_LINEAR) {
  all_res_lm <- do.call(rbind, lapply(elec_eval, function(elec) {
    process_election_bayesstan(elec, dat = data_filter, dv = "voteshare",
                               filter_basedon = filter_basedon,
                               predictor_sets = predictor_sets_lm,
                               prior_aux_rate = prior_aux_rate,
                               prior_scale = prior_scale,
                               alpha = alpha_eval)
  }))
  res_eval_agg <- all_res_lm %>%
    group_by(model_id) %>%
    dplyr::summarise(mae = mean(abs(voteshare*100 - fit*100), na.rm = T),
                     rmse = sqrt(mean((voteshare*100 - fit*100)^2, na.rm = T)),
                     bias = mean(voteshare*100 - fit*100, na.rm = T),
                     coverage = mean(lwr < voteshare & voteshare < upr, na.rm = T),
                     mean_interval_pp = mean((upr - lwr)*100, na.rm = T)) %>%
    arrange(rmse) %>%
    separate(model_id, c(NA,"lead","predictors")) %>%
    mutate(predictors = ifelse(is.na(predictors),"polls", predictors),
           model_type = "linear")
  res_eval_agg <- bind_rows(res_eval_agg, res_eval_agg_lr) %>% arrange(rmse)
} else {
  res_eval_agg <- res_eval_agg_lr
}


# Print Result
print(res_eval_agg)


# Save the results
saveRDS(res_eval_agg, file = file.path(DATA_OUT_MODEL, "model_bayes_eval.RDS"))

# Write eval table for blog (MAE, RMSE, coverage per lead)
RESULTS_TABLES <- file.path(ROOT, "results", "tables")
dir.create(RESULTS_TABLES, showWarnings = FALSE, recursive = TRUE)
eval_txt <- res_eval_agg %>%
  mutate(mae = round(mae, 2), rmse = round(rmse, 2), coverage = round(coverage, 3),
         mean_interval_pp = round(mean_interval_pp, 1)) %>%
  select(lead, predictors, model_type, mae, rmse, coverage, mean_interval_pp)
eval_lines <- c(
  "Model evaluation (leave-one-out) · for blog",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "lead | predictors | model_type | mae | rmse | coverage | mean_interval_pp",
  paste(rep("-", 70), collapse = "")
)
for (i in seq_len(nrow(eval_txt))) {
  r <- eval_txt[i, ]
  eval_lines <- c(eval_lines,
    paste(r$lead, r$predictors, r$model_type, r$mae, r$rmse, r$coverage, r$mean_interval_pp, sep = " | "))
}
writeLines(eval_lines, file.path(RESULTS_TABLES, "model_eval_blog.txt"))
message("  Written results/tables/model_eval_blog.txt")

# Error analysis: build df_error for 06_model_error_analysis.R (needs lead, predictors, dv, voteshare, fit, ae)
error_lr <- all_res_lr %>%
  mutate(voteshare = exp(votesharelr) / (1 + exp(votesharelr)),
         fit_lr = exp(fit) / (1 + exp(fit)),
         ae_lr = abs(voteshare - fit_lr)) %>%
  select(party, elec_ind, model_id, fit_lr, ae_lr, voteshare)

if (ESTIMATE_LINEAR) {
  error_lm <- all_res_lm %>%
    mutate(ae_lm = abs(voteshare - fit)) %>%
    select(party, elec_ind, model_id, fit_lm = fit, ae_lm, voteshare)
  df_error <- left_join(error_lr, error_lm, by = c("party", "elec_ind", "model_id")) %>%
    pivot_longer(cols = c("fit_lr", "fit_lm", "ae_lr", "ae_lm")) %>%
    separate(name, c("var", "dv")) %>%
    pivot_wider(names_from = "var", values_from = "value") %>%
    separate(model_id, c(NA, "lead", "predictors")) %>%
    mutate(predictors = ifelse(is.na(predictors), "polls", predictors))
} else {
  df_error <- error_lr %>%
    separate(model_id, c(NA, "lead", "predictors")) %>%
    mutate(predictors = ifelse(is.na(predictors), "polls", predictors),
           dv = "lr", fit = fit_lr, ae = ae_lr) %>%
    select(party, elec_ind, lead, predictors, dv, voteshare, fit, ae)
}

saveRDS(df_error, file = file.path(DATA_OUT_MODEL, "model_bayes_errors.RDS"))




