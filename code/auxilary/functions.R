

# Function fo log-ration with correction if value ß
log_ratio <- function(x,corre_fct = 0.01){
  x[x < 0] <- 0
  x[x == 0] <- x[x == 0] + corre_fct 
  return(log(x / (1 - x)))
}        

# Function to mask results and select
mask_and_select <- function(sel = "nw_2022-05-15", dat = data_structural){
  
  # Step 1: Determine the election date for the selected election
  date <- dat %>% filter(elec_ind == sel) %>% pull(electiondate) %>% unique()
  
  # Step 2: Filter the data to include only elections that occurred before the selected election or the selected election itself
  dat <- dat %>% filter(electiondate < date | elec_ind == sel)
  
  # Step 3: Extract vote share data for the selected election, including relevant columns (party, state, election index, and year)
  dat_voterest  <- dat %>%
    filter(elec_ind == sel) %>% 
    dplyr::select(voteshare, party, state, elec_ind, year)
  
  # Step 4: Assign a unique ID (pid) to each row in the extracted data
  dat_voterest$pid <- 1:nrow(dat_voterest)
  
  # Step 5: Mask the vote share results for the selected election (replace with NA)
  dat <- dat %>% mutate(
    voteshare = case_when(elec_ind == sel ~ NA, TRUE ~ voteshare)
  )
  
  # Step 6: Return a list containing the masked data and the extracted vote share data
  return(list("dat_masked" = dat, "dat_results" = dat_voterest))
  
}

# Function to impute missing poll data (lead days inferred from dat: polls_8, polls_23, ...)
imput_poll <- function(dat = data_structural) {
  lead_days <- as.integer(sort(unique(gsub("^polls_([0-9]+)$", "\\1", names(dat)[grepl("^polls_[0-9]+$", names(dat))]))))
  for (ld in lead_days) {
    pcol <- paste0("polls_", ld)
    nacol <- paste0("pollsNA_", ld)
    if (!pcol %in% names(dat) || !nacol %in% names(dat)) next
    m <- lm(as.formula(paste(pcol, "~ voteshare_l1")), filter(dat, !!sym(nacol) == 0))
    dat[[pcol]][dat[[nacol]] == 1] <- predict(m, filter(dat, !!sym(nacol) == 1) %>% select(voteshare_l1)) +
      rnorm(sum(dat[[nacol]] == 1), mean = 0, sd = sigma(m))
  }
  return(dat)
}

# Function to run one iteration of the loop for processing election data
process_election_lm <- function(elec, 
                                predictor_sets = list(
                                    "months" = c("polls_months", "voteshare_l1", "pm", "gov"),
                                    "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov"),
                                    "days" = c("polls_days", "voteshare_l1", "pm", "gov")
                                )) {
  
  # Step 1: Mask the selected election results and prepare the data for processing
  dat_list <- mask_and_select(sel = elec)
  dat_masked <- dat_list$dat_masked
  
  # Step 2: Impute missing poll data in the masked dataset
  dat_masked <- imput_poll(dat_masked)
  
  # Step 3: Initialize an empty list to store results from each predictive model
  all_res_dat <- list()
  
  # Step 4: Loop through each set of predictors (e.g., months, weeks, days)
  for (name in names(predictor_sets)) {
    
    # Step 4a: Create a linear model formula based on the current set of predictors
    lm_form <- as.formula(paste("voteshare ~", paste(predictor_sets[[name]], collapse = " + ")))
    
    # Step 4b: Fit the linear model using the masked data
    mdl <- lm(lm_form, data = dat_masked)
    
    # Step 4c: Prepare the prediction data by filtering for the selected election and selecting the relevant predictors
    pred_dat <- dat_masked %>% filter(elec_ind == elec) %>% select(predictor_sets[[name]])
    
    # Step 4d: Predict the vote share using the fitted model
    res_dat <- dat_list$dat_results
    res_dat$pred <- predict(mdl, pred_dat)
    
    # Step 4e: Add an identifier to the results indicating which predictor set was used
    res_dat$model_id <- paste0("model_", name)
    
    # Step 4f: Store the results in the list
    all_res_dat[[name]] <- res_dat
  }
  
  # Step 5: Combine the results from all models into a single data frame
  combined_res_dat <- do.call(rbind, all_res_dat)
  
  # Step 6: Return the combined results
  return(combined_res_dat)
}

# Function to run regression models and add predictions with confidence intervals
run_regression_models <- function(data, forecast_data,  
                                  models, model_forecast) {
  
  
  # Initialize a list to store model summaries
  model_summaries <- list()
  
  # Fit the models and store the summaries
  for (model_name in names(models)) {
    model <- lm(as.formula(models[[model_name]]), data)
    model_summaries[[model_name]] <- model
    print(summary(model_summaries[[model_name]]))  # Print the summary
  }
  
  # Select the relevant columns from the forecast data
  dat_fore <- forecast_data %>% select(names(coef(model_summaries[[model_forecast]]))[-1])
  
  # Add predictions with confidence intervals using the first model (m1 as example)
  forecast_data_pred <- predict(lm(models[[model_forecast]], data = data), newdata = dat_fore, interval = "prediction")
  forecast_data <- cbind(forecast_data,forecast_data_pred)
  
  return(list("model_summaries" = model_summaries, "forecast_data" = forecast_data))
}


# Function to mask results and select
mask_and_select <- function(sel = "nw_2022-05-15", dat = data_structural){
  
  # Step 1: Extract the election date of the selected election
  date <- dat %>% filter(elec_ind == sel) %>% pull(electiondate) %>% unique()
  
  # Step 2: Filter the data to include only elections that occurred before the selected election date
  # and include the selected election itself
  dat <- dat %>% filter(electiondate < date | elec_ind == sel)
  
  # Step 3: Extract the vote share data and relevant columns (party, state, election index, and year)
  dat_voterest <- dat %>% 
    filter(elec_ind == sel) %>% 
    dplyr::select(voteshare, party, state, elec_ind, year)
  
  # Step 4: Assign a unique identifier (pid) to each row in the extracted data
  dat_voterest$pid <- 1:nrow(dat_voterest)
  
  # Step 5: Mask the vote share results of the selected election by replacing them with NA
  dat <- dat %>% mutate(
    voteshare = case_when(elec_ind == sel ~ NA, TRUE ~ voteshare)
  )
  
  # Step 6: Return a list containing the masked data and the extracted vote share data
  return(list("dat_masked" = dat, "dat_results" = dat_voterest))
  
}

# Function to turn data into a list for Stan model input
turn_in_stanlist <- function(dat, 
                             predictors = c("polls_weeks","voteshare_l1","pm","gov"),
                             dependent  = "voteshare"){
  
  # Step 1: Arrange data by election index and state
  prep_dat <- dat %>% arrange(elec_ind, state)
  
  # Step 2: Create a matrix of election results (dependent variable)
  election_res <- as.matrix(prep_dat[, dependent])
  
  # Step 3: Create a matrix of predictors for past elections
  election_pred <- as.matrix(prep_dat[, predictors])
  
  # Step 4: Extract party names and calculate the number of parties
  party_names <- prep_dat$party
  nParties <- length(party_names) # Number of parties in upcoming election
  nParties_vec <- as.vector(table(prep_dat$elec_ind)) # Number of parties in all elections
  pid <- as.numeric(as.factor(party_names))
  
  # Step 5: Identify observed and missing election results
  ii_obs <- which(complete.cases(c(election_res))) # Index of observed elections for Stan
  ii_mis <- which(!complete.cases(c(election_res))) # Index of missing election results
  ii_state <- as.numeric(as.factor(prep_dat$state)) # State indicator
  
  # Step 6: Calculate the number of years specific to each party
  year_seq <- min(prep_dat$year):max(prep_dat$year)
  NY <- length(year_seq) + 1 # Include prior year
  NY_start <- sapply(1:length(unique(pid)), function(pid_id) min(prep_dat$year[pid == pid_id]))
  NY_prior <- sapply(NY_start, function(y) which(year_seq == y))
  NY_party <- NY - NY_prior + 1
  
  # Step 7: Create a list containing all necessary data for the Stan model
  forstan <- list(
    NE = length(unique(prep_dat$elec_ind)), # Number of elections
    NE_mis = length(unique(prep_dat$elec_ind[ii_mis])), # Number of elections with missing results
    Nobs = length(ii_obs), # Number of observations
    Nmis = length(ii_mis), # Number of missing outcomes
    N = nrow(election_res), # Total number of rows in election results
    v_obs = c(election_res[ii_obs,]),  # Observed dependent variable (vote share)
    x = election_pred, # Predictors matrix
    K = ncol(election_pred),   # Number of predictors
    p = nParties_vec, # Number of parties in the different elections
    s = ii_state, # State indicator
    S = max(ii_state), # Number of states
    ii_obs = ii_obs, # Index of observed election results
    ii_mis = ii_mis,  # Index of missing election results
    p_mis = length(ii_mis), # Number of missing outcomes
    pid = pid, # Party ID
    NP = length(unique(pid)), # Number of unique parties
    NY = NY, # Number of years considered
    year_partyprior = NY_prior, # Prior year for each party
    year = prep_dat$year - (min(prep_dat$year) - 2) # Adjusted year sequence
  )
  
  # Step 8: Return the list for Stan model input
  return(forstan)
  
}

# Function to impute missing poll data
imput_poll <- function(dat = data_structural){
  
  # Step 1: Impute missing data for 'polls_days' using linear regression
  m_days <- lm(polls_days ~ voteshare_l1, filter(dat, pollsNA_days == 0)) 
  dat$polls_days[dat$pollsNA_days == 1] <- predict(m_days, filter(dat, pollsNA_days == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_days == 1), mean = 0, sd = sigma(m_days))
  
  # Step 2: Impute missing data for 'polls_weeks' using linear regression
  m_weeks <- lm(polls_weeks ~ voteshare_l1, filter(dat, pollsNA_weeks == 0)) 
  dat$polls_weeks[dat$pollsNA_weeks == 1] <- predict(m_weeks, filter(dat, pollsNA_weeks == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_weeks == 1), mean = 0, sd = sigma(m_weeks))
  
  # Step 3: Impute missing data for 'polls_months' using linear regression
  m_mnts <- lm(polls_months ~ voteshare_l1, filter(dat, pollsNA_months == 0)) 
  dat$polls_months[dat$pollsNA_months == 1] <- predict(m_mnts, filter(dat, pollsNA_months == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_months == 1), mean = 0, sd = sigma(m_mnts))
  
  # Step 4: Return the dataset with imputed poll data
  return(dat)
}

# Function to compute credible intervals for forecast samples
credible_intervals <- function(dat_smpl, alpha = (1/6)/2) {
  
  # Step 1: Calculate credible intervals, mean, median, and standard deviation for each variable
  intervals <- dat_smpl %>%
    summarise_all(list(
      lower = ~quantile(.x, probs = alpha),
      median = ~median(.x),
      mean = ~mean(.x),
      sd = ~sd(.x),
      upper = ~quantile(.x, probs = 1 - alpha)
    ))
  
  # Step 2: Reshape the data for easier interpretation and merge with party ID
  intervals_long <- intervals %>%
    pivot_longer(cols = everything(),
                 names_to = c("variable", ".value"),
                 names_pattern = "(.*)_(.*)") %>%
    mutate(pid = str_extract(variable, "\\d+") %>% as.integer())
  
  # Step 3: Return the credible intervals
  return(intervals_long)
}

# Function to run Stan model with specified predictors and process the results
run_stan_with_predictors <- function(dat_masked, dat_list, mdl, num_iter, num_warmup, nchains, cores_per_stan, predictors) {
  
  # Step 1: Prepare data for Stan model using the specified predictors
  forstan <- turn_in_stanlist(dat_masked, predictors = predictors)
  
  # Step 2: Run the Stan model with the specified parameters (number of iterations, warmup, chains, and cores)
  res_smpl <- stan(file = mdl, data = forstan,
                   iter = num_iter, warmup = num_warmup, 
                   chains = nchains, cores = cores_per_stan)
  
  # Step 3: Convert Stan samples to a data frame and compute credible intervals for forecasted values
  forcast_smpl <- as.data.frame(res_smpl, pars = "v_mis")
  forcast_ci <- credible_intervals(forcast_smpl)
  
  # Step 4: Merge the forecast results with the original data results based on party ID
  res_dat <- left_join(dat_list$dat_results, forcast_ci, by = "pid")
  
  # Step 5: Return the merged data with forecast results
  return(res_dat)
}

# Function to run one iteration of the loop
process_election_stan <- function(elec,  mdl = "model_code/dirichlet_fundamentals_eval.stan",
                                  predictor_sets = list(
                                    "months" = c("polls_months", "voteshare_l1", "pm", "gov"),
                                    "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov"),
                                    "days" = c("polls_days", "voteshare_l1", "pm", "gov")
                                  )) {
  
  # Mask and prepare the data
  dat_list <- mask_and_select(sel = elec)
  dat_masked <- dat_list$dat_masked
  dat_masked <- imput_poll(dat_masked)
  
  # Initialize an empty list to store results for each model
  all_res_dat <- list()
  
  # Loop over each set of predictors
  for (name in names(predictor_sets)) {
    # Run the model with the current set of predictors
    res_dat <- run_stan_with_predictors(
      dat_masked = dat_masked,
      dat_list = dat_list,
      mdl = mdl,
      num_iter = num_iter,
      num_warmup = num_warmup,
      nchains = nchains,
      cores_per_stan = cores_per_stan,
      predictors = predictor_sets[[name]]
    )
    
    # Store the result with an identifier for the predictor set
    res_dat$model_id <- paste0("model_", name)
    all_res_dat[[name]] <- res_dat
  }
  
  # Combine the results from all models into a single data frame
  combined_res_dat <- do.call(rbind, all_res_dat)
  
  return(combined_res_dat)
}


# Impute Polls
impute_polls <- function(var, na_var) {
  model <- lm(as.formula(paste(var, "~ voteshare_l1")), filter(data_structural, !!sym(na_var) == 0))
  data_structural[[var]][data_structural[[na_var]] == 1] <- predict(model, filter(data_structural, !!sym(na_var) == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(data_structural[[na_var]] == 1), mean = 0, sd = sigma(model))
  plot(data_structural[[var]] ~ voteshare_l1, data_structural)
}


# Functions for Data =====  

# Define the general function
generate_forecast_data <- function(data = forecast_data, election_id, model,
                                   inv_function = function(x) {exp(x)/(1 + exp(x))},
                                   alpha = 1/6, party_colors, caption_text) {

  # Include all parties (including oth/Sonstige); model is trained with oth
  pred_dat <- data %>%
    filter(elec_ind == election_id)

  pred_weeks <- pred_dat %>%
    select(names(coef(model))[-1])
  # Impute NA by column median (avoid 0: in log-ratio space 0 = 50%, compresses party differences).
  # If a column is entirely NA (e.g. no state polls), use 0 so prediction runs; forecast will be weak.
  for (j in seq_len(ncol(pred_weeks))) {
    na_idx <- is.na(pred_weeks[[j]])
    if (any(na_idx)) {
      med <- median(pred_weeks[[j]], na.rm = TRUE)
      if (!is.finite(med)) {
        med <- 0
        if (all(na_idx)) warning("Predictor ", names(pred_weeks)[j], " is all NA for ", election_id, "; imputing 0. Forecast may be flat.")
      }
      pred_weeks[[j]][na_idx] <- med
    }
  }

  pred_sim <- posterior_predict(model, pred_weeks)
  if (!is.null(inv_function)) pred_sim <- apply(pred_sim, 2, inv_function)
  # Sonstige is a party in the model: we produce draws for all parties (including oth), then
  # normalize each draw to sum to 100%; we do not replace oth with 1 - sum(others).
  pred_sim <- sweep(pred_sim, 1, rowSums(pred_sim), "/")
  # Point estimate = median (50% quantile) so interval has "direction" for skewed posteriors
  pred_fit <- apply(pred_sim, 2, quantile, probs = 0.5)
  pred_lwr <- apply(pred_sim, 2, quantile, probs = alpha / 2)   # 1/12
  pred_upr <- apply(pred_sim, 2, quantile, probs = 1 - (alpha / 2))  # 11/12

  pred_dat <- cbind(pred_dat, fit = pred_fit, lwr = pred_lwr, upr = pred_upr)
  return(pred_dat)
}


# Function to get posterior draws in long format
get_posterior_draws <- function(data = forecast_data, election_id, model,
                                inv_function = function(x) {exp(x)/(1 + exp(x))}) {

  # Include all parties (including oth/Sonstige); model is trained with oth
  pred_dat <- data %>%
    filter(elec_ind == election_id)

  pred_weeks <- pred_dat %>%
    select(names(coef(model))[-1])
  for (j in seq_len(ncol(pred_weeks))) {
    na_idx <- is.na(pred_weeks[[j]])
    if (any(na_idx)) {
      med <- median(pred_weeks[[j]], na.rm = TRUE)
      if (!is.finite(med)) med <- 0
      pred_weeks[[j]][na_idx] <- med
    }
  }

  pred_sim <- posterior_predict(model, pred_weeks)
  if (!is.null(inv_function)) pred_sim <- apply(pred_sim, 2, inv_function)
  # Sonstige is a party: draws for all parties (including oth), then normalize to sum to 100%
  pred_sim <- sweep(pred_sim, 1, rowSums(pred_sim), "/")

  pred_long <- as.data.frame(pred_sim) %>%
    mutate(draw = row_number()) %>%
    pivot_longer(cols = -draw, names_to = "party", values_to = "posterior_draw") %>%
    mutate(party = pred_dat$party[as.numeric(gsub("V", "", party))],
           elec_ind = election_id,
           state = unique(pred_dat$state),
           date = unique(pred_dat$electiondate))
  return(pred_long)
}

# Default scope -> land (2-letter) mapping for state elections; API base configurable via POLLING_API_BASE
# Include "st" and "sa" so API scope "st" or "sa" (Sachsen-Anhalt) maps to land "st"
SCOPE_TO_LAND <- c(
  "baden-württemberg" = "bw", "baden-wuerttemberg" = "bw", "bayern" = "by", "berlin" = "be",
  "brandenburg" = "bb", "bremen" = "hb", "hamburg" = "hh", "hessen" = "he",
  "mecklenburg-vorpommern" = "mv", "niedersachsen" = "ni", "nordrhein-westfalen-nrw" = "nw", "nrw" = "nw",
  "rheinland-pfalz" = "rp", "sa" = "st", "saarland" = "sl", "sachsen" = "sn", "sachsen-anhalt" = "st", "sachsenanhalt" = "st", "st" = "st",
  "schleswig-holstein" = "sh", "thüringen" = "th", "thueringen" = "th"
)

#' Get state election IDs to forecast from API v1/elections: future only, at most max_months_ahead before election.
#' Returns character vector of elec_ind (e.g. "bw_2026-03-08"). API base from env POLLING_API_BASE.
get_elections_to_forecast <- function(api_base = Sys.getenv("POLLING_API_BASE", "https://api.fasttrack29.com"),
                                      max_months_ahead = 12,
                                      scope_to_land = SCOPE_TO_LAND) {
  if (nzchar(api_base) && endsWith(api_base, "/")) api_base <- sub("/+$", "", api_base)
  cache_bust <- as.character(as.numeric(Sys.time()))
  api_headers <- httr::add_headers(
    "Cache-Control" = "no-cache, no-store, must-revalidate",
    "Pragma" = "no-cache",
    "User-Agent" = paste0("state-models/1 (", cache_bust, ")")
  )
  resp <- httr::GET(httr::modify_url(file.path(api_base, "v1", "elections"), query = list("_t" = cache_bust)), api_headers)
  if (httr::status_code(resp) != 200) {
    stop("v1/elections failed: HTTP ", httr::status_code(resp), ". API base: ", api_base)
  }
  elections <- httr::content(resp, as = "parsed")
  today <- as.Date(Sys.Date())
  window_end <- today + lubridate::period(max_months_ahead, units = "months")
  elec_ind <- character(0)
  for (e in elections) {
    if (!identical(e$election_type, "Landtagswahl") || is.null(e$scope) || e$scope %in% c("federal", "eu")) next
    scope <- trimws(tolower(as.character(e$scope)))
    # API schema uses election_date (ISO string) and date_is_estimated (bool). Some backends may also provide e$date as bool.
    edate <- e$election_date %||% e$election_date_iso %||% e$electiondate %||% e$date
    if (is.null(edate)) next
    # Guard against empty strings / non-ISO dates coming back from the API
    edate_chr <- as.character(edate)
    if (!nzchar(edate_chr)) next
    edate <- suppressWarnings(tryCatch(as.Date(edate_chr), error = function(err) NA))
    if (is.na(edate)) next
    if (edate < today || edate > window_end) next
    land <- scope_to_land[scope]
    if (is.na(land)) next
    elec_ind <- c(elec_ind, paste(unname(land), format(edate, "%Y-%m-%d"), sep = "_"))
  }
  unique(elec_ind)
}

# Minimal %||% for optional fields
`%||%` <- function(x, y) if (is.null(x)) y else x

# Land codes used in elec_ind / polls CSV → FastTrack v2 scope
LAND_TO_V2_SCOPE <- c(
  bw = "bw", by = "by", be = "be", bb = "bb", hb = "hb", hh = "hh", he = "he",
  mv = "mv", ni = "ni", nw = "nrw", rp = "rp", sl = "sl", sn = "sn", st = "st",
  sh = "sh", th = "th"
)

#' Newest poll date per land from a polls data frame (columns date, land; optional poll_share).
last_poll_dates_by_land <- function(polls) {
  if (is.null(polls) || !nrow(polls) || !all(c("date", "land") %in% names(polls))) {
    return(setNames(as.Date(character()), character()))
  }
  polls <- polls
  polls$date <- as.Date(polls$date)
  polls$land <- tolower(as.character(polls$land))
  if ("poll_share" %in% names(polls)) {
    polls <- polls[!is.na(polls$poll_share), , drop = FALSE]
  }
  polls <- polls[!is.na(polls$date) & nzchar(polls$land), , drop = FALSE]
  if (!nrow(polls)) return(setNames(as.Date(character()), character()))
  out <- tapply(polls$date, polls$land, function(d) max(d, na.rm = TRUE))
  setNames(as.Date(unname(out), origin = "1970-01-01"), names(out))
}

#' Fetch newest published_date per land via FastTrack v2 (one page, limit=1).
#' Used to anchor lead_days before 01_build_data has written the polls CSV.
fetch_last_state_poll_dates <- function(lands,
                                        api_base = Sys.getenv("POLLING_API_BASE", "https://api.fasttrack29.com")) {
  if (nzchar(api_base) && endsWith(api_base, "/")) api_base <- sub("/+$", "", api_base)
  lands <- unique(tolower(as.character(lands)))
  lands <- lands[nzchar(lands)]
  if (!length(lands)) return(setNames(as.Date(character()), character()))
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr required for fetch_last_state_poll_dates")
  }
  cache_bust <- as.character(as.numeric(Sys.time()))
  headers <- httr::add_headers(
    "Cache-Control" = "no-cache, no-store, must-revalidate",
    "Pragma" = "no-cache",
    "User-Agent" = paste0("state-models/last-poll (", cache_bust, ")")
  )
  out <- setNames(as.Date(rep(NA, length(lands))), lands)
  for (land in lands) {
    scope <- LAND_TO_V2_SCOPE[[land]] %||% land
    url <- httr::modify_url(
      file.path(api_base, "v2", "polls"),
      query = list(
        scope = scope,
        include_results = "true",
        limit = 1L,
        offset = 0L,
        sort = "-published_date",
        "_t" = cache_bust
      )
    )
    resp <- tryCatch(httr::GET(url, headers, httr::timeout(60)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200L) next
    data <- httr::content(resp, as = "parsed")
    items <- data$data
    if (is.null(items) || !length(items)) next
    pub <- items[[1]]$published_date %||% items[[1]]$publish_date
    if (is.null(pub) || !nzchar(as.character(pub))) next
    out[[land]] <- suppressWarnings(as.Date(as.character(pub)))
  }
  # Prefer newer Stand from website-pipeline DAWUM Landtag scrape when available.
  wp <- Sys.getenv("WEBSITE_PIPELINE_ROOT", "")
  fetch_r <- file.path(wp, "R", "fetch_polls.R")
  if (nzchar(wp) && file.exists(fetch_r)) {
    if (!exists("fetch_dawum_state_polls", mode = "function")) {
      cfg <- file.path(wp, "R", "config.R")
      if (file.exists(cfg)) source(cfg, local = FALSE)
      source(fetch_r, local = FALSE)
    }
    for (land in lands) {
      scope <- LAND_TO_V2_SCOPE[[land]] %||% land
      scraped <- tryCatch(fetch_dawum_state_polls(scope), error = function(e) list())
      if (!length(scraped)) next
      dates <- vapply(scraped, function(p) {
        as.character(p$publish_date %||% p$published_date %||% "")
      }, character(1))
      dates <- dates[nzchar(dates)]
      if (!length(dates)) next
      dawum_stand <- suppressWarnings(max(as.Date(dates), na.rm = TRUE))
      if (is.na(dawum_stand)) next
      api_stand <- out[[land]]
      if (is.null(api_stand) || is.na(api_stand) || dawum_stand > api_stand) {
        out[[land]] <- dawum_stand
      }
    }
  }
  out
}

#' Exact lead days for forecast elections, anchored on per-state last-poll Stand
#' (not calendar today). Falls back to Sys.Date() when a land has no poll date.
lead_days_from_last_polls <- function(elections_to_forecast,
                                      stand_by_land = NULL,
                                      api_base = Sys.getenv("POLLING_API_BASE", "https://api.fasttrack29.com"),
                                      polls_csv = NULL) {
  elections_to_forecast <- unique(trimws(as.character(elections_to_forecast)))
  elections_to_forecast <- elections_to_forecast[nzchar(elections_to_forecast)]
  if (!length(elections_to_forecast)) {
    stop("lead_days_from_last_polls: no elections")
  }
  lands <- unique(sub("_.*", "", elections_to_forecast))
  # Named Date [[ fails on a missing land (empty vector after a fresh clone
  # with no data/output/01_state-polls.csv). Keep a list while merging.
  as_stand_list <- function(x) {
    if (is.null(x) || !length(x)) return(list())
    if (is.list(x)) return(x)
    as.list(x)
  }
  stand_by_land <- as_stand_list(stand_by_land)
  if (!length(stand_by_land) && !is.null(polls_csv) && file.exists(polls_csv)) {
    polls <- tryCatch(utils::read.csv(polls_csv, stringsAsFactors = FALSE), error = function(e) NULL)
    stand_by_land <- as_stand_list(last_poll_dates_by_land(polls))
  }
  # Always refresh Stand from API (+ optional DAWUM scrape) and keep the newer
  # of CSV vs live — otherwise a stale polls CSV pins lead days behind new polls.
  fetched <- fetch_last_state_poll_dates(lands, api_base = api_base)
  for (land in lands) {
    live <- if (land %in% names(fetched)) fetched[[land]] else as.Date(NA)
    if (is.null(live) || is.na(live)) next
    prev <- stand_by_land[[land]]
    if (is.null(prev) || is.na(prev) || live > prev) {
      stand_by_land[[land]] <- as.Date(live)
    }
  }
  stand_nms <- names(stand_by_land)
  stand_by_land <- setNames(
    as.Date(vapply(stand_by_land, function(d) as.character(as.Date(d)), character(1))),
    stand_nms
  )
  stand_by_land <- stand_by_land[!duplicated(names(stand_by_land))]
  fallback <- as.Date(Sys.Date())
  leads <- vapply(elections_to_forecast, function(eid) {
    land <- sub("_.*", "", eid)
    ed <- as.Date(sub(".*_", "", eid))
    stand <- if (land %in% names(stand_by_land)) stand_by_land[[land]] else as.Date(NA)
    if (is.null(stand) || is.na(stand)) stand <- fallback
    as.numeric(ed - as.Date(stand))
  }, numeric(1))
  leads <- sort(unique(as.integer(leads)))
  if (!length(leads) || any(leads <= 0L)) {
    stop(
      "Lead days from last-poll Stand must be positive. Got: ",
      paste(leads, collapse = ", "),
      ". Stands: ",
      paste(names(stand_by_land), stand_by_land, sep = "=", collapse = ", ")
    )
  }
  attr(leads, "stand_by_land") <- stand_by_land
  leads
}

#' Sanity check forecasts: no NA for major parties, fit in [0,1].
#' Sonstige (oth) is included in training and forecast; not checked here.
validate_forecast_sanity <- function(fcst_df,
                                     major_parties = c("cdu", "spd", "gru", "afd")) {
  stopifnot("fit" %in% names(fcst_df), "party" %in% names(fcst_df), "elec_ind" %in% names(fcst_df))
  for (eid in unique(fcst_df$elec_ind)) {
    sub <- fcst_df %>% filter(elec_ind == eid)
    for (p in major_parties) {
      row <- sub %>% filter(party == p)
      if (nrow(row) == 0) {
        stop("Forecast sanity check failed: no forecast for major party ", p, " in election ", eid, ".")
      }
      fit_val <- row$fit[1]
      if (is.na(fit_val)) {
        stop("Forecast sanity check failed: missing value for major party ", p, " in election ", eid, ".")
      }
      if (fit_val < 0 || fit_val > 1) {
        stop("Forecast sanity check failed: fit for ", p, " in ", eid, " is ", fit_val, " (must be in [0, 1]).")
      }
    }
  }
  invisible(TRUE)
}

# Define the function to map party codes to party names
map_party_names <- function(party_codes) {
  party_labels <- case_when(
    party_codes == "afd" ~ "AfD",
    party_codes == "bsw" ~ "BSW",
    party_codes == "cdu" ~ "CDU",
    party_codes == "gru" ~ "Greens",
    party_codes == "spd" ~ "SPD",
    party_codes == "lin" ~ "Linke",
    party_codes == "fdp" ~ "FDP",
    TRUE ~ "Other"  # Default label for any unspecified party code
  )
  
  return(party_labels)
}

# Define party colors
party_colors <- c(
  AfD = "#009DE0",      # AfD (Blue)
  BSW = "#800080",      # BSW (Yellow)
  CDU = "#151518",      # CDU (Black)
  FDP = "#FFED00",      # FDP (Yellow)
  FW = "#FF8000",       # FW (Orange)
  Greens = "#409A3C",   # Greens (Green)
  Linke = "#BE3075",    # Linke (Dark Red)
  Other = "#808080",    # Other (Grey)
  SPD = "#E3000F"       # SPD (Red)
)



# Define the function to map state codes to state names
map_state_names <- function(state_codes) {
  state_labels <- case_when(
    state_codes == "bb" ~ "Brandenburg",
    state_codes == "be" ~ "Berlin",
    state_codes == "bw" ~ "Baden-Württemberg",
    state_codes == "by" ~ "Bavaria",
    state_codes == "hb" ~ "Bremen",
    state_codes == "he" ~ "Hesse",
    state_codes == "hh" ~ "Hamburg",
    state_codes == "mv" ~ "Mecklenburg-Vorpommern",
    state_codes == "ni" ~ "Lower Saxony",
    state_codes == "nw" ~ "North Rhine-Westphalia",
    state_codes == "rp" ~ "Rheinland-Pfalz",
    state_codes == "sh" ~ "Schleswig-Holstein",
    state_codes == "sl" ~ "Saarland",
    state_codes == "sn" ~ "Saxony",
    state_codes == "st" ~ "Saxony-Anhalt",
    state_codes == "th" ~ "Thuringia",
    
    TRUE ~ "Other"  # Default label for any unspecified party code
  )
  
  return(state_labels)
}

# Define the function to generate the election forecast plot
generate_forecast_plot <- function(dat, title, subtitle, party_colors, caption_text) {
  y_max <- max(0.5, min(1, max(dat$upr, na.rm = TRUE) + 0.05))
  y_breaks <- seq(0, 1, 0.1)
  y_breaks <- y_breaks[y_breaks <= y_max]

  plot <- ggplot(data = dat, aes(x = reorder(party_name, -fit), y = fit)) +
    geom_hline(yintercept = 0.05, linetype = "dotted", size = 1.5, color = "black") +
    geom_linerange(aes(ymin = lwr, ymax = upr), linewidth = 10, alpha = 0.3, col = party_colors[dat$party_name]) +
    geom_point(size = 6, color = "white", shape = 21, stroke = 2, fill = party_colors[dat$party_name]) +
    geom_point(size = 2, fill = "white", shape = 21) +
    geom_label(aes(y = upr + 0.03, label = paste(round(fit * 100, 1), "%"))) +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = y_breaks,
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption_text,
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text(size = 12, face = "bold", color = "black"),
      axis.line.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )

  return(plot)
}


# # Get Latnet Support using random walk model ==== 
get_latent_support <- function(data, party, date, last_election_date,
                               var_party, var_date, var_percent) {
  
  #
  # Reuires DLM
  require(dlm)
  
  # Convert date column to Date class if not already
  data[[var_date]] <- as.Date(data[[var_date]])
  last_election_date <- as.Date(last_election_date)
  date <- as.Date(date)
  if (is.na(last_election_date) || is.na(date)) return(NA)
  # If the requested date is on/before the last election, we cannot build a forward daily sequence.
  if (date <= last_election_date) return(NA)
  
  # Include polls on stand_date ("as of Stand"); exclusive upper bound would drop the last poll.
  filtered_data <- data[data[[var_party]] == party & data[[var_date]] <= date, ]
  
  # Ensure the data is sorted by date
  filtered_data <- filtered_data[order(filtered_data[[var_date]]), ]
  
  # Times series format from last elction to date
  complete_data <- data.frame(Datum = seq.Date(from = last_election_date, to = date, by = "day"))
  
  merged_data <- merge(complete_data, filtered_data, by.x = "Datum", by.y = "date", all.x = TRUE)
  
  # Create Time-series data with missing values
  start_date <- min(merged_data$Datum)
  ts_data <- ts(merged_data$poll_share, start = c(as.numeric(format(start_date, "%Y")),
                                                  as.numeric(format(start_date, "%j"))), frequency = 365)
  
  # Define the DLM model with a random walk component
  build_dlm <- function(param) {
    dlmModPoly(order = 1, dV = exp(param[1]), dW = exp(param[2]))
  }
  
  # Fit the model using Maximum Likelihood Estimation (Lapack can fail on sparse series)
  fit <- tryCatch(
    dlmMLE(ts_data, parm = c(0, 0), build = build_dlm),
    error = function(e) {
      warning("dlmMLE failed for party=", party, " asof=", date, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NA)
  
  # Build the final model with estimated parameters
  model <- build_dlm(fit$par)
  
  # Apply the Kalman filter to estimate the latent support
  filtered <- dlmFilter(ts_data, model)
  
  # Apply smoothing to get a refined estimate of the latent support
  smoothed <- dlmSmooth(filtered)
  latent_support <- smoothed$s[-1]  # Remove the first NA value
  
  # Return the latent support for the specified date
  if (nrow(filter(filtered_data, !is.na(poll_share))) > 1) return(latent_support[length(latent_support)]) else return(NA)
}


# Get Latnet Support using random walk model ==== 
# Adapted for use with land
get_latent_support_land <- function(data, party, land, date, last_election_date,
                                    var_party, var_date, var_percent, var_land) {
  
  # data <- current_polls
  # party = "spd"
  # land = "th"
  # last_election_date = ymd("2019-09-01")
  # var_party = "party"
  # var_date = "date"
  # var_percent = "poll_share"
  # date = ymd("2024-08-24")
  
  # Reuires DLM
  require(dlm)
  
  # Convert date column to Date class if not already
  data[[var_date]] <- as.Date(data[[var_date]])
  last_election_date <- as.Date(last_election_date)
  date <- as.Date(date)
  if (is.na(last_election_date) || is.na(date)) return(NA)
  # If the requested forecast date is on/before the last election, we cannot build a forward daily sequence.
  if (date <= last_election_date) return(NA)
  
  # Include polls on stand_date ("as of Stand"); exclusive upper bound would drop the last poll.
  filtered_data <- data[data[[var_party]] == party & data[[var_date]] <= date & data[[var_land]] == land, ]
  
  # Ensure the data is sorted by date
  filtered_data <- filtered_data[order(filtered_data[[var_date]]), ]
  
  # Times series format from last election to date
  complete_data <- data.frame(Datum = seq.Date(from = last_election_date, to = date, by = "day"))
  
  merged_data <- merge(complete_data, filtered_data, by.x = "Datum", by.y = "date", all.x = TRUE)
  
  # Create Time-series data with missing values
  start_date <- min(merged_data$Datum)
  ts_data <- ts(merged_data[[var_percent]], start = c(as.numeric(format(start_date, "%Y")), 
                                                      as.numeric(format(start_date, "%j"))), frequency = 365)
  
  # Define the DLM model with a random walk component
  build_dlm <- function(param) {
    dlmModPoly(order = 1, dV = exp(param[1]), dW = exp(param[2]))
  }
  
  # Fit the model using Maximum Likelihood Estimation (Lapack can fail on sparse series)
  fit <- tryCatch(
    dlmMLE(ts_data, parm = c(0, 0), build = build_dlm),
    error = function(e) {
      warning("dlmMLE failed for party=", party, " land=", land, " asof=", date, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NA)
  
  # Build the final model with estimated parameters
  model <- build_dlm(fit$par)
  
  # Apply the Kalman filter to estimate the latent support
  filtered <- dlmFilter(ts_data, model)
  
  # Apply smoothing to get a refined estimate of the latent support
  smoothed <- dlmSmooth(filtered)
  latent_support <- smoothed$s[-1]  # Remove the first NA value
  
  # Return the latent support for the specified date (need >1 non-NA poll points for DLM)
  n_valid <- sum(!is.na(filtered_data[[var_percent]]))
  if (n_valid > 1) return(latent_support[length(latent_support)]) else return(NA)
}


# Function to mask results and select
mask_and_select <- function(sel = "nw_2022-05-15", dat = data_structural){
  
  # Step 1: Determine the election date for the selected election
  date <- dat %>% filter(elec_ind == sel) %>% pull(electiondate) %>% unique()
  
  # Step 2: Filter the data to include only elections that occurred before the selected election or the selected election itself
  dat <- dat %>% filter(electiondate < date | elec_ind == sel)
  
  # Step 3: Extract vote share data for the selected election, including relevant columns (party, state, election index, and year)
  dat_voterest  <- dat %>%
    filter(elec_ind == sel) %>% 
    dplyr::select(voteshare, party, state, elec_ind, year)
  
  # Step 4: Assign a unique ID (pid) to each row in the extracted data
  dat_voterest$pid <- 1:nrow(dat_voterest)
  
  # Step 5: Mask the vote share results for the selected election (replace with NA)
  dat <- dat %>% mutate(
    voteshare = case_when(elec_ind == sel ~ NA, TRUE ~ voteshare)
  )
  
  # Step 6: Return a list containing the masked data and the extracted vote share data
  return(list("dat_masked" = dat, "dat_results" = dat_voterest))
  
}

# Function to impute missing poll data (lead days inferred from dat: polls_8, polls_23, ...)
imput_poll <- function(dat = data_structural) {
  lead_days <- as.integer(sort(unique(gsub("^polls_([0-9]+)$", "\\1", names(dat)[grepl("^polls_[0-9]+$", names(dat))]))))
  for (ld in lead_days) {
    pcol <- paste0("polls_", ld)
    nacol <- paste0("pollsNA_", ld)
    if (!pcol %in% names(dat) || !nacol %in% names(dat)) next
    m <- lm(as.formula(paste(pcol, "~ voteshare_l1")), filter(dat, !!sym(nacol) == 0))
    dat[[pcol]][dat[[nacol]] == 1] <- predict(m, filter(dat, !!sym(nacol) == 1) %>% select(voteshare_l1)) +
      rnorm(sum(dat[[nacol]] == 1), mean = 0, sd = sigma(m))
  }
  return(dat)
}

# Function to run one iteration of the loop for processing election data
process_election_lm <- function(elec, 
                                predictor_sets = list(
                                    "months" = c("polls_months", "voteshare_l1", "pm", "gov"),
                                    "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov"),
                                    "days" = c("polls_days", "voteshare_l1", "pm", "gov")
                                )) {
  
  # Step 1: Mask the selected election results and prepare the data for processing
  dat_list <- mask_and_select(sel = elec)
  dat_masked <- dat_list$dat_masked
  
  # Step 2: Impute missing poll data in the masked dataset
  dat_masked <- imput_poll(dat_masked)
  
  # Step 3: Initialize an empty list to store results from each predictive model
  all_res_dat <- list()
  
  # Step 4: Loop through each set of predictors (e.g., months, weeks, days)
  for (name in names(predictor_sets)) {
    
    # Step 4a: Create a linear model formula based on the current set of predictors
    lm_form <- as.formula(paste("voteshare ~", paste(predictor_sets[[name]], collapse = " + ")))
    
    # Step 4b: Fit the linear model using the masked data
    mdl <- lm(lm_form, data = dat_masked)
    
    # Step 4c: Prepare the prediction data by filtering for the selected election and selecting the relevant predictors
    pred_dat <- dat_masked %>% filter(elec_ind == elec) %>% select(predictor_sets[[name]])
    
    # Step 4d: Predict the vote share using the fitted model
    res_dat <- dat_list$dat_results
    res_dat$pred <- predict(mdl, pred_dat)
    
    # Step 4e: Add an identifier to the results indicating which predictor set was used
    res_dat$model_id <- paste0("model_", name)
    
    # Step 4f: Store the results in the list
    all_res_dat[[name]] <- res_dat
  }
  
  # Step 5: Combine the results from all models into a single data frame
  combined_res_dat <- do.call(rbind, all_res_dat)
  
  # Step 6: Return the combined results
  return(combined_res_dat)
}

# Function to run regression models and add predictions with confidence intervals
run_regression_models <- function(data, forecast_data,  
                                  models, model_forecast) {
  
  
  # Initialize a list to store model summaries
  model_summaries <- list()
  
  # Fit the models and store the summaries
  for (model_name in names(models)) {
    model <- lm(as.formula(models[[model_name]]), data)
    model_summaries[[model_name]] <- model
    print(summary(model_summaries[[model_name]]))  # Print the summary
  }
  
  # Select the relevant columns from the forecast data
  dat_fore <- forecast_data %>% select(names(coef(model_summaries[[model_forecast]]))[-1])
  
  # Add predictions with confidence intervals using the first model (m1 as example)
  forecast_data_pred <- predict(lm(models[[model_forecast]], data = data), newdata = dat_fore, interval = "prediction")
  forecast_data <- cbind(forecast_data,forecast_data_pred)
  
  return(list("model_summaries" = model_summaries, "forecast_data" = forecast_data))
}


# Function to mask results and select
mask_and_select <- function(sel = "nw_2022-05-15", dat = data_structural){
  
  # Step 1: Extract the election date of the selected election
  date <- dat %>% filter(elec_ind == sel) %>% pull(electiondate) %>% unique()
  
  # Step 2: Filter the data to include only elections that occurred before the selected election date
  # and include the selected election itself
  dat <- dat %>% filter(electiondate < date | elec_ind == sel)
  
  # Step 3: Extract the vote share data and relevant columns (party, state, election index, and year)
  dat_voterest <- dat %>% 
    filter(elec_ind == sel) %>% 
    dplyr::select(voteshare, party, state, elec_ind, year)
  
  # Step 4: Assign a unique identifier (pid) to each row in the extracted data
  dat_voterest$pid <- 1:nrow(dat_voterest)
  
  # Step 5: Mask the vote share results of the selected election by replacing them with NA
  dat <- dat %>% mutate(
    voteshare = case_when(elec_ind == sel ~ NA, TRUE ~ voteshare)
  )
  
  # Step 6: Return a list containing the masked data and the extracted vote share data
  return(list("dat_masked" = dat, "dat_results" = dat_voterest))
  
}

# Function to turn data into a list for Stan model input
turn_in_stanlist <- function(dat, 
                             predictors = c("polls_weeks","voteshare_l1","pm","gov"),
                             dependent  = "voteshare"){
  
  # Step 1: Arrange data by election index and state
  prep_dat <- dat %>% arrange(elec_ind, state)
  
  # Step 2: Create a matrix of election results (dependent variable)
  election_res <- as.matrix(prep_dat[, dependent])
  
  # Step 3: Create a matrix of predictors for past elections
  election_pred <- as.matrix(prep_dat[, predictors])
  
  # Step 4: Extract party names and calculate the number of parties
  party_names <- prep_dat$party
  nParties <- length(party_names) # Number of parties in upcoming election
  nParties_vec <- as.vector(table(prep_dat$elec_ind)) # Number of parties in all elections
  pid <- as.numeric(as.factor(party_names))
  
  # Step 5: Identify observed and missing election results
  ii_obs <- which(complete.cases(c(election_res))) # Index of observed elections for Stan
  ii_mis <- which(!complete.cases(c(election_res))) # Index of missing election results
  ii_state <- as.numeric(as.factor(prep_dat$state)) # State indicator
  
  # Step 6: Calculate the number of years specific to each party
  year_seq <- min(prep_dat$year):max(prep_dat$year)
  NY <- length(year_seq) + 1 # Include prior year
  NY_start <- sapply(1:length(unique(pid)), function(pid_id) min(prep_dat$year[pid == pid_id]))
  NY_prior <- sapply(NY_start, function(y) which(year_seq == y))
  NY_party <- NY - NY_prior + 1
  
  # Step 7: Create a list containing all necessary data for the Stan model
  forstan <- list(
    NE = length(unique(prep_dat$elec_ind)), # Number of elections
    NE_mis = length(unique(prep_dat$elec_ind[ii_mis])), # Number of elections with missing results
    Nobs = length(ii_obs), # Number of observations
    Nmis = length(ii_mis), # Number of missing outcomes
    N = nrow(election_res), # Total number of rows in election results
    v_obs = c(election_res[ii_obs,]),  # Observed dependent variable (vote share)
    x = election_pred, # Predictors matrix
    K = ncol(election_pred),   # Number of predictors
    p = nParties_vec, # Number of parties in the different elections
    s = ii_state, # State indicator
    S = max(ii_state), # Number of states
    ii_obs = ii_obs, # Index of observed election results
    ii_mis = ii_mis,  # Index of missing election results
    p_mis = length(ii_mis), # Number of missing outcomes
    pid = pid, # Party ID
    NP = length(unique(pid)), # Number of unique parties
    NY = NY, # Number of years considered
    year_partyprior = NY_prior, # Prior year for each party
    year = prep_dat$year - (min(prep_dat$year) - 2) # Adjusted year sequence
  )
  
  # Step 8: Return the list for Stan model input
  return(forstan)
  
}

# Function to impute missing poll data
imput_poll <- function(dat = data_structural){
  
  # Step 1: Impute missing data for 'polls_days' using linear regression
  m_days <- lm(polls_days ~ voteshare_l1, filter(dat, pollsNA_days == 0)) 
  dat$polls_days[dat$pollsNA_days == 1] <- predict(m_days, filter(dat, pollsNA_days == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_days == 1), mean = 0, sd = sigma(m_days))
  
  # Step 2: Impute missing data for 'polls_weeks' using linear regression
  m_weeks <- lm(polls_weeks ~ voteshare_l1, filter(dat, pollsNA_weeks == 0)) 
  dat$polls_weeks[dat$pollsNA_weeks == 1] <- predict(m_weeks, filter(dat, pollsNA_weeks == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_weeks == 1), mean = 0, sd = sigma(m_weeks))
  
  # Step 3: Impute missing data for 'polls_months' using linear regression
  m_mnts <- lm(polls_months ~ voteshare_l1, filter(dat, pollsNA_months == 0)) 
  dat$polls_months[dat$pollsNA_months == 1] <- predict(m_mnts, filter(dat, pollsNA_months == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_months == 1), mean = 0, sd = sigma(m_mnts))
  
  # Step 4: Return the dataset with imputed poll data
  return(dat)
}

# Function to compute credible intervals for forecast samples
credible_intervals <- function(dat_smpl, alpha = (1/6)/2) {
  
  # Step 1: Calculate credible intervals, mean, median, and standard deviation for each variable
  intervals <- dat_smpl %>%
    summarise_all(list(
      lower = ~quantile(.x, probs = alpha),
      median = ~median(.x),
      mean = ~mean(.x),
      sd = ~sd(.x),
      upper = ~quantile(.x, probs = 1 - alpha)
    ))
  
  # Step 2: Reshape the data for easier interpretation and merge with party ID
  intervals_long <- intervals %>%
    pivot_longer(cols = everything(),
                 names_to = c("variable", ".value"),
                 names_pattern = "(.*)_(.*)") %>%
    mutate(pid = str_extract(variable, "\\d+") %>% as.integer())
  
  # Step 3: Return the credible intervals
  return(intervals_long)
}

# Function to run Stan model with specified predictors and process the results
run_stan_with_predictors <- function(dat_masked, dat_list, mdl, num_iter, num_warmup, nchains, cores_per_stan, predictors) {
  
  # Step 1: Prepare data for Stan model using the specified predictors
  forstan <- turn_in_stanlist(dat_masked, predictors = predictors)
  
  # Step 2: Run the Stan model with the specified parameters (number of iterations, warmup, chains, and cores)
  res_smpl <- stan(file = mdl, data = forstan,
                   iter = num_iter, warmup = num_warmup, 
                   chains = nchains, cores = cores_per_stan)
  
  # Step 3: Convert Stan samples to a data frame and compute credible intervals for forecasted values
  forcast_smpl <- as.data.frame(res_smpl, pars = "v_mis")
  forcast_ci <- credible_intervals(forcast_smpl)
  
  # Step 4: Merge the forecast results with the original data results based on party ID
  res_dat <- left_join(dat_list$dat_results, forcast_ci, by = "pid")
  
  # Step 5: Return the merged data with forecast results
  return(res_dat)
}

# Function to run one iteration of the loop
process_election_stan <- function(elec,  mdl = "model_code/dirichlet_fundamentals_eval.stan",
                                  predictor_sets = list(
                                    "months" = c("polls_months", "voteshare_l1", "pm", "gov"),
                                    "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov"),
                                    "days" = c("polls_days", "voteshare_l1", "pm", "gov")
                                  )) {
  
  # Mask and prepare the data
  dat_list <- mask_and_select(sel = elec)
  dat_masked <- dat_list$dat_masked
  dat_masked <- imput_poll(dat_masked)
  
  # Initialize an empty list to store results for each model
  all_res_dat <- list()
  
  # Loop over each set of predictors
  for (name in names(predictor_sets)) {
    # Run the model with the current set of predictors
    res_dat <- run_stan_with_predictors(
      dat_masked = dat_masked,
      dat_list = dat_list,
      mdl = mdl,
      num_iter = num_iter,
      num_warmup = num_warmup,
      nchains = nchains,
      cores_per_stan = cores_per_stan,
      predictors = predictor_sets[[name]]
    )
    
    # Store the result with an identifier for the predictor set
    res_dat$model_id <- paste0("model_", name)
    all_res_dat[[name]] <- res_dat
  }
  
  # Combine the results from all models into a single data frame
  combined_res_dat <- do.call(rbind, all_res_dat)
  
  return(combined_res_dat)
}


# Impute Polls
impute_polls <- function(var, na_var) {
  model <- lm(as.formula(paste(var, "~ voteshare_l1")), filter(data_structural, !!sym(na_var) == 0))
  data_structural[[var]][data_structural[[na_var]] == 1] <- predict(model, filter(data_structural, !!sym(na_var) == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(data_structural[[na_var]] == 1), mean = 0, sd = sigma(model))
  plot(data_structural[[var]] ~ voteshare_l1, data_structural)
}