# Bayesian polls-only log-ratio coefs by lead — same priors as website forecasts.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(dlm)
  library(tidyr)
  library(ggplot2)
  library(rstanarm)
})

ROOT <- if (basename(getwd()) == "code") normalizePath("..") else normalizePath(".")
source(file.path(ROOT, "code", "auxilary", "functions.R"))
options(mc.cores = parallel::detectCores())

# Match website / 02_estimate_model.R polls-only settings
PRIOR_SCALE <- 1
PRIOR_AUX_RATE <- 2
SEED <- 238523862L

log_ratio <- function(x, c = 0.01) {
  x <- pmin(pmax(as.numeric(x), c), 1 - c)
  log(x / (1 - x))
}

latent_series <- function(data, party, land, last_election_date, end_date) {
  last_election_date <- as.Date(last_election_date)
  end_date <- as.Date(end_date)
  if (is.na(last_election_date) || is.na(end_date) || end_date <= last_election_date) {
    return(NULL)
  }
  filtered <- data %>%
    filter(
      .data$party == .env$party,
      .data$land == .env$land,
      .data$date < .env$end_date,
      .data$date >= .env$last_election_date
    ) %>%
    arrange(date)
  if (sum(!is.na(filtered$poll_share)) <= 1) return(NULL)
  complete <- data.frame(Datum = seq.Date(last_election_date, end_date, by = "day"))
  merged <- merge(complete, filtered, by.x = "Datum", by.y = "date", all.x = TRUE)
  start_date <- min(merged$Datum)
  ts_data <- ts(
    merged$poll_share,
    start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%j"))),
    frequency = 365
  )
  build_dlm <- function(param) dlmModPoly(1, dV = exp(param[1]), dW = exp(param[2]))
  fit <- tryCatch(dlmMLE(ts_data, parm = c(0, 0), build = build_dlm), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  sm <- tryCatch(dlmSmooth(dlmFilter(ts_data, build_dlm(fit$par))), error = function(e) NULL)
  if (is.null(sm)) return(NULL)
  tibble(date = merged$Datum, latent_pct = as.numeric(sm$s[-1]))
}

polls <- read_csv(file.path(ROOT, "data/output/01_state-polls.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
res <- read_csv(file.path(ROOT, "data/input/01_state-election-results.csv"), show_col_types = FALSE) %>%
  filter(
    on_ballot == 1, !is.na(vote_share),
    party %in% c("afd", "bsw", "cdu", "fdp", "gru", "lin", "spd", "oth")
  ) %>%
  mutate(
    electiondate = as.Date(electiondate),
    electiondate_l1 = as.Date(electiondate_l1),
    voteshare = vote_share / 100
  )

elec <- res %>%
  distinct(land, electiondate, electiondate_l1) %>%
  filter(electiondate >= as.Date("1994-01-01"), !is.na(electiondate_l1))

leads <- c(2L, seq(5L, 90L, by = 5L))
message("Precomputing latent series for ", nrow(elec), " elections...")

cache <- list()
n_ok <- 0L
for (i in seq_len(nrow(elec))) {
  land <- elec$land[i]
  ed <- elec$electiondate[i]
  l1 <- elec$electiondate_l1[i]
  key <- paste(land, ed, sep = "_")
  parties <- res %>% filter(land == !!land, electiondate == ed) %>% pull(party)
  series_by_party <- list()
  for (p in parties) {
    ser <- tryCatch(latent_series(polls, p, land, l1, ed), error = function(e) NULL)
    if (!is.null(ser)) series_by_party[[p]] <- ser
  }
  if (length(series_by_party) > 0) {
    cache[[key]] <- list(
      land = land, ed = ed, l1 = l1, series = series_by_party,
      results = res %>% filter(land == !!land, electiondate == ed)
    )
    n_ok <- n_ok + 1L
  }
  if (i %% 20 == 0) message("  ", i, "/", nrow(elec), " cached=", n_ok)
}
message("Cached elections with series: ", n_ok)

# Also load production coefs for overlay at 37 and 51
prod <- readRDS(file.path(ROOT, "data/output/model/model_bayes.RDS"))
prod_rows <- list()
for (nm in names(prod$lr)) {
  lead <- as.integer(sub("_.*", "", nm))
  cf <- coef(prod$lr[[nm]])
  a <- unname(cf[1])
  b <- unname(cf[2])
  pred40 <- 1 / (1 + exp(-(a + b * log_ratio(0.40))))
  prod_rows[[length(prod_rows) + 1]] <- tibble(
    lead = lead, alpha = a, beta = b, pred40 = pred40, delta40 = pred40 - 0.40,
    source = "website"
  )
}
prod_coefs <- bind_rows(prod_rows)

coef_rows <- list()
for (L in leads) {
  rows <- list()
  for (key in names(cache)) {
    obj <- cache[[key]]
    asof <- obj$ed - L
    if (asof <= obj$l1) next
    for (p in names(obj$series)) {
      ser <- obj$series[[p]]
      lat <- ser$latent_pct[ser$date == asof]
      if (length(lat) != 1 || !is.finite(lat)) next
      poll <- lat / 100
      vs <- obj$results %>% filter(party == p) %>% pull(voteshare)
      if (length(vs) != 1 || !is.finite(vs) || vs <= 0 || vs >= 1) next
      if (!is.finite(poll) || poll <= 0 || poll >= 1) next
      rows[[length(rows) + 1]] <- tibble(
        lead = L, party = p,
        pollslr = log_ratio(poll),
        votesharelr = log_ratio(vs),
        voteshare = vs
      )
    }
  }
  dat <- bind_rows(rows)
  if (nrow(dat) < 30) next

  message(sprintf("Fitting Bayes lead=%d (n=%d)...", L, nrow(dat)))
  set.seed(SEED)
  mdl <- stan_glm(
    votesharelr ~ pollslr,
    data = dat,
    family = gaussian(),
    prior = normal(0, PRIOR_SCALE),
    prior_intercept = normal(0, PRIOR_SCALE),
    prior_aux = exponential(rate = PRIOR_AUX_RATE),
    chains = 2, iter = 2000,
    seed = SEED,
    refresh = 0
  )
  cf <- coef(mdl)
  a <- unname(cf[1])
  b <- unname(cf[2])
  pred40 <- 1 / (1 + exp(-(a + b * log_ratio(0.40))))
  # in-sample MAE on probability scale via posterior median linear predictor
  fit_lr <- a + b * dat$pollslr
  fit_p <- 1 / (1 + exp(-fit_lr))
  coef_rows[[length(coef_rows) + 1]] <- tibble(
    lead = L,
    n = nrow(dat),
    alpha = a,
    beta = b,
    pred40 = as.numeric(pred40),
    delta40 = as.numeric(pred40 - 0.40),
    mae = mean(abs(100 * (fit_p - dat$voteshare))),
    source = "bayes_grid"
  )
  message(sprintf(
    "  lead=%2d  α=%7.3f  β=%6.3f  40→%.1f (%+.1f)",
    L, a, b, 100 * pred40, 100 * (pred40 - 0.40)
  ))
}

coefs <- bind_rows(coef_rows)
out_dir <- file.path(ROOT, "results/figures/model")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(coefs, file.path(out_dir, "coefs_by_lead_bayes.csv"))

# Copy destination in website-pipeline workspace if present
ws_tmp <- "/mnt/cerfort/forecasts/website-pipeline/tmp/coefs_by_lead"
dir.create(ws_tmp, recursive = TRUE, showWarnings = FALSE)

theme_plot <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

mk <- function(y, ylab, hline, title, sub) {
  ggplot(coefs, aes(x = lead)) +
    geom_vline(xintercept = c(37, 51), colour = "#c0392b", linetype = 2, alpha = 0.75) +
    geom_hline(yintercept = hline, colour = "grey80") +
    geom_line(aes(y = {{ y }}), colour = "#1f4e79", linewidth = 1) +
    geom_point(aes(y = {{ y }}), colour = "#1f4e79", size = 2.3) +
    geom_point(
      data = prod_coefs, aes(x = lead, y = {{ y }}),
      colour = "#c0392b", size = 3.5, shape = 18
    ) +
    labs(title = title, subtitle = sub, x = "Lead (days)", y = ylab) +
    theme_plot
}

p1 <- mk(
  alpha, "α", 0,
  "Intercept α by lead (website-analog Bayes)",
  "stan_glm polls-only log-ratio; prior N(0,1); red diamonds = live 37/51_polls"
)
p2 <- mk(
  beta, "β", 1,
  "Slope β by lead (website-analog Bayes)",
  "Same priors as production; red diamonds = live model_bayes.RDS"
)
p3 <- mk(
  100 * delta40, "Δ pp", 0,
  "Δ at poll = 40% (website-analog Bayes)",
  "pred − 40pp before normalize; dashed = live leads"
)

save_png <- function(path, plot, w = 1000, h = 480) {
  png(path, width = w, height = h, res = 140)
  print(plot)
  dev.off()
}

save_png(file.path(out_dir, "coefs_by_lead_bayes_alpha.png"), p1)
save_png(file.path(out_dir, "coefs_by_lead_bayes_beta.png"), p2)
save_png(file.path(out_dir, "coefs_by_lead_bayes_delta40.png"), p3)
if (requireNamespace("patchwork", quietly = TRUE)) {
  save_png(file.path(out_dir, "coefs_by_lead_bayes.png"), p1 / p2 / p3, w = 1000, h = 1200)
}

file.copy(
  list.files(out_dir, pattern = "coefs_by_lead_bayes", full.names = TRUE),
  ws_tmp,
  overwrite = TRUE
)
# also write csv to tmp
file.copy(file.path(out_dir, "coefs_by_lead_bayes.csv"), ws_tmp, overwrite = TRUE)

message("Wrote Bayes-by-lead plots to ", out_dir, " and ", ws_tmp)
print(coefs %>% mutate(across(c(alpha, beta, pred40, delta40, mae), ~ round(., 4))), n = 40)
print(prod_coefs %>% mutate(across(c(alpha, beta, pred40, delta40), ~ round(., 4))))
