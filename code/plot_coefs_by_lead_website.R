# Coefs-by-lead plot using website-analog sample + Bayes priors.
# Panel = production full_data rows (same elections/parties + oth).
# Latents are CAUSAL (polls strictly before asof), matching get_latent_support_land.
# At live leads 37/51 use stored polls_* so the curve hits the live diamonds.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(rstanarm)
  library(patchwork)
})

ROOT <- if (basename(getwd()) == "code") normalizePath("..") else normalizePath(".")
source(file.path(ROOT, "code", "auxilary", "functions.R"))
options(mc.cores = parallel::detectCores())

PRIOR_SCALE <- 1
PRIOR_AUX_RATE <- 2
SEED <- 238523862L

log_ratio <- function(x, c = 0.01) {
  x <- pmin(pmax(as.numeric(x), c), 1 - c)
  log(x / (1 - x))
}

# Causal latent at asof: only polls with date < asof (same as production).
latent_at_asof <- function(polls, party, land, asof, last_election_date) {
  lat <- tryCatch(
    get_latent_support_land(
      polls, party, land, asof, last_election_date,
      "party", "date", "poll_share", "land"
    ),
    error = function(e) NA_real_
  )
  if (!is.finite(lat)) return(NA_real_)
  if (lat > 1) lat <- lat / 100
  if (!is.finite(lat) || lat <= 0 || lat >= 1) return(NA_real_)
  lat
}

load(file.path(ROOT, "data/output/05_full_data.RData"))
polls <- read_csv(file.path(ROOT, "data/output/01_state-polls.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
res <- read_csv(file.path(ROOT, "data/input/01_state-election-results.csv"), show_col_types = FALSE) %>%
  mutate(electiondate = as.Date(electiondate), electiondate_l1 = as.Date(electiondate_l1)) %>%
  distinct(land, electiondate, electiondate_l1)

# Website training universe: rows that qualify for polls_37 model (includes oth)
base <- full_data %>%
  filter(
    pollsNA_37 == 0,
    !is.na(voteshare), !is.na(polls_37),
    voteshare > 0, voteshare < 1,
    polls_37 > 0, polls_37 < 1
  ) %>%
  mutate(electiondate = as.Date(electiondate)) %>%
  left_join(res %>% rename(state = land), by = c("state", "electiondate")) %>%
  filter(!is.na(electiondate_l1))

message("Website-analog panel: n=", nrow(base), " elections=", n_distinct(base$elec_ind))

leads <- sort(unique(c(2L, seq(5L, 90L, by = 5L), 37L, 51L)))

prod <- readRDS(file.path(ROOT, "data/output/model/model_bayes.RDS"))
prod_coefs <- bind_rows(lapply(names(prod$lr), function(nm) {
  lead <- as.integer(sub("_.*", "", nm))
  cf <- coef(prod$lr[[nm]])
  a <- unname(cf[1]); b <- unname(cf[2])
  pred40 <- 1 / (1 + exp(-(a + b * log_ratio(0.40))))
  tibble(lead = lead, alpha = a, beta = b, pred40 = pred40, delta40 = pred40 - 0.40, source = "website")
}))

# Cache causal latents: key = state|party|election|asof
lat_cache <- new.env(parent = emptyenv())
get_poll <- function(r, L) {
  pcol <- paste0("polls_", L)
  # Exact live training X at stored leads
  if (pcol %in% names(r) && is.finite(r[[pcol]]) && r[[pcol]] > 0 && r[[pcol]] < 1) {
    nacol <- paste0("pollsNA_", L)
    if (!nacol %in% names(r) || isTRUE(r[[nacol]] == 0)) {
      return(as.numeric(r[[pcol]]))
    }
  }
  asof <- as.Date(r$electiondate) - as.integer(L)
  if (asof <= as.Date(r$electiondate_l1)) return(NA_real_)
  key <- paste(r$state, r$party, r$electiondate, asof, sep = "|")
  if (exists(key, envir = lat_cache, inherits = FALSE)) {
    return(get(key, envir = lat_cache))
  }
  lat <- latent_at_asof(polls, r$party, r$state, asof, r$electiondate_l1)
  assign(key, lat, envir = lat_cache)
  lat
}

coef_rows <- list()
for (L in leads) {
  rows <- list()
  for (i in seq_len(nrow(base))) {
    r <- base[i, ]
    poll <- get_poll(r, L)
    if (!is.finite(poll) || poll <= 0 || poll >= 1) next
    rows[[length(rows) + 1]] <- tibble(
      pollslr = log_ratio(poll),
      votesharelr = r$votesharelr,
      voteshare = r$voteshare,
      polls = poll
    )
  }
  dat <- bind_rows(rows)
  if (nrow(dat) < 30) {
    message("lead=", L, " skipped (n=", nrow(dat), ")")
    next
  }

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
  a <- unname(cf[1]); b <- unname(cf[2])
  pred40 <- 1 / (1 + exp(-(a + b * log_ratio(0.40))))
  fit_p <- 1 / (1 + exp(-(a + b * dat$pollslr)))
  coef_rows[[length(coef_rows) + 1]] <- tibble(
    lead = L, n = nrow(dat), alpha = a, beta = b,
    pred40 = as.numeric(pred40), delta40 = as.numeric(pred40 - 0.40),
    mae = mean(abs(100 * (fit_p - dat$voteshare))),
    source = "bayes_website_panel_causal"
  )
  message(sprintf(
    "  lead=%2d  α=%7.3f  β=%6.3f  40→%.1f (%+.1f)",
    L, a, b, 100 * pred40, 100 * (pred40 - 0.40)
  ))
}

coefs <- bind_rows(coef_rows)
out_dir <- file.path(ROOT, "results/figures/model")
ws_tmp <- "/mnt/cerfort/forecasts/website-pipeline/tmp/coefs_by_lead"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ws_tmp, recursive = TRUE, showWarnings = FALSE)
write_csv(coefs, file.path(out_dir, "coefs_by_lead_website_analog.csv"))
write_csv(coefs, file.path(ws_tmp, "coefs_by_lead_website_analog.csv"))

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
      colour = "#c0392b", size = 3.8, shape = 18
    ) +
    labs(title = title, subtitle = sub, x = "Lead (days)", y = ylab) +
    theme_plot
}

p1 <- mk(
  alpha, "α", 0,
  "Intercept α by lead",
  "Website panel + causal DLM (no lookahead); ◆ = live model; 37/51 use stored polls_*"
)
p2 <- mk(
  beta, "β", 1,
  "Slope β by lead",
  "Causal latents; at 37/51 curve uses stored polls so β matches live (~0.93)"
)
p3 <- mk(
  100 * delta40, "Δ pp", 0,
  "Δ at poll = 40%",
  "pred − 40pp before normalize"
)

save_png <- function(path, plot, w = 1000, h = 480) {
  png(path, width = w, height = h, res = 140)
  print(plot)
  dev.off()
  file.copy(path, ws_tmp, overwrite = TRUE)
}

save_png(file.path(out_dir, "coefs_by_lead_website_alpha.png"), p1)
save_png(file.path(out_dir, "coefs_by_lead_website_beta.png"), p2)
save_png(file.path(out_dir, "coefs_by_lead_website_delta40.png"), p3)

png(file.path(out_dir, "coefs_by_lead.png"), width = 1000, height = 1200, res = 140)
print(p1 / p2 / p3)
dev.off()
file.copy(file.path(out_dir, "coefs_by_lead.png"), ws_tmp, overwrite = TRUE)
file.copy(file.path(out_dir, "coefs_by_lead.png"), file.path(ws_tmp, "coefs_by_lead_website.png"), overwrite = TRUE)
# Also refresh plain beta name user has open
file.copy(file.path(out_dir, "coefs_by_lead_website_beta.png"),
          file.path(ws_tmp, "coefs_by_lead_beta.png"), overwrite = TRUE)
file.copy(file.path(out_dir, "coefs_by_lead_website_alpha.png"),
          file.path(ws_tmp, "coefs_by_lead_alpha.png"), overwrite = TRUE)

message("Wrote to ", ws_tmp)
print(coefs %>% mutate(across(c(alpha, beta, pred40, delta40, mae), ~ round(., 4))), n = 40)
print(prod_coefs %>% mutate(across(where(is.numeric), ~ round(., 4))))
