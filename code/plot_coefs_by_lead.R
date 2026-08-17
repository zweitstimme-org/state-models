# Plot polls-only log-ratio OLS coefficients by lead (5-day steps).
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(dlm)
  library(tidyr)
  library(ggplot2)
})

ROOT <- if (basename(getwd()) == "code") normalizePath("..") else normalizePath(".")
source(file.path(ROOT, "code", "auxilary", "functions.R"))

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
      rows[[length(rows) + 1]] <- tibble(lead = L, party = p, poll = poll, voteshare = vs)
    }
  }
  dat <- bind_rows(rows)
  if (nrow(dat) < 30) next
  fit <- lm(log_ratio(voteshare) ~ log_ratio(poll), data = dat)
  cf <- coef(fit)
  pred40 <- 1 / (1 + exp(-(cf[1] + cf[2] * log_ratio(0.40))))
  coef_rows[[length(coef_rows) + 1]] <- tibble(
    lead = L,
    n = nrow(dat),
    alpha = unname(cf[1]),
    beta = unname(cf[2]),
    pred40 = as.numeric(pred40),
    delta40 = as.numeric(pred40 - 0.40),
    mae = mean(abs(100 * (1 / (1 + exp(-fitted(fit))) - dat$voteshare)))
  )
  message(sprintf(
    "lead=%2d  n=%3d  α=%7.3f  β=%6.3f  40→%.1f (%+.1f)",
    L, nrow(dat), cf[1], cf[2], 100 * pred40, 100 * (pred40 - 0.40)
  ))
}

coefs <- bind_rows(coef_rows) %>% mutate(live = lead %in% c(37L, 51L))
out_dir <- file.path(ROOT, "results/figures/model")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(coefs, file.path(out_dir, "coefs_by_lead.csv"))

theme_plot <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

p1 <- ggplot(coefs, aes(x = lead)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_line(aes(y = alpha), colour = "#1f4e79", size = 1) +
  geom_point(aes(y = alpha, colour = live), size = 2.5) +
  scale_colour_manual(values = c("FALSE" = "#1f4e79", "TRUE" = "#c0392b"), guide = "none") +
  labs(
    title = "Polls-only Log-Ratio: Intercept α by lead",
    subtitle = "OLS on historical Landtagswahlen; red ≈ live leads (37 / 51)",
    x = "Days until election (lead)", y = "α (logit scale)"
  ) +
  theme_plot

p2 <- ggplot(coefs, aes(x = lead)) +
  geom_hline(yintercept = 1, colour = "grey80") +
  geom_line(aes(y = beta), colour = "#1f4e79", size = 1) +
  geom_point(aes(y = beta, colour = live), size = 2.5) +
  scale_colour_manual(values = c("FALSE" = "#1f4e79", "TRUE" = "#c0392b"), guide = "none") +
  labs(
    title = "Polls-only Log-Ratio: Slope β by lead",
    subtitle = "β → 1 closer to election (less attenuation)",
    x = "Days until election (lead)", y = "β"
  ) +
  theme_plot

p3 <- ggplot(coefs, aes(x = lead)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_line(aes(y = 100 * delta40), colour = "#1f4e79", size = 1) +
  geom_point(aes(y = 100 * delta40, colour = live), size = 2.5) +
  scale_colour_manual(values = c("FALSE" = "#1f4e79", "TRUE" = "#c0392b"), guide = "none") +
  labs(
    title = "Implied adjustment at poll = 40%",
    subtitle = "pred − 40pp after invlogit(α + β·logit(0.4)); before normalize",
    x = "Days until election (lead)", y = "Δ percentage points"
  ) +
  theme_plot

png(file.path(out_dir, "coefs_by_lead_alpha.png"), width = 1000, height = 480, res = 140)
print(p1)
dev.off()
png(file.path(out_dir, "coefs_by_lead_beta.png"), width = 1000, height = 480, res = 140)
print(p2)
dev.off()
png(file.path(out_dir, "coefs_by_lead_delta40.png"), width = 1000, height = 480, res = 140)
print(p3)
dev.off()

if (requireNamespace("patchwork", quietly = TRUE)) {
  png(file.path(out_dir, "coefs_by_lead.png"), width = 1000, height = 1200, res = 140)
  print(p1 / p2 / p3)
  dev.off()
} else if (requireNamespace("gridExtra", quietly = TRUE)) {
  png(file.path(out_dir, "coefs_by_lead.png"), width = 1000, height = 1200, res = 140)
  gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
  dev.off()
}

message("Wrote figures to ", out_dir)
print(coefs %>% mutate(across(c(alpha, beta, pred40, delta40, mae), ~ round(., 4))), n = 40)
