###########################
# Build data pipeline: state polls, state leads, federal polls (API), federal leads, full data
# Run from project root. Input: data/input/ (numbered CSVs). Output: data/output/ (numbered).
# State and federal polls from same API (POLLING_API_BASE); federal leads and fed_trend_* use full API federal polls.
###########################

# Project root and paths (run from repo root)
if (basename(getwd()) %in% c("code", "data")) setwd("..")
ROOT <- getwd()

source(file.path(ROOT, "code", "auxilary", "packages.R"))
ROOT <- getwd()  # re-set after packages.R does rm(list=ls())
source(file.path(ROOT, "code", "auxilary", "functions.R"))

# Paths (set after source: packages.R clears workspace)
DATA_IN  <- file.path(ROOT, "data", "input")
DATA_OUT <- file.path(ROOT, "data", "output")
dir.create(DATA_OUT, showWarnings = FALSE, recursive = TRUE)

# Main parties only: CDU/CSU, SPD, Grüne, FDP, AfD, Linke, BSW. Sonstige (oth) = 100% - sum(main).
# FW (Freie Wähler) excluded to keep training/forecast simple; API fw rows are dropped via PARTIES_KEEP / PARTIES_FED.
MAIN_PARTIES <- c("cdu", "spd", "gru", "fdp", "afd", "lin", "bsw")
PARTIES_FED <- c(MAIN_PARTIES, "oth")

message("=== 1. State polls (API v2) ===")
###########################
# 1. State polls from FastTrack polling API v2
###########################
API_BASE <- Sys.getenv("POLLING_API_BASE", "https://api.fasttrack29.com")
if (nzchar(API_BASE) && endsWith(API_BASE, "/")) API_BASE <- sub("/+$", "", API_BASE)
message("  API base: ", API_BASE, " (v2/polls)")
POLLS_LIMIT <- 500L

# v2 scopes are already 2-letter land codes (plus nrw).
STATE_SCOPES <- c(
  "bw", "by", "be", "bb", "hb", "hh", "he", "mv", "ni", "nrw",
  "rp", "sl", "sn", "st", "sh", "th"
)
scope_to_land <- setNames(STATE_SCOPES, STATE_SCOPES)
scope_to_land[["nrw"]] <- "nw"

PARTIES_KEEP <- MAIN_PARTIES  # only main parties in polls; oth computed as 100% - sum(main)
# BSW poll_share is NA before this date; we only impute for BSW on/after this date (state and federal)
BSW_POLL_START_DATE <- as.Date("2024-01-01")
# Institutes (e.g. Infratest dimap) often only list a party from ~3% upward; below that
# it is folded into Sonstige. For latent-support DLMs we briefly hold unreported
# forecast parties at MISSING_IMPUTE_PCT after their last report in-scope — not forever,
# so a party that truly disappears can fall out. (Website Stimmung leaves NA instead.)
MISSING_IMPUTE_PCT <- 2
MISSING_HOLD_DAYS <- 90L
PARTIES_IMPUTE_IF_MISSING <- c("cdu", "spd", "fdp", "gru", "lin", "afd", "bsw")

#' Impute unreported main-party shares for a limited hold window.
#' @param df polls with date, party, poll_share (+ scope_cols, e.g. land)
#' @param scope_cols columns defining the geography (character()); empty = federal
#' @param poll_cols columns defining one poll release (e.g. date+institut)
impute_unreported_parties <- function(df,
                                      scope_cols,
                                      poll_cols,
                                      parties = PARTIES_IMPUTE_IF_MISSING,
                                      impute_pct = MISSING_IMPUTE_PCT,
                                      hold_days = MISSING_HOLD_DAYS,
                                      bsw_start = BSW_POLL_START_DATE) {
  df <- df %>%
    group_by(across(all_of(poll_cols))) %>%
    mutate(poll_has_data = any(!is.na(poll_share))) %>%
    ungroup()

  # Last date this party was actually reported in-scope (any institute), carried forward.
  group_vars <- c(scope_cols, "party")
  df <- df %>%
    group_by(across(all_of(group_vars))) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      .obs_date = if_else(!is.na(poll_share), date, as.Date(NA)),
      .last_seen = {
        # na.locf without zoo dependency
        x <- .obs_date
        last <- as.Date(NA)
        out <- as.Date(rep(NA, length(x)))
        for (i in seq_along(x)) {
          if (!is.na(x[i])) last <- x[i]
          out[i] <- last
        }
        out
      }
    ) %>%
    ungroup()

  df %>%
    mutate(
      .eligible = party %in% parties & (party != "bsw" | date >= bsw_start),
      .within_hold = !is.na(.last_seen) & (as.integer(date - .last_seen) <= hold_days),
      # Never reported yet in this scope (e.g. BSW just after debut): impute until first real poll.
      .never_seen = is.na(.last_seen) & .eligible,
      poll_share = if_else(
        poll_has_data & is.na(poll_share) & .eligible & (.within_hold | .never_seen),
        impute_pct,
        poll_share
      )
    ) %>%
    select(-poll_has_data, -.obs_date, -.last_seen, -.eligible, -.within_hold, -.never_seen)
}

# FastTrack v2 party_key → model code
party_key_to_code <- c(
  "CDU_CSU" = "cdu", "CDU" = "cdu", "CSU" = "cdu",
  "SPD" = "spd", "FDP" = "fdp",
  "GRUENE" = "gru", "GRUNE" = "gru", "GRÜNE" = "gru",
  "LINKE" = "lin", "AFD" = "afd", "BSW" = "bsw",
  "SONSTIGE" = "oth", "OTH" = "oth"
)

cache_bust <- as.character(as.numeric(Sys.time()))
api_headers <- httr::add_headers(
  "Cache-Control" = "no-cache, no-store, must-revalidate",
  "Pragma" = "no-cache",
  "User-Agent" = paste0("state-models/1 (", cache_bust, ")")
)

fetch_v2_polls <- function(scope, api_base = API_BASE, limit = POLLS_LIMIT, headers = api_headers) {
  out <- list()
  offset <- 0L
  repeat {
    url <- httr::modify_url(
      file.path(api_base, "v2", "polls"),
      query = list(
        scope = scope,
        include_results = "true",
        limit = limit,
        offset = offset,
        sort = "-published_date",
        "_t" = cache_bust
      )
    )
    resp <- GET(url, headers, httr::timeout(120))
    stopifnot(httr::status_code(resp) == 200)
    data <- httr::content(resp, as = "parsed")
    items <- data$data
    if (is.null(items) || length(items) == 0) break
    out <- c(out, items)
    pag <- data$pagination
    has_next <- isTRUE(pag$has_next) || (!is.null(pag$total) && (offset + length(items) < pag$total))
    if (!has_next || length(items) < limit) break
    offset <- offset + length(items)
  }
  # Optional: merge DAWUM Landtag scrapes from website-pipeline (clear Parliament_ID
  # mapping + QC/dedup) while FastTrack mis-scopes those rows as federal.
  wp <- Sys.getenv("WEBSITE_PIPELINE_ROOT", "")
  fetch_r <- file.path(wp, "R", "fetch_polls.R")
  if (nzchar(wp) && file.exists(fetch_r)) {
    if (!exists("inject_dawum_state_polls", mode = "function")) {
      cfg <- file.path(wp, "R", "config.R")
      if (file.exists(cfg)) source(cfg, local = FALSE)
      source(fetch_r, local = FALSE)
    }
    before <- length(out)
    out <- inject_dawum_state_polls(out, scope)
    if (length(out) > before) {
      message(sprintf(
        "  DAWUM inject scope=%s: +%d poll(s)",
        scope, length(out) - before
      ))
    }
  }
  out
}

map_party_key <- function(r, keep = PARTIES_KEEP) {
  key <- r$party_key %||% r$party_slug %||% r$party_code %||% r$party_short_name %||% ""
  key <- trimws(as.character(key))
  if (!nzchar(key)) return(NA_character_)
  norm <- toupper(gsub("[^A-Za-zÄÖÜäöü_]", "", key))
  code <- if (norm %in% names(party_key_to_code)) unname(party_key_to_code[[norm]]) else NA_character_
  if (is.na(code)) {
    slug <- trimws(tolower(key))
    if (slug %in% keep) code <- slug
  }
  if (is.na(code) || !code %in% keep) return(NA_character_)
  code
}

all_polls <- list()
for (scope in STATE_SCOPES) {
  all_polls <- c(all_polls, fetch_v2_polls(scope))
}

raw_list <- list()
for (p in all_polls) {
  scope <- p$scope
  if (is.null(scope)) next
  scope_norm <- trimws(tolower(as.character(scope)))
  if (!scope_norm %in% names(scope_to_land)) next
  land <- unname(scope_to_land[[scope_norm]])
  date <- p$published_date %||% p$publish_date
  if (is.null(date) || !nzchar(as.character(date))) next
  inst <- if (is.null(p$institute_name)) NA_character_ else p$institute_name
  if (identical(inst, "various") && !is.null(p$provider_name)) inst <- p$provider_name
  if (is.null(p$results) || length(p$results) == 0) next
  for (r in p$results) {
    pct <- r$percentage
    if (is.null(pct)) next
    party <- map_party_key(r, PARTIES_KEEP)
    if (is.na(party)) next
    raw_list[[length(raw_list) + 1]] <- list(
      date = as.character(date), land = land, institut = inst,
      party = party, poll_share = as.numeric(pct)
    )
  }
}
if (length(raw_list) == 0) stop("No state polls returned from API v2.")

state_polls <- bind_rows(lapply(raw_list, as.data.frame)) %>% select(date, institut, land, party, poll_share)
state_polls$date <- as.Date(state_polls$date)
# Prefer API values over NA when same (date, land, institut, party) appears multiple times
state_polls <- state_polls %>%
  group_by(date, land, institut, party) %>%
  summarise(poll_share = if (any(!is.na(poll_share))) first(poll_share[!is.na(poll_share)]) else NA_real_, .groups = "drop") %>%
  ungroup()
state_polls <- state_polls %>%
  tidyr::complete(tidyr::nesting(date, land, institut), party = PARTIES_KEEP, fill = list(poll_share = NA_real_)) %>%
  select(date, institut, land, party, poll_share) %>% arrange(date, land, party)

# Unreported parties: hold at MISSING_IMPUTE_PCT for MISSING_HOLD_DAYS after last in-land report
# (institutes often omit parties below ~3%). Not permanent — see impute_unreported_parties().
state_polls <- impute_unreported_parties(
  state_polls,
  scope_cols = "land",
  poll_cols = c("date", "land", "institut")
)

# Add oth (Sonstige) = 100% - sum(main parties only) per poll
state_polls_oth <- state_polls %>%
  group_by(date, land, institut) %>%
  summarise(poll_share = if (any(!is.na(poll_share))) 100 - sum(poll_share, na.rm = TRUE) else NA_real_, .groups = "drop") %>%
  mutate(party = "oth")
state_polls <- bind_rows(state_polls, state_polls_oth) %>% arrange(date, land, party)
# BSW poll_share is NA before BSW_POLL_START_DATE in the written CSV
state_polls <- state_polls %>%
  mutate(poll_share = if_else(party == "bsw" & date < BSW_POLL_START_DATE, NA_real_, poll_share))

all_lands <- sort(unique(unname(scope_to_land)))
polls_per_scope <- state_polls %>% group_by(land) %>% summarise(first_poll = min(date, na.rm = TRUE), latest_poll = max(date, na.rm = TRUE), .groups = "drop")
full_scope_latest <- tibble(land = all_lands) %>% left_join(polls_per_scope, by = "land") %>%
  mutate(first_label = if_else(is.na(first_poll), "no data", as.character(first_poll)), latest_label = if_else(is.na(latest_poll), "no data", as.character(latest_poll)))
scope_lines <- c("  federal: not in state-polls (Bundestag only)", paste0("  ", full_scope_latest$land, ": ", full_scope_latest$first_label, " .. ", full_scope_latest$latest_label, collapse = "\n"))
n_polls_total <- state_polls %>% distinct(date, land, institut) %>% nrow()
n_oth <- sum(state_polls$party == "oth", na.rm = TRUE)
latest_5_dates <- state_polls %>% distinct(date, land, institut) %>% arrange(desc(date)) %>% head(5) %>% pull(date)
message("  5 latest poll dates (state): ", paste(as.character(latest_5_dates), collapse = ", "))
log_lines <- c(
  "=== State polls diagnostic ===",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  "",
  "API base: ", paste0("  ", API_BASE),
  "",
  "Missing-party impute: ",
  paste0("  hold ", MISSING_IMPUTE_PCT, "% for ", MISSING_HOLD_DAYS,
         " days after last in-land report (institutes often omit below ~3%); not permanent"),
  "",
  "5 latest poll dates (state): ", paste0("  ", paste(as.character(latest_5_dates), collapse = ", ")),
  "",
  "State polls include party 'oth' (Sonstige = 100% - sum(others)) per poll; oth rows: ", paste0("  ", n_oth),
  "",
  "First and latest poll per scope (land):", scope_lines,
  "",
  "Total number of polls (distinct date × land × institut):", paste0("  ", n_polls_total),
  ""
)

writeLines(log_lines, file.path(DATA_OUT, "01_state-polls-diagnostics.txt"))
write.csv(state_polls, file.path(DATA_OUT, "01_state-polls.csv"), row.names = FALSE)
message("  Written 01_state-polls.csv (", nrow(state_polls), " rows)")


message("=== 2. State leads ===")
###########################
# 2. State leads (lead = days until election; set from forecast elections)
###########################
# Lead days come from run_everything/run_pipeline (e.g. 8 and 23 if elections are in 8 and 23 days)
lead_days_path <- file.path(DATA_OUT, "lead_days.RData")
if (file.exists(lead_days_path)) {
  load(lead_days_path)
  LEAD_DAYS <- sort(unique(as.integer(lead_days)))
  message("  Lead days (from forecast elections): ", paste(LEAD_DAYS, collapse = ", "))
} else {
  stop("lead_days.RData not found. Run from run_everything.R or run_pipeline.R so lead days are set from forecast elections.")
}
state_results <- read.csv(file.path(DATA_IN, "01_state-election-results.csv"))
state_results$electiondate <- ymd(state_results$electiondate)
state_results$electiondate_l1 <- ymd(state_results$electiondate_l1)
# Keep only main parties; Sonstige = 100% - sum(main) per (land, electiondate)
state_results <- state_results %>% filter(party %in% MAIN_PARTIES)
state_results_oth <- state_results %>%
  group_by(land, electiondate) %>%
  summarise(vote_share = 100 - sum(vote_share, na.rm = TRUE), vote_share_l1 = 100 - sum(vote_share_l1, na.rm = TRUE), .groups = "drop") %>%
  mutate(party = "oth")
if (nrow(state_results_oth) > 0L) {
  state_results_oth <- state_results_oth %>%
    left_join(state_results %>% group_by(land, electiondate) %>% slice(1L) %>% ungroup() %>% select(land, electiondate, any_of(setdiff(names(state_results), c("party", "vote_share", "vote_share_l1")))), by = c("land", "electiondate")) %>%
    select(names(state_results))
  state_results <- bind_rows(state_results, state_results_oth)
}
state_polls$date <- ymd(state_polls$date)

state_leads <- state_results %>% select(c(land, party, electiondate, electiondate_l1, vote_share, vote_share_l1)) %>% unique

# Add state leads for all forecast elections (electiondate_l1 and vote_share_l1 from last state election in that land)
elections_to_forecast_path <- file.path(DATA_OUT, "elections_to_forecast.RData")
PARTIES_STATE <- c(MAIN_PARTIES, "oth")
if (file.exists(elections_to_forecast_path)) {
  load(elections_to_forecast_path)
  forecast_elecs <- data.frame(
    land = sub("_.*", "", elections_to_forecast),
    electiondate = as.Date(sub(".*_", "", elections_to_forecast)),
    stringsAsFactors = FALSE
  )
  if (nrow(forecast_elecs) > 0L) {
    get_l1_state <- function(land, ed) {
      x <- state_results$electiondate[state_results$land == land & state_results$electiondate < ed]
      if (length(x)) max(x) else as.Date(NA)
    }
    forecast_state_l1 <- forecast_elecs %>%
      rowwise() %>%
      mutate(electiondate_l1 = get_l1_state(land, electiondate)) %>%
      ungroup() %>%
      filter(!is.na(electiondate_l1))
    if (nrow(forecast_state_l1) > 0L) {
      forecast_state_leads <- forecast_state_l1 %>%
        tidyr::crossing(party = PARTIES_STATE) %>%
        left_join(
          state_results %>% select(land, party, electiondate_l1 = electiondate, vote_share_l1 = vote_share),
          by = c("land", "party", "electiondate_l1")
        ) %>%
        mutate(vote_share = NA_real_) %>%
        select(land, party, electiondate, electiondate_l1, vote_share, vote_share_l1)
      state_leads <- bind_rows(state_leads, forecast_state_leads)
      message("  Added ", nrow(forecast_state_leads), " state-lead rows for ", nrow(forecast_state_l1), " forecast election(s)")
    }
  }
}

for (ld in LEAD_DAYS) {
  state_leads[[paste0("date_", ld)]] <- state_leads$electiondate - days(ld)
}
state_leads <- filter(state_leads, .data[[paste0("date_", max(LEAD_DAYS))]] >= min(state_polls$date, na.rm = TRUE))
for (ld in LEAD_DAYS) {
  state_leads[[paste0("polls_", ld)]] <- NA
}
state_leads$calculated <- FALSE
for (i in 1:nrow(state_leads)) {
  if (state_leads$calculated[i]) next
  for (ld in LEAD_DAYS) {
    state_leads[[paste0("polls_", ld)]][i] <- get_latent_support_land(state_polls, state_leads$party[i], state_leads$land[i], state_leads[[paste0("date_", ld)]][i], state_leads$electiondate_l1[i], "party", "date", "poll_share", "land")
  }
  state_leads$calculated[i] <- TRUE
}
state_leads <- state_leads %>% select(-calculated, -starts_with("date_"))
write.csv(state_leads, file.path(DATA_OUT, "02_state-leads.csv"), row.names = FALSE)
message("  Written 02_state-leads.csv (", nrow(state_leads), " rows)")


message("=== 3. Federal polls (API v2) ===")
###########################
# 3. Federal polls from FastTrack v2 (scope=federal)
###########################
federal_results <- read.csv(file.path(DATA_IN, "03_federal-election-results.csv"))
# Keep only main parties; Sonstige = 100% - sum(main) per electiondate
federal_results <- federal_results %>% filter(party %in% MAIN_PARTIES)
federal_results_oth <- federal_results %>%
  group_by(electiondate) %>%
  summarise(
    vote_share = 100 - sum(vote_share, na.rm = TRUE),
    vote_share_l1 = if ("vote_share_l1" %in% names(federal_results)) 100 - sum(vote_share_l1, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(party = "oth")
if (nrow(federal_results_oth) > 0L) {
  federal_results_oth <- federal_results_oth %>%
    left_join(federal_results %>% group_by(electiondate) %>% slice(1L) %>% ungroup() %>% select(electiondate, any_of(setdiff(names(federal_results), c("party", "vote_share", "vote_share_l1")))), by = "electiondate") %>%
    select(names(federal_results))
  federal_results <- bind_rows(federal_results, federal_results_oth)
}
fed_polls_raw <- list()
fed_items <- fetch_v2_polls("federal")
for (p in fed_items) {
  date <- p$published_date %||% p$publish_date
  if (is.null(date) || !nzchar(as.character(date))) next
  inst <- if (is.null(p$institute_name)) NA_character_ else p$institute_name
  if (is.null(p$results) || length(p$results) == 0) next
  for (r in p$results) {
    pct <- r$percentage
    if (is.null(pct)) next
    # Federal: keep main parties only; oth is residual below.
    party <- map_party_key(r, MAIN_PARTIES)
    if (is.na(party)) next
    fed_polls_raw[[length(fed_polls_raw) + 1]] <- list(
      auftraggeber = inst, date = as.character(date),
      party = party, poll_share = as.numeric(pct)
    )
  }
}
if (length(fed_polls_raw) == 0) stop("No federal polls returned from API v2.")

federal_polls <- bind_rows(lapply(fed_polls_raw, as.data.frame)) %>% dplyr::select(auftraggeber, date, party, poll_share)
federal_polls$date <- lubridate::ymd(federal_polls$date)
federal_polls <- dplyr::filter(federal_polls, !is.na(date))
federal_polls <- unique(federal_polls)

# Complete so each (date, auftraggeber) has all parties; impute unreported mains
# for MISSING_HOLD_DAYS after last federal report (see impute_unreported_parties).
federal_polls <- federal_polls %>%
  tidyr::complete(tidyr::nesting(date, auftraggeber), party = PARTIES_FED, fill = list(poll_share = NA_real_))
federal_polls_main <- federal_polls %>% filter(party %in% MAIN_PARTIES)
federal_polls_main <- impute_unreported_parties(
  federal_polls_main,
  scope_cols = character(),
  poll_cols = c("date", "auftraggeber")
)
federal_polls <- bind_rows(
  federal_polls_main,
  federal_polls %>% filter(party == "oth")
) %>%
  group_by(date, auftraggeber) %>%
  mutate(
    poll_has_data = any(!is.na(poll_share[party %in% MAIN_PARTIES])),
    poll_share = if_else(
      party == "oth" & poll_has_data,
      100 - sum(poll_share[party %in% MAIN_PARTIES], na.rm = TRUE),
      poll_share
    )
  ) %>%
  ungroup() %>%
  select(-poll_has_data)
# BSW poll_share is NA before BSW_POLL_START_DATE in the written CSV
federal_polls <- federal_polls %>%
  mutate(poll_share = if_else(party == "bsw" & date < BSW_POLL_START_DATE, NA_real_, poll_share))

election_dates <- federal_results %>% dplyr::ungroup() %>% dplyr::select(electiondate, electiondate_l1, electiondate_lead1) %>% dplyr::distinct() %>%
  dplyr::mutate(electiondate = as.Date(electiondate), electiondate_l1 = as.Date(electiondate_l1), electiondate_lead1 = as.Date(electiondate_lead1))
find_election <- function(poll_date, ed) {
  poll_date <- as.Date(poll_date)
  idx <- which(ed$electiondate >= poll_date & (is.na(ed$electiondate_l1) | ed$electiondate_l1 < poll_date))
  if (length(idx) == 0) return(NA)
  ed$electiondate[idx[which.max(ed$electiondate[idx])]]
}
federal_polls$electiondate <- vapply(federal_polls$date, find_election, FUN.VALUE = as.Date(NA), ed = election_dates)
federal_polls$electiondate <- as.Date(federal_polls$electiondate, origin = "1970-01-01")
federal_results <- federal_results %>% dplyr::mutate(electiondate = as.Date(electiondate))
federal_polls <- federal_polls %>%
  dplyr::left_join(federal_results %>% dplyr::select(electiondate, party, vote_share, vote_share_l1), by = c("electiondate", "party")) %>%
  dplyr::left_join(election_dates %>% dplyr::select(electiondate, electiondate_l1, electiondate_lead1), by = "electiondate")
# For polls after the last known election (electiondate NA), fill electiondate_l1 and vote_share_l1 from the most recent election
last_election <- federal_results %>%
  dplyr::filter(electiondate == max(as.Date(electiondate), na.rm = TRUE)) %>%
  dplyr::select(party, last_ed = electiondate, last_vs = vote_share) %>%
  dplyr::mutate(last_ed = as.Date(last_ed))
federal_polls <- federal_polls %>%
  dplyr::left_join(last_election, by = "party") %>%
  dplyr::mutate(
    electiondate_l1 = dplyr::if_else(is.na(electiondate), last_ed, electiondate_l1),
    vote_share_l1 = dplyr::if_else(is.na(electiondate), last_vs, vote_share_l1),
    electiondate_lead1 = dplyr::if_else(is.na(electiondate), as.Date(NA), electiondate_lead1)
  ) %>%
  dplyr::select(-last_ed, -last_vs)
federal_polls <- federal_polls[order(federal_polls$date, federal_polls$party, decreasing = TRUE), ]
latest_5_fed <- federal_polls %>% distinct(date, auftraggeber) %>% arrange(desc(date)) %>% head(5) %>% pull(date)
message("  5 latest poll dates (federal): ", paste(as.character(latest_5_fed), collapse = ", "))
write.csv(federal_polls, file.path(DATA_OUT, "03_federal-polls.csv"), row.names = FALSE)
message("  Written 03_federal-polls.csv (", nrow(federal_polls), " rows)")


message("=== 4. Federal leads ===")
###########################
# 4. Federal leads (same lead days 2, 14, 60)
###########################
federal_polls <- read.csv(file.path(DATA_OUT, "03_federal-polls.csv"))
federal_results <- read.csv(file.path(DATA_IN, "03_federal-election-results.csv"))
# Keep only main parties; Sonstige = 100% - sum(main) per electiondate
federal_results <- federal_results %>% filter(party %in% MAIN_PARTIES)
federal_results_oth <- federal_results %>%
  group_by(electiondate) %>%
  summarise(
    vote_share = 100 - sum(vote_share, na.rm = TRUE),
    vote_share_l1 = if ("vote_share_l1" %in% names(federal_results)) 100 - sum(vote_share_l1, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(party = "oth")
if (nrow(federal_results_oth) > 0L) {
  federal_results_oth <- federal_results_oth %>%
    left_join(federal_results %>% group_by(electiondate) %>% slice(1L) %>% ungroup() %>% select(electiondate, any_of(setdiff(names(federal_results), c("party", "vote_share", "vote_share_l1")))), by = "electiondate") %>%
    select(names(federal_results))
  federal_results <- bind_rows(federal_results, federal_results_oth)
}
state_results_fed <- read.csv(file.path(DATA_IN, "01_state-election-results.csv"))
state_results_fed$electiondate <- ymd(state_results_fed$electiondate)
state_results_fed$electiondate_l1 <- ymd(state_results_fed$electiondate_l1)
# Keep only main parties; Sonstige = 100% - sum(main) per (land, electiondate)
state_results_fed <- state_results_fed %>% filter(party %in% MAIN_PARTIES)
state_results_oth_fed <- state_results_fed %>%
  group_by(land, electiondate) %>%
  summarise(vote_share = 100 - sum(vote_share, na.rm = TRUE), vote_share_l1 = 100 - sum(vote_share_l1, na.rm = TRUE), .groups = "drop") %>%
  mutate(party = "oth")
if (nrow(state_results_oth_fed) > 0L) {
  state_results_oth_fed <- state_results_oth_fed %>%
    left_join(state_results_fed %>% group_by(land, electiondate) %>% slice(1L) %>% ungroup() %>% select(land, electiondate, any_of(setdiff(names(state_results_fed), c("party", "vote_share", "vote_share_l1")))), by = c("land", "electiondate")) %>%
    select(names(state_results_fed))
  state_results_fed <- bind_rows(state_results_fed, state_results_oth_fed)
}

federal_leads <- state_results_fed %>% select(c(land, party, electiondate, electiondate_l1)) %>% unique

# Add federal leads for all forecast elections (electiondate_l1 = last state election in that land)
if (file.exists(elections_to_forecast_path)) {
  load(elections_to_forecast_path)
  forecast_elecs_fed <- data.frame(
    land = sub("_.*", "", elections_to_forecast),
    electiondate = as.Date(sub(".*_", "", elections_to_forecast)),
    stringsAsFactors = FALSE
  )
  if (nrow(forecast_elecs_fed) > 0L) {
    get_l1_state_fed <- function(land, ed) {
      x <- state_results_fed$electiondate[state_results_fed$land == land & state_results_fed$electiondate < ed]
      if (length(x)) max(x) else as.Date(NA)
    }
    forecast_fed_l1 <- forecast_elecs_fed %>%
      rowwise() %>%
      mutate(electiondate_l1 = get_l1_state_fed(land, electiondate)) %>%
      ungroup() %>%
      filter(!is.na(electiondate_l1))
    if (nrow(forecast_fed_l1) > 0L) {
      forecast_federal_leads <- forecast_fed_l1 %>%
        tidyr::crossing(party = PARTIES_STATE) %>%
        select(land, party, electiondate, electiondate_l1)
      federal_leads <- bind_rows(federal_leads, forecast_federal_leads)
      message("  Added ", nrow(forecast_federal_leads), " federal-lead rows for ", nrow(forecast_fed_l1), " forecast election(s)")
    }
  }
}

federal_leads$electiondate <- ymd(federal_leads$electiondate)
federal_leads$electiondate_l1 <- ymd(federal_leads$electiondate_l1)
for (ld in LEAD_DAYS) {
  federal_leads[[paste0("date_", ld)]] <- federal_leads$electiondate - days(ld)
}
federal_leads <- filter(federal_leads, .data[[paste0("date_", max(LEAD_DAYS))]] >= min(as.Date(federal_polls$date), na.rm = TRUE))
for (ld in LEAD_DAYS) {
  federal_leads[[paste0("fed_polls_", ld)]] <- NA
}
federal_leads$calculated <- FALSE
federal_polls$date <- as.Date(federal_polls$date)
for (i in 1:nrow(federal_leads)) {
  if (federal_leads$calculated[i]) next
  for (ld in LEAD_DAYS) {
    federal_leads[[paste0("fed_polls_", ld)]][i] <- get_latent_support(federal_polls, federal_leads$party[i], federal_leads[[paste0("date_", ld)]][i], federal_leads$electiondate_l1[i], "party", "date", "poll_share")
  }
  federal_leads$calculated[i] <- TRUE
}
federal_leads <- federal_leads %>% select(-calculated, -starts_with("date_"))
federal_leads$fed_electiondate_l1 <- NA
for (i in 1:nrow(federal_leads)) {
  subset_results <- federal_results %>% filter(as.Date(electiondate) < federal_leads$electiondate[i])
  if (nrow(subset_results) > 0) federal_leads$fed_electiondate_l1[i] <- (subset_results %>% filter(electiondate == max(subset_results$electiondate)))$electiondate %>% unique
}
federal_leads$fed_electiondate_l1 <- as_date(federal_leads$fed_electiondate_l1)
federal_results$electiondate <- as_date(federal_results$electiondate)
federal_leads <- federal_results %>% select(electiondate, party, vote_share) %>%
  dplyr::rename(fed_vote_share = vote_share, fed_electiondate_l1 = electiondate) %>%
  merge(federal_leads, by = c("fed_electiondate_l1", "party"), all.y = TRUE)
write.csv(federal_leads, file.path(DATA_OUT, "04_federal-leads.csv"), row.names = FALSE)
message("  Written 04_federal-leads.csv (", nrow(federal_leads), " rows)")


message("=== 5. Full data ===")
###########################
# 5. Full data (merge cabinets, state/federal leads, create modeling dataset)
###########################
federal_leads <- read.csv(file.path(DATA_OUT, "04_federal-leads.csv"))
state_leads <- read.csv(file.path(DATA_OUT, "02_state-leads.csv")) %>% select(-c(vote_share, vote_share_l1))
cabinets <- read.csv(file.path(DATA_IN, "02_state-cabinets.csv"))
# Include 1 Sept 2024 state elections so BSW (first ran then) is in the data; BSW before that gets 0
state_results <- read.csv(file.path(DATA_IN, "01_state-election-results.csv")) %>% filter(electiondate <= ymd("2024-09-01"))
state_results$electiondate <- ymd(state_results$electiondate)
if ("electiondate_l1" %in% names(state_results)) state_results$electiondate_l1 <- ymd(state_results$electiondate_l1)
# Keep only main parties; Sonstige = 100% - sum(main) per (land, electiondate)
state_results <- state_results %>% filter(party %in% MAIN_PARTIES)
state_results_oth_5 <- state_results %>%
  group_by(land, electiondate) %>%
  summarise(vote_share = 100 - sum(vote_share, na.rm = TRUE), vote_share_l1 = 100 - sum(vote_share_l1, na.rm = TRUE), .groups = "drop") %>%
  mutate(party = "oth")
if (nrow(state_results_oth_5) > 0L) {
  state_results_oth_5 <- state_results_oth_5 %>%
    left_join(state_results %>% group_by(land, electiondate) %>% slice(1L) %>% ungroup() %>% select(land, electiondate, any_of(setdiff(names(state_results), c("party", "vote_share", "vote_share_l1")))), by = c("land", "electiondate")) %>%
    select(names(state_results))
  state_results <- bind_rows(state_results, state_results_oth_5)
}
# Add BSW row for every (land, electiondate) that has none; use 0 for vote_share/vote_share_l1 before BSW existed
state_results_bsw_missing <- state_results %>%
  group_by(land, electiondate) %>%
  filter(!("bsw" %in% party)) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(party = "bsw", vote_share = 0, vote_share_l1 = 0, on_ballot = 0)
if (nrow(state_results_bsw_missing) > 0L) {
  state_results_bsw_missing <- state_results_bsw_missing %>% select(names(state_results))
  state_results <- bind_rows(state_results, state_results_bsw_missing)
}
# BSW rows with NA vote_share (e.g. pre-2024 in CSV): set to 0 so they stay in full_data
state_results <- state_results %>%
  mutate(
    vote_share = if_else(party == "bsw" & is.na(vote_share), 0, vote_share),
    vote_share_l1 = if_else(party == "bsw" & is.na(vote_share_l1), 0, vote_share_l1)
  )
state_results$year_next_election <- substr(state_results$electiondate, 1, 4) %>% as.numeric
full_data <- state_results %>%
  select(land, year_next_election, party, vote_share, vote_share_l1, electiondate, on_ballot) %>%
  left_join(cabinets, by = c("land", "year_next_election", "party"), suffix = c(".election", ".coalition"))

for (election in unique(full_data$electiondate)) {
  for (land in unique(full_data$land)) {
    for (var in c("pm_name", "pm_party", "cabinet_name", "year_start", "year_end")) {
      filler <- unique(full_data[full_data$electiondate == election & full_data$land == land, var])
      if (length(filler[!is.na(filler)]) > 0) full_data[full_data$electiondate == election & full_data$land == land, var] <- filler[!is.na(filler)]
    }
  }
}
full_data$cabinet_party[!is.na(full_data$vote_share) & is.na(full_data$cabinet_party) & !is.na(full_data$year_end)] <- 0
full_data$is_pm_party[!is.na(full_data$vote_share) & is.na(full_data$is_pm_party) & !is.na(full_data$pm_party)] <- 0

full_data <- merge(full_data, select(state_leads, -c(electiondate_l1)), by = c("party", "land", "electiondate"), all.x = TRUE)
fed_lead_cols <- c("electiondate", "land", "party", paste0("fed_polls_", LEAD_DAYS), "fed_vote_share")
full_data <- merge(full_data, select(federal_leads, all_of(fed_lead_cols)), by = c("party", "land", "electiondate"), all.x = TRUE)
full_data$electiondate <- as.Date(full_data$electiondate)

full_data <- full_data %>% filter(substr(electiondate, 1, 4) > 1990) %>% filter(!is.na(vote_share)) %>% filter(party %in% MAIN_PARTIES)

for (land in unique(full_data$land)) {
  for (electiondate in unique(full_data$electiondate[full_data$land == land])) {
    ed <- as.Date(electiondate)
    full_data <- data.frame(land = land, electiondate = ed,
      year_start = unique(full_data$year_start[full_data$land == land & full_data$electiondate == ed & !is.na(full_data$year_start)]),
      year_end = unique(full_data$year_end[full_data$land == land & full_data$electiondate == ed & !is.na(full_data$year_end)]),
      year_next_election = unique(full_data$year_next_election[full_data$land == land & full_data$electiondate == ed & !is.na(full_data$year_next_election)]),
      on_ballot = 1, pm_name = unique(full_data$pm_name[full_data$land == land & full_data$electiondate == ed & !is.na(full_data$pm_name)]),
      pm_party = unique(full_data$pm_party[full_data$land == land & full_data$electiondate == ed & !is.na(full_data$pm_party)]),
      cabinet_name = unique(full_data$cabinet_name[full_data$land == land & full_data$electiondate == ed & !is.na(full_data$cabinet_name)]),
      cabinet_party = 0, is_pm_party = 0, party = "oth",
      vote_share = 100 - sum(full_data$vote_share[full_data$land == land & full_data$electiondate == ed], na.rm = TRUE),
      vote_share_l1 = 100 - sum(full_data$vote_share_l1[full_data$land == land & full_data$electiondate == ed], na.rm = TRUE)) %>%
      bind_rows(full_data)
  }
}

full_data <- full_data %>% filter(party != "oth") %>% group_by(land, electiondate) %>%
  dplyr::summarise(total_other_shares = sum(vote_share, na.rm = TRUE), total_other_shareslag = sum(vote_share_l1, na.rm = TRUE), total_other_fed_shares = sum(fed_vote_share, na.rm = TRUE), .groups = "drop") %>%
  left_join(full_data, ., by = c("land", "electiondate")) %>%
  mutate(vote_share = case_when(party == "oth" ~ 100 - total_other_shares, TRUE ~ vote_share),
         vote_share_l1 = case_when(party == "oth" ~ 100 - total_other_shareslag, TRUE ~ vote_share_l1),
         fed_vote_share = case_when(party == "oth" ~ 100 - total_other_fed_shares, TRUE ~ fed_vote_share)) %>%
  dplyr::select(-total_other_shares, -total_other_shareslag)

full_data$fed_vote_share_missing <- as.numeric(is.na(full_data$fed_vote_share))
full_data$fed_vote_share[is.na(full_data$fed_vote_share)] <- 0
for (ld in LEAD_DAYS) {
  pcol <- paste0("polls_", ld); mcol <- paste0("polls_", ld, "_missing")
  fcol <- paste0("fed_polls_", ld); fmcol <- paste0("fed_polls_", ld, "_missing")
  full_data[[mcol]] <- as.numeric(is.na(full_data[[pcol]]))
  full_data[[pcol]][is.na(full_data[[pcol]])] <- 0
  full_data[[fmcol]] <- as.numeric(is.na(full_data[[fcol]]))
  full_data[[fcol]][is.na(full_data[[fcol]])] <- 0
}

summarise_oth <- list(
  total_other_shares = quote(sum(vote_share, na.rm = TRUE)),
  total_other_shareslag = quote(sum(vote_share_l1, na.rm = TRUE)),
  total_other_fed_shares = quote(sum(fed_vote_share, na.rm = TRUE)),
  total_fed_vote_share = quote(sum(fed_vote_share, na.rm = TRUE)),
  fed_vote_share_missingall = quote(as.numeric(all(fed_vote_share_missing == 1)))
)
for (ld in LEAD_DAYS) {
  summarise_oth[[paste0("polls_", ld, "_missingall")]] <- substitute(as.numeric(all(x == 1)), list(x = as.symbol(paste0("polls_", ld, "_missing"))))
  summarise_oth[[paste0("total_polls_", ld)]] <- substitute(sum(x, na.rm = TRUE), list(x = as.symbol(paste0("polls_", ld))))
  summarise_oth[[paste0("fed_polls_", ld, "_missingall")]] <- substitute(as.numeric(all(x == 1)), list(x = as.symbol(paste0("fed_polls_", ld, "_missing"))))
  summarise_oth[[paste0("total_fed_polls_", ld)]] <- substitute(sum(x, na.rm = TRUE), list(x = as.symbol(paste0("fed_polls_", ld))))
}
polls_oth_summary <- full_data %>% filter(party != "oth") %>% group_by(land, electiondate) %>%
  dplyr::summarise(!!!summarise_oth, .groups = "drop")
full_data <- left_join(full_data, polls_oth_summary, by = c("land", "electiondate"))
for (ld in LEAD_DAYS) {
  pcol <- paste0("polls_", ld); mcol <- paste0("polls_", ld, "_missing"); tall <- paste0("polls_", ld, "_missingall"); tcol <- paste0("total_polls_", ld)
  fcol <- paste0("fed_polls_", ld); fmcol <- paste0("fed_polls_", ld, "_missing"); fall <- paste0("fed_polls_", ld, "_missingall"); ftcol <- paste0("total_fed_polls_", ld)
  full_data[[pcol]] <- ifelse(full_data$party == "oth" & full_data[[tall]] == 0, 100 - full_data[[tcol]], ifelse(full_data$party == "oth" & full_data[[tall]] == 1, 0, full_data[[pcol]]))
  full_data[[fcol]] <- ifelse(full_data$party == "oth" & full_data[[fall]] == 0, 100 - full_data[[ftcol]], ifelse(full_data$party == "oth" & full_data[[fall]] == 1, 0, full_data[[fcol]]))
  full_data[[mcol]] <- ifelse(full_data$party == "oth" & full_data[[pcol]] == 0, 1, ifelse(full_data$party == "oth" & full_data[[pcol]] != 0, 0, full_data[[mcol]]))
  full_data[[fmcol]] <- ifelse(full_data$party == "oth" & full_data[[fcol]] == 0, 1, ifelse(full_data$party == "oth" & full_data[[fcol]] != 0, 0, full_data[[fmcol]]))
}
full_data$fed_vote_share <- ifelse(full_data$party == "oth" & full_data$fed_vote_share_missingall == 0, 100 - full_data$total_fed_vote_share, ifelse(full_data$party == "oth" & full_data$fed_vote_share_missingall == 1, 0, full_data$fed_vote_share))
full_data$fed_vote_share_missing <- ifelse(full_data$party == "oth" & full_data$fed_vote_share == 0, 1, ifelse(full_data$party == "oth" & full_data$fed_vote_share != 0, 0, full_data$fed_vote_share_missing))

log_fed_lr <- function(fp, fv) {
  fp <- ifelse(fp == 0, 0.000001, fp)
  fv <- ifelse(fv == 0, 0.000001, fv)
  log(fp / (100 - fp)) - log(fv / (100 - fv))
}
for (ld in LEAD_DAYS) {
  fcol <- paste0("fed_polls_", ld)
  full_data[[paste0("fed_trends_lr_", ld)]] <- log_fed_lr(full_data[[fcol]], full_data$fed_vote_share)
  full_data[[paste0("fed_trend_lr_", ld, "_missing")]] <- as.numeric(full_data[[paste0("fed_polls_", ld, "_missing")]] | full_data$fed_vote_share_missing)
}
fed_lr_missingall_list <- lapply(LEAD_DAYS, function(ld) substitute(as.numeric(all(x == 1)), list(x = as.symbol(paste0("fed_trend_lr_", ld, "_missing")))))
names(fed_lr_missingall_list) <- paste0("fed_trend_lr_", LEAD_DAYS, "_missingall")
full_data <- full_data %>% group_by(land, electiondate) %>%
  dplyr::summarise(!!!fed_lr_missingall_list, .groups = "drop") %>% left_join(full_data, ., by = c("land", "electiondate"))
for (ld in LEAD_DAYS) {
  full_data[[paste0("fed_trend_", ld)]] <- full_data[[paste0("fed_polls_", ld)]] - full_data$fed_vote_share
  full_data[[paste0("fed_trend_", ld, "_missing")]] <- as.numeric(full_data[[paste0("fed_polls_", ld, "_missing")]] | full_data$fed_vote_share_missing)
}
fed_trend_missingall_list <- lapply(LEAD_DAYS, function(ld) substitute(as.numeric(all(x == 1)), list(x = as.symbol(paste0("fed_trend_", ld, "_missing")))))
names(fed_trend_missingall_list) <- paste0("fed_trend_", LEAD_DAYS, "_missingall")
full_data <- full_data %>% group_by(land, electiondate) %>%
  dplyr::summarise(!!!fed_trend_missingall_list, .groups = "drop") %>% left_join(full_data, ., by = c("land", "electiondate"))

full_data <- arrange(full_data, land, electiondate, party)
select_list <- list(
  state = quote(land), elec_ind = quote(elec_ind), year = quote(year_start), electiondate = quote(electiondate), party = quote(party), voteshare = quote(vote_share), voteshare_l1 = quote(vote_share_l1), pm = quote(is_pm_party), gov = quote(cabinet_party)
)
for (ld in LEAD_DAYS) {
  select_list[[paste0("polls_", ld)]] <- as.symbol(paste0("polls_", ld))
  select_list[[paste0("pollsNA_", ld)]] <- as.symbol(paste0("polls_", ld, "_missing"))
  select_list[[paste0("pollsNAall_", ld)]] <- as.symbol(paste0("polls_", ld, "_missingall"))
}
for (ld in LEAD_DAYS) {
  select_list[[paste0("fed_polls_", ld)]] <- as.symbol(paste0("fed_polls_", ld))
  select_list[[paste0("fed_pollsNA_", ld)]] <- as.symbol(paste0("fed_polls_", ld, "_missing"))
  select_list[[paste0("fed_pollsNAall_", ld)]] <- as.symbol(paste0("fed_polls_", ld, "_missingall"))
}
select_list <- c(select_list, list(fed_vote_share = quote(fed_vote_share), fed_vote_share_missing = quote(fed_vote_share_missing), fed_vote_share_missingall = quote(fed_vote_share_missingall)))
for (ld in LEAD_DAYS) {
  select_list[[paste0("fed_trend_", ld)]] <- as.symbol(paste0("fed_trend_", ld))
  select_list[[paste0("fed_trend_", ld, "_missing")]] <- as.symbol(paste0("fed_trend_", ld, "_missing"))
  select_list[[paste0("fed_trend_", ld, "_missingall")]] <- as.symbol(paste0("fed_trend_", ld, "_missingall"))
}
for (ld in LEAD_DAYS) {
  select_list[[paste0("fed_trends_lr_", ld)]] <- as.symbol(paste0("fed_trends_lr_", ld))
  select_list[[paste0("fed_trend_lr_", ld, "_missing")]] <- as.symbol(paste0("fed_trend_lr_", ld, "_missing"))
  select_list[[paste0("fed_trend_lr_", ld, "_missingall")]] <- as.symbol(paste0("fed_trend_lr_", ld, "_missingall"))
}
full_data <- full_data %>% mutate(elec_ind = paste(land, electiondate, sep = "_")) %>%
  dplyr::select(!!!select_list) %>%
  mutate(new_party = case_when(is.na(voteshare_l1) ~ 1, TRUE ~ 0), voteshare_l1 = case_when(is.na(voteshare_l1) ~ 0, TRUE ~ voteshare_l1), pm = case_when(is.na(pm) ~ 0, TRUE ~ pm), gov = case_when(is.na(gov) ~ 0, TRUE ~ gov)) %>%
  mutate(voteshare_l1 = case_when(voteshare_l1 == 100 & party == "oth" ~ 0, TRUE ~ voteshare_l1))

log_ratio <- function(x, corre_fct = 0.01) {
  x[x < 0] <- 0
  x[x == 0] <- x[x == 0] + corre_fct
  x[x >= 1] <- 1 - corre_fct
  return(log(x / (1 - x)))
}
div100_list <- list(pmXgov = quote(pm * gov), voteshare = quote(voteshare/100), voteshare_l1 = quote(voteshare_l1/100), fed_vote_share = quote(fed_vote_share/100))
for (ld in LEAD_DAYS) {
  div100_list[[paste0("polls_", ld)]] <- parse(text = paste0("polls_", ld, "/100"))[[1]]
  div100_list[[paste0("fed_polls_", ld)]] <- parse(text = paste0("fed_polls_", ld, "/100"))[[1]]
  div100_list[[paste0("fed_trend_", ld)]] <- parse(text = paste0("fed_trend_", ld, "/100"))[[1]]
}
full_data <- full_data %>% mutate(!!!div100_list)
round_list <- list(election_type = quote("past"), voteshare_l1 = quote(round(voteshare_l1, 3)))
for (ld in LEAD_DAYS) {
  round_list[[paste0("polls_", ld)]] <- parse(text = paste0("round(polls_", ld, ", 3)"))[[1]]
  round_list[[paste0("fed_polls_", ld)]] <- parse(text = paste0("round(fed_polls_", ld, ", 3)"))[[1]]
  round_list[[paste0("fed_trend_", ld)]] <- parse(text = paste0("round(fed_trend_", ld, ", 3)"))[[1]]
}
round_list <- c(round_list, list(fed_vote_share = quote(round(fed_vote_share, 3))))
full_data <- full_data %>% mutate(!!!round_list)
pollslr_list <- list(votesharelr = quote(log_ratio(voteshare)), votesharelr_l1 = quote(log_ratio(voteshare_l1)))
for (ld in LEAD_DAYS) pollslr_list[[paste0("pollslr_", ld)]] <- parse(text = paste0("log_ratio(polls_", ld, ")"))[[1]]
full_data <- full_data %>% mutate(!!!pollslr_list)

save(full_data, lead_days, file = file.path(DATA_OUT, "05_full_data.RData"))
message("  Written 05_full_data.RData")
message("Done. Outputs in ", DATA_OUT)
