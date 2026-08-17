#' ---
#' title: "Figures Forcast Data"
#' date: " 2024"
#' ---

#' ---
#' title: "Development Model"
#' date: " 2024"
#' ---


###########################
# Prepare environment
###########################

if (basename(getwd()) == "code") setwd("..")
ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "packages.R"))
ROOT <- getwd()
source(file.path(ROOT, "code", "auxilary", "functions.R"))
DATA_OUT <- file.path(ROOT, "data", "output")
DATA_IN <- file.path(ROOT, "data", "input")
DATA_OUT_FORECAST <- file.path(ROOT, "data", "output", "forecast")
RESULTS_FIG_FORECAST <- file.path(ROOT, "results", "figures", "forecast")
RESULTS_TAB_FORECAST <- file.path(ROOT, "results", "tables", "forecast")
dir.create(RESULTS_FIG_FORECAST, showWarnings = FALSE, recursive = TRUE)
dir.create(RESULTS_TAB_FORECAST, showWarnings = FALSE, recursive = TRUE)

###########################
# Load forecast data (from 03_forecast.R). One fcst_ci, one stand_date; "Stand: <date>".
###########################

fcst_state_path <- file.path(DATA_OUT_FORECAST, "fcst_state.Rdata")
if (!file.exists(fcst_state_path)) {
  message("No forecast file found at ", fcst_state_path, ". Skipping forecast figures.")
  quit(status = 0, save = "no")
}
load(fcst_state_path)
if (!exists("fcst_ci", inherits = FALSE)) stop("No fcst_ci in fcst_state.Rdata; run 03_forecast.R first.")
if (!exists("stand_date", inherits = FALSE)) stand_date <- Sys.Date()
if (!exists("fcst_draws", inherits = FALSE)) fcst_draws <- tibble(draw = integer(), party = character(), state = character(), posterior_draw = double(), elec_ind = character())
if (!exists("model_type_suffix", inherits = FALSE)) model_type_suffix <- "all"
# Use saved results and polls from forecast (main parties + oth) when available
if (!exists("last_election_results", inherits = FALSE)) last_election_results <- NULL
if (!exists("state_polls_for_figure", inherits = FALSE)) state_polls_for_figure <- NULL

# Single forecast table; caption/title use "Stand: <stand_date>"
all_fcst <- fcst_ci %>%
  mutate(party_name = map_party_names(party), state_code = state, state = map_state_names(state))

state_info <- all_fcst %>% distinct(state_code, state)
if ("electiondate" %in% names(all_fcst)) {
  state_info <- all_fcst %>% distinct(state_code, state, electiondate) %>% group_by(state_code, state) %>% slice(1L) %>% ungroup()
}
state_codes_in_fcst   <- state_info$state_code
state_display_in_fcst <- state_info$state
state_election_date <- if ("electiondate" %in% names(state_info)) setNames(format(state_info$electiondate, "%Y-%m-%d"), state_info$state_code) else character(0)

# No lead/model facets anymore; one forecast per state (Stand: stand_date)
stand_label <- format(as.Date(stand_date), "%d.%m.%Y")
model_label <- if (model_type_suffix == "polls") "Polls" else "Polls + Fund."
all_fcst$model <- factor(model_label, levels = model_label)
all_fcst$lead <- factor("Stand", levels = "Stand")
models_in_fcst <- model_label
leads_in_fcst  <- "Stand"

# State polls (for scatter) and last-election results (for grey bars)
# Prefer saved data from forecast run so we use the same main-parties + oth definition.
forecast_party_names <- setdiff(unique(all_fcst$party_name), "Other")
if (!is.null(state_polls_for_figure) && nrow(state_polls_for_figure) > 0L) {
  state_polls <- as.data.frame(state_polls_for_figure)
  state_polls$date <- as.Date(state_polls$date, origin = "1970-01-01")
  message("  Using state polls from fcst_state.Rdata (same as forecast; main parties + oth).")
} else {
  state_polls_path <- file.path(DATA_OUT, "01_state-polls.csv")
  state_polls <- if (file.exists(state_polls_path)) {
    out <- read.csv(state_polls_path, stringsAsFactors = FALSE)
    out$date <- as.Date(out$date)
    out
  } else {
    data.frame(date = as.Date(character()), land = character(), party = character(), poll_share = double(), stringsAsFactors = FALSE)
  }
}
if (!is.null(last_election_results) && nrow(last_election_results) > 0L) {
  results <- as.data.frame(last_election_results)
  message("  Using last election results from fcst_state.Rdata (main parties + oth).")
} else {
  state_results_path <- file.path(DATA_IN, "01_state-election-results.csv")
  state_results <- if (file.exists(state_results_path)) {
    out <- read.csv(state_results_path, stringsAsFactors = FALSE)
    out$electiondate <- as.Date(out$electiondate)
    out
  } else {
    data.frame(electiondate = as.Date(character()), land = character(), party = character(), vote_share = double(), stringsAsFactors = FALSE)
  }
  # Last election result per state (grey bars) — only when not using saved results
  # Normalise land so "sa" (Sachsen-Anhalt) is treated as "st". oth = 100% - sum(main).
  results <- if (nrow(state_results) > 0L && length(state_codes_in_fcst) > 0L && length(forecast_party_names) > 0L) {
    main <- state_results %>%
      mutate(land = if_else(land == "sa", "st", land)) %>%
      filter(land %in% state_codes_in_fcst) %>%
      group_by(land) %>%
      filter(electiondate == max(electiondate, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        state = map_state_names(land),
        party_name = map_party_names(trimws(tolower(as.character(party)))),
        share = vote_share / 100
      ) %>%
      filter(party_name %in% forecast_party_names) %>%
      group_by(state, party_name) %>%
      summarise(share = sum(share), .groups = "drop")
    main <- main %>% tidyr::complete(state, party_name = forecast_party_names, fill = list(share = 0))
    other_rows <- main %>%
      group_by(state) %>%
      summarise(share = 1 - sum(share, na.rm = TRUE), .groups = "drop") %>%
      mutate(party_name = "Other", share = pmax(0, pmin(1, share)))
    bind_rows(main, other_rows) %>% as.data.frame()
  } else {
    data.frame(party_name = character(), share = double(), state = character(), stringsAsFactors = FALSE)
  }
}

# Log: forecast figure creation and what estimates sum to
message("Forecast figures: Stand ", stand_label, "; ", length(state_codes_in_fcst), " state(s): ", paste(state_codes_in_fcst, collapse = ", "))
sum_by_state <- all_fcst %>% group_by(state_code, state) %>% summarise(sum_fit = sum(fit, na.rm = TRUE), n_parties = n(), .groups = "drop")
for (k in seq_len(nrow(sum_by_state))) {
  message("  ", sum_by_state$state[k], " (", sum_by_state$state_code[k], "): sum(fit) = ", round(sum_by_state$sum_fit[k], 4), " (", sum_by_state$n_parties[k], " parties)")
}
message("  (Sonstige in training and forecast; predictions normalized to sum to 100% per election.)")
message("  Output dir: ", RESULTS_FIG_FORECAST)

# Poll dots per state (last 6 months) — same window as in the plot
polls_cutoff_6m <- as.Date(stand_date) - months(6)
n_oth_in_polls <- sum(state_polls$party %in% c("oth", "Oth", "OTH") & !is.na(state_polls$poll_share), na.rm = TRUE)
message("  Poll dots in figure (last 6 months, Stand ", stand_label, "); state_polls rows with party=oth: ", n_oth_in_polls)
for (i in seq_along(state_codes_in_fcst)) {
  state_code <- state_codes_in_fcst[i]
  state <- state_display_in_fcst[i]
  plot_dat <- all_fcst[all_fcst$state == state & all_fcst$model == "Polls + Fund." & all_fcst$lead == "Stand", ]
  if (nrow(plot_dat) == 0L) next
  land_match <- if (state_code == "st") c("st", "sa") else state_code
  state_polls_6m <- state_polls %>%
    filter(land %in% land_match, date >= polls_cutoff_6m, !is.na(poll_share)) %>%
    mutate(party = trimws(tolower(as.character(party))), party_name = map_party_names(party)) %>%
    filter(party_name %in% plot_dat$party_name)
  n_dots <- nrow(state_polls_6m)
  n_oth <- sum(state_polls_6m$party_name == "Other", na.rm = TRUE)
  n_poll_dates <- state_polls %>%
    filter(land %in% land_match, date >= polls_cutoff_6m, !is.na(poll_share)) %>%
    distinct(date) %>%
    nrow()
  last_poll <- state_polls %>%
    filter(land %in% land_match, date >= polls_cutoff_6m, !is.na(poll_share)) %>%
    summarise(last = max(date, na.rm = TRUE)) %>%
    pull(last)
  last_str <- if (length(last_poll) && !is.na(last_poll)) format(as.Date(last_poll), "%Y-%m-%d") else "none"
  message("    ", state, " (", state_code, "): ", n_dots, " dots (", n_oth, " Sonstige) from ", n_poll_dates, " poll date(s); last poll: ", last_str)
  # Log poll values (date, institut, party shares), newest first
  polls_to_log <- state_polls_6m %>%
    mutate(party_display = case_when(party_name == "Greens" ~ "Grüne", party_name == "Other" ~ "Sonstige", TRUE ~ party_name))
  if (nrow(polls_to_log) > 0L) {
    has_institut <- "institut" %in% names(polls_to_log)
    poll_ids <- if (has_institut) {
      polls_to_log %>% distinct(date, institut) %>% arrange(desc(date))
    } else {
      polls_to_log %>% distinct(date) %>% arrange(desc(date))
    }
    for (j in seq_len(nrow(poll_ids))) {
      row <- poll_ids[j, , drop = FALSE]
      s <- polls_to_log %>% filter(date == row$date[1])
      if (has_institut) s <- s %>% filter(institut == row$institut[1])
      vals <- s %>% arrange(party_display) %>% mutate(v = paste0(party_display, " ", round(poll_share, 1), "%")) %>% pull(v)
      msg_inst <- if (has_institut) paste0(", ", row$institut[1]) else ""
      msg_date <- format(as.Date(row$date[1]), "%Y-%m-%d")
      message("      ", msg_date, msg_inst, ": ", paste(vals, collapse = ", "))
    }
  }
}

all_fcst %>%

  ggplot(aes(x = reorder(party_name, -fit), y = fit, color = party_name, fill = party_name, group = lead)) +
  geom_col(data = results, aes(x = party_name, y = share, group = NA), color = NA, fill = "grey", alpha = .2) +
  scale_color_manual(values = party_colors, labels = names(party_colors)) +
  scale_fill_manual(values = party_colors, labels = names(party_colors)) +
  geom_hline(yintercept = 0.05, linetype = "dotted", size = .5, color = "black") +
  geom_linerange(aes(ymin = lwr, ymax = upr), linewidth = 2, alpha = 0.3, position=position_dodge(width=.5)) + # , col = party_colors[all_fcst$party_name]) +
  geom_point(size = 2, color = NA, shape = 21, stroke = 2, position=position_dodge(width=.5)) + #, fill = party_colors[all_fcst$party_name]) +
  geom_point(size = 1, fill = "white", shape = 21, position=position_dodge(width=.5)) +
  # geom_label_repel(aes(y = upr + 0.03, label = paste(round(fit * 100, 1), "%")), fill = NA, col = "black", position=position_dodge(width=.5), size = 2) +
  scale_y_continuous(
    limits = c(0, max(0.1, max(all_fcst$upr, na.rm = TRUE) + 0.05)),
    breaks = seq(0, 1, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Forecasts of the Election Results",
    subtitle = stand_label,
    # caption = "The forecasts are based on a Bayesian regression model fitted on state elections from 1990 to 2025.\nThe intervals show 5/6 Credible Intervals, and the point represents the posterior mean.\nThe forecasts are grouped by lead times. The bars show the election results.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    text = element_text(family = "sans"),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 8, face = "bold", color = "black"),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  facet_grid(state ~ model, scales = "free", space = "free") 
 
# Same plots but one by one (states, models, leads from forecast data)
# Filenames: forecast_{state}_{electiondate}_{model_type}_days_de (model_type = polls | all); German labels only
model_slug <- function(m) { m <- as.character(m); if (m == "Polls + Fund.") "all" else if (m == "Polls") "polls" else if (m == "Fund.") "fund" else tolower(gsub("[^a-z]", "", m)) }
for (i in seq_along(state_codes_in_fcst)) {
  state_code <- state_codes_in_fcst[i]
  state <- state_display_in_fcst[i]
  edate_str <- if (length(state_election_date) && state_code %in% names(state_election_date)) state_election_date[state_code] else ""
  state_de <- state %>% str_replace_all(c("Thuringia" = "Thüringen", "Saxony" = "Sachsen"))
  edate_label <- if (nzchar(edate_str)) format(as.Date(edate_str), "%d.%m.%Y") else ""
  subtitle_text <- if (nzchar(edate_label)) paste0(state_de, ", ", edate_label) else state_de
  for (model in models_in_fcst) {
    for (lead in leads_in_fcst) {
      plot_dat <- all_fcst[all_fcst$state == state & all_fcst$model == model & all_fcst$lead == lead, ]
      if (nrow(plot_dat) == 0L) next
      y_max <- max(0.1, max(plot_dat$upr, na.rm = TRUE) + 0.05)
      y_breaks <- seq(0, 1, 0.1)
      y_breaks <- y_breaks[y_breaks <= y_max]
      # Polls from last 6 months for this state (scatter in background)
      polls_cutoff <- as.Date(stand_date) - months(6)
      party_order <- levels(reorder(plot_dat$party_name, -plot_dat$fit))
      # Sonstige (Other) always on the right
      if ("Other" %in% party_order) party_order <- c(setdiff(party_order, "Other"), "Other")
      land_match <- if (state_code == "st") c("st", "sa") else state_code
      # Include oth (Sonstige): state_polls from 01_build_data has party "oth" = 100% - sum(others)
      # Parties at 1%: remove and add their share to Sonstige
      grp_cols <- c("date", "land")
      if ("institut" %in% names(state_polls)) grp_cols <- c(grp_cols, "institut")
      polls_state <- state_polls %>%
        filter(land %in% land_match, date >= polls_cutoff, !is.na(poll_share)) %>%
        mutate(party = trimws(tolower(as.character(party))), party_name = map_party_names(party), share = poll_share / 100) %>%
        filter(party_name %in% plot_dat$party_name) %>%
        group_by(across(all_of(grp_cols))) %>%
        mutate(to_sonstige = sum(share[share <= 0.01], na.rm = TRUE)) %>%
        filter(share > 0.01) %>%
        mutate(share = if_else(party_name == "Other", share + to_sonstige, share)) %>%
        ungroup()
      # Add Other rows for polls where we removed 1% parties (including Other itself)
      polls_state_oth <- polls_state %>%
        group_by(across(all_of(grp_cols))) %>%
        filter(to_sonstige > 0 & !any(party_name == "Other")) %>%
        summarise(party_name = "Other", share = first(to_sonstige), .groups = "drop") %>%
        mutate(party_name = factor(party_name, levels = party_order))
      polls_state <- polls_state %>% select(-to_sonstige)
      if (nrow(polls_state_oth) > 0L) polls_state <- bind_rows(polls_state, polls_state_oth)
      polls_state <- polls_state %>% mutate(party_name = factor(party_name, levels = party_order))
      # Grey bars: only parties in forecast, same x order as forecast so bars align
      state_curr <- state
      results_plot <- results %>%
        filter(state == state_curr, party_name %in% plot_dat$party_name) %>%
        mutate(party_name = factor(party_name, levels = party_order))
      plot_dat <- plot_dat %>% mutate(party_name = factor(party_name, levels = party_order))
      plot_dat %>%
        ggplot(aes(x = party_name, y = fit, color = party_name, fill = party_name)) +
        geom_blank() +
        geom_col(data = results_plot, aes(x = party_name, y = share, group = NA), color = NA, fill = "grey", alpha = .2) +
        scale_color_manual(values = party_colors, labels = names(party_colors)) +
        scale_fill_manual(values = party_colors, labels = names(party_colors)) +
        geom_hline(yintercept = 0.05, linetype = "dotted", size = .5, color = "black") +
        geom_linerange(aes(ymin = lwr, ymax = upr), linewidth = 10, alpha = 0.3) +
        geom_point(size = 6, color = "white", shape = 21, stroke = 2) +
        geom_point(size = 2, fill = "white", shape = 21) +
        geom_label(aes(y = upr + 0.03, label = paste0(round(fit * 100, 0), "%")), fill = NA, color = "black") +
        geom_point(data = polls_state, aes(x = party_name, y = share, color = party_name), alpha = 0.5, size = 2,
                  position = position_jitter(width = 0.15, height = 0, seed = 1L), inherit.aes = FALSE) +
        scale_y_continuous(
          limits = c(0, y_max),
          breaks = y_breaks,
          labels = scales::percent_format(accuracy = 1)
        ) +
        labs(
          title = "Prognose Landtagswahl",
          subtitle = subtitle_text,
          caption = str_c(
            "Die Prognosen basieren auf einem bayesianischen Regressionsmodell (Landtagswahlen 1990-2025). Stand: ", stand_label, ".\n",
            "Die Intervalle zeigen 83%-Wahrscheinlichkeitsspannen. Graue Balken: letzte Landtagswahl.\n",
            "Punkte: Umfragen der letzten 6 Monate."
          ),
          x = NULL,
          y = NULL
        ) +
        theme_minimal(base_family = "Helvetica") +
        theme(
          text = element_text(family = "Helvetica"),
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.text.x = element_text(size = 8, face = "bold", color = "black"),
          axis.line.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.margin = margin(10, 10, 10, 10)
        ) +
        scale_x_discrete(labels = function(x) case_when(x == "Greens" ~ "Grüne", x == "Other" ~ "Sonstige", TRUE ~ x)) -> p
      fbase <- if (nzchar(edate_str)) paste0("forecast_", state_code, "_", edate_str, "_", model_type_suffix, "_days_de") else paste0("forecast_", state_code, "_", model_type_suffix, "_days_de")
      pdf(file.path(RESULTS_FIG_FORECAST, paste0(fbase, ".pdf")), width = 6*1.5, height = 6, family = "Helvetica", encoding = "ISOLatin1", useDingbats = FALSE)
      print(p)
      dev.off()
      ggsave(filename = file.path(RESULTS_FIG_FORECAST, paste0(fbase, ".png")), plot = p, device = "png", type = "cairo", dpi = 300, height = 5, width = 5*1.5, bg = "white")
      message("  Written ", fbase, ".pdf, .png")
    }
  }
}




# Calculate probabilities =====
  coal_majo <- function(share, share_above_hurdle){
    if (any(share < 0.05)) return(FALSE)
    sum(share) / share_above_hurdle > 0.5
  }

# Scenario probabilities for BW and RP (from fcst_draws; Stand: stand_date)
  if (exists("fcst_draws") && nrow(fcst_draws) > 0L) {
    scenario_draws <- fcst_draws %>% filter(state %in% c("bw", "rp"))
    if (nrow(scenario_draws) > 0L) {
      scenario_probs <- scenario_draws %>%
        group_by(state, draw) %>%
        summarise(
          cdu_staerkste = max(posterior_draw) == posterior_draw[party == "cdu"],
          gru_staerkste = max(posterior_draw) == posterior_draw[party == "gru"],
          spd_staerkste = max(posterior_draw) == posterior_draw[party == "spd"],
          afd_staerkste = max(posterior_draw) == posterior_draw[party == "afd"],
          fdp_above5 = any(party == "fdp" & posterior_draw > 0.05),
          lin_above5 = any(party == "lin" & posterior_draw > 0.05),
          bsw_above5 = any(party == "bsw" & posterior_draw > 0.05),
          .groups = "drop"
        ) %>%
        group_by(state) %>%
        summarise(
          cdu_staerkste = mean(cdu_staerkste),
          gru_staerkste = mean(gru_staerkste),
          spd_staerkste = mean(spd_staerkste),
          afd_staerkste = mean(afd_staerkste),
          fdp_above5 = mean(fdp_above5),
          lin_above5 = mean(lin_above5),
          bsw_above5 = mean(bsw_above5),
          .groups = "drop"
        )
      prob_staerkste_bw <- scenario_probs %>% filter(state == "bw") %>%
        select(state, cdu_staerkste, gru_staerkste, afd_staerkste) %>%
        pivot_longer(cols = c(cdu_staerkste, gru_staerkste, afd_staerkste), names_to = "party_code", values_to = "probability") %>%
        mutate(
          scenario_type = "Stärkste Kraft",
          party_name = case_when(party_code == "cdu_staerkste" ~ "CDU", party_code == "gru_staerkste" ~ "Grüne", party_code == "afd_staerkste" ~ "AfD", TRUE ~ party_code)
        ) %>%
        select(state, scenario_type, party_name, probability)
      prob_staerkste_rp <- scenario_probs %>% filter(state == "rp") %>%
        select(state, cdu_staerkste, spd_staerkste, afd_staerkste) %>%
        pivot_longer(cols = c(cdu_staerkste, spd_staerkste, afd_staerkste), names_to = "party_code", values_to = "probability") %>%
        mutate(
          scenario_type = "Stärkste Kraft",
          party_name = case_when(party_code == "cdu_staerkste" ~ "CDU", party_code == "spd_staerkste" ~ "SPD", party_code == "afd_staerkste" ~ "AfD", TRUE ~ party_code)
        ) %>%
        select(state, scenario_type, party_name, probability)
      prob_hurdle <- scenario_probs %>%
        select(state, fdp_above5, lin_above5, bsw_above5) %>%
        pivot_longer(cols = c(fdp_above5, lin_above5, bsw_above5), names_to = "party_code", values_to = "probability") %>%
        mutate(
          scenario_type = "Über 5% Hürde",
          party_name = case_when(
            party_code == "fdp_above5" ~ "FDP",
            party_code == "lin_above5" ~ "Linke",
            party_code == "bsw_above5" ~ "BSW",
            TRUE ~ party_code
          )
        ) %>%
        select(state, scenario_type, party_name, probability)
      scenario_prob_long <- bind_rows(prob_staerkste_bw, prob_staerkste_rp, prob_hurdle) %>%
        mutate(state = map_state_names(state))
      # Facet strip: state name + election date (same as single-state figures)
      state_info_bw_rp <- state_info %>% filter(state_code %in% c("bw", "rp"))
      scenario_state_labels <- if (nrow(state_info_bw_rp) > 0L && "electiondate" %in% names(state_info_bw_rp)) {
        setNames(
          paste0(state_info_bw_rp$state, ", ", format(as.Date(state_info_bw_rp$electiondate), "%d.%m.%Y")),
          state_info_bw_rp$state
        )
      } else setNames(state_info_bw_rp$state, state_info_bw_rp$state)
      party_colors_local <- party_colors
      if (!"Grüne" %in% names(party_colors_local)) party_colors_local["Grüne"] <- party_colors_local["Greens"]
      # Only use colors for parties that appear in the data
      party_colors_local <- party_colors_local[names(party_colors_local) %in% scenario_prob_long$party_name]
      p_scenario <- ggplot(scenario_prob_long, aes(x = party_name, y = probability, fill = party_name)) +
        geom_col(width = 0.7, alpha = 0.9) +
        geom_label(aes(y = probability + 0.1, label = scales::percent(probability, accuracy = 1)),
          fill = "white", color = "black", fontface = "bold", show.legend = FALSE
        ) +
        scale_fill_manual(values = party_colors_local, guide = "none", drop = TRUE) +
        scale_x_discrete(drop = TRUE) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
        labs(
          x = NULL, y = "Wahrscheinlichkeit",
          title = "Szenario-Wahrscheinlichkeiten",
          caption = str_c("Die Prognosen basieren auf einem bayesianischen Regressionsmodell (Landtagswahlen 1990-2025). Stand: ", stand_label, ".")
        ) +
        facet_wrap(state ~ scenario_type, scales = "free", labeller = labeller(state = function(x) ifelse(x %in% names(scenario_state_labels), scenario_state_labels[x], x))) +
        theme_minimal(base_family = "Helvetica") +
        theme(
          text = element_text(family = "Helvetica"),
          strip.text = element_text(face = "bold"),
          panel.grid.major.x = element_blank(),
          plot.caption = element_text(hjust = 0, size = rel(0.9))
        ) +
        # ylim(0,100) +
        coord_flip() + ylab("")
      dir.create(RESULTS_FIG_FORECAST, showWarnings = FALSE, recursive = TRUE)
      pdf(file.path(RESULTS_FIG_FORECAST, "fig_scenario_prob_bw_rp.pdf"), width = 8, height = 6, family = "Helvetica", encoding = "ISOLatin1", useDingbats = FALSE)
      print(p_scenario)
      dev.off()
      ggsave(file.path(RESULTS_FIG_FORECAST, "fig_scenario_prob_bw_rp.png"), plot = p_scenario, width = 8, height = 6, device = "png", type = "cairo", dpi = 300, bg = "white")
      message("  Written fig_scenario_prob_bw_rp.pdf, .png")
    }
  }

# Write forecast values and scenario probabilities for blog/copy-paste (same as in figures)
fcst_for_txt <- all_fcst %>%
  mutate(
    party_display = case_when(party_name == "Greens" ~ "Grüne", party_name == "Other" ~ "Sonstige", TRUE ~ party_name),
    fit_pct = round(fit * 100, 0),
    lwr_pct = round(lwr * 100, 0),
    upr_pct = round(upr * 100, 0),
    interval_str = paste0(fit_pct, "% (", lwr_pct, "%–", upr_pct, "%)")
  ) %>%
  arrange(state, party_name == "Other", desc(fit))
txt_lines <- c(
  paste0("Forecast values for blog · Stand: ", stand_label),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "=== FORECASTS (point estimate, 5/6 credible interval) ===",
  ""
)
for (st in unique(fcst_for_txt$state)) {
  txt_lines <- c(txt_lines, paste0("--- ", st, " ---"))
  st_dat <- fcst_for_txt %>% filter(state == st)
  for (i in seq_len(nrow(st_dat))) {
    txt_lines <- c(txt_lines, paste0("  ", st_dat$party_display[i], ": ", st_dat$interval_str[i]))
  }
  txt_lines <- c(txt_lines, "")
}
if (exists("scenario_prob_long") && nrow(scenario_prob_long) > 0L) {
  txt_lines <- c(txt_lines, "=== SZENARIO-WAHRSCHEINLICHKEITEN ===", "")
  for (st in unique(scenario_prob_long$state)) {
    for (stype in unique(scenario_prob_long$scenario_type[scenario_prob_long$state == st])) {
      sub <- scenario_prob_long %>% filter(state == st, scenario_type == stype)
      probs_str <- paste0(sub$party_name, " ", round(sub$probability * 100, 0), "%", collapse = ", ")
      txt_lines <- c(txt_lines, paste0(st, " – ", stype, ": ", probs_str))
    }
  }
}
# Add model evaluation (MAE, RMSE, coverage) for the leads used in this forecast
eval_path <- file.path(DATA_OUT, "model", "model_bayes_eval.RDS")
if (file.exists(eval_path)) {
  res_eval <- readRDS(eval_path)
  leads_used <- unique(all_fcst$lead_days_used[!is.na(all_fcst$lead_days_used)])
  if (length(leads_used) > 0L && "lead" %in% names(res_eval)) {
    eval_used <- res_eval %>%
      filter(as.character(lead) %in% as.character(leads_used)) %>%
      mutate(lead_display = paste0(lead, " Tage"), mae = round(mae, 2), rmse = round(rmse, 2), coverage = round(coverage, 3))
    if (nrow(eval_used) > 0L) {
      txt_lines <- c(txt_lines, "", "=== MODELL-EVALUATION (MAE, RMSE, Coverage für verwendete Lead-Modelle) ===", "")
      for (i in seq_len(nrow(eval_used))) {
        r <- eval_used[i, ]
        txt_lines <- c(txt_lines, paste0(r$lead_display, ": MAE ", r$mae, " pp, RMSE ", r$rmse, " pp, Coverage ", r$coverage))
      }
    }
  }
}
fcst_txt_path <- file.path(RESULTS_FIG_FORECAST, "forecast_blog_values.txt")
writeLines(txt_lines, fcst_txt_path)
message("  Written ", fcst_txt_path)
