# When keep_env=TRUE (e.g. run_everything sourcing 03), do not wipe so ROOT/paths survive
if (!isTRUE(getOption("keep_env"))) rm(list = ls())

# Minimal set for pipeline + forecast figures (match .github/workflows/run-forecast.yml)
# Add haven, openxlsx, rvest, kableExtra, stargazer, etc. locally if needed for other scripts
p_required <- c("dplyr",
                "tidyr",
                "ggplot2",
                "lubridate",
                "stringr",
                "readr",
                "httr",
                "dlm",
                "rstan",
                "rstanarm",
                "ggrepel",
                "xtable")

packages <- rownames(installed.packages())
p_to_install <- p_required[!(p_required %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

sapply(p_required, require, character.only = TRUE)
rm(p_required, p_to_install, packages)
