
library(tidyverse)
library(arrow)
library(ggrepel)

OUT_DIR <- "dev/outputs/case_studies"
SIM_DIR <- file.path(OUT_DIR, "simulations")

FOCUS_SCENARIOS <- c("SSP3-7.0 / 2025-2035", "SSP3-7.0 / 2040-2060")
SCENARIO_LABELS <- c("SSP3-7.0 / 2025-2035" = "SSP3 2030", "SSP3-7.0 / 2040-2060" = "SSP3 2050")

EXCEED_DIR <- file.path(SIM_DIR, "exceedance_plots")
dir.create(EXCEED_DIR, showWarnings = FALSE, recursive = TRUE)

POVERTY_LINES <- c(300, 830)

exceed_policy_labels <- c(
  "sp_p5_bottom40"      = "Cash transfer",
  "elec_universal"      = "Universal electricity",
  "secondary_universal" = "Universal secondary education"
)

exceed_policy_colours <- c(
  "no_policy"           = "#1d2f73",
  "sp_p5_bottom40"      = "#c42716",
  "elec_universal"      = "#c42716",
  "secondary_universal" = "#c42716"
)

exceed_scenario_lty <- c(
  "SSP3-7.0 / 2025-2035" = "solid",
  "SSP3-7.0 / 2040-2060" = "dashed"
)

all_policy_labels <- c("no_policy", names(exceed_policy_labels))

# Log x-axis: return period = 1/exceed_prob
# Breaks at exceedance probs corresponding to 1-in-N year events
rp_breaks       <- c(1/20, 1/10, 1/5, 1/2)
rp_break_labels <- c("1-in-20", "1-in-10", "1-in-5", "1-in-2")

# Load all countries; use each country's own wx_name
outcomes_all <- read_parquet(file.path(SIM_DIR, "outcomes.parquet")) |>
  filter(
    policy_label %in% all_policy_labels,
    scenario_id  %in% c("historical", FOCUS_SCENARIOS)
  )

# One wx_name per country (use whichever is present)
country_wx <- outcomes_all |>
  distinct(code, wx_name) |>
  slice(1, .by = code)

exceedance_curve <- function(x) {
  x_sorted <- sort(x, decreasing = TRUE)
  n        <- length(x_sorted)
  tibble(value = x_sorted, exceed_prob = seq_len(n) / (n + 1))
}

# 30 sim_years → Weibull positions 1/31 .. 30/31; match future interp to same range
n_years      <- 30
common_probs <- seq(1 / (n_years + 1), n_years / (n_years + 1), length.out = n_years)

make_curves <- function(poverty_agg) {
  outcomes_ep <- outcomes_all |>
    filter(agg_method == poverty_agg) |>
    semi_join(country_wx, by = c("code", "wx_name"))

  # Raw 30-point empirical curve — identical to 04a_climate_sim_results.R
  hist_curve <- outcomes_ep |>
    filter(scenario_id == "historical", policy_label == "no_policy") |>
    summarise(value = mean(value), .by = c(code, sim_year)) |>
    reframe(exceedance_curve(value), .by = code)

  fut_curve <- outcomes_ep |>
    filter(scenario_id %in% FOCUS_SCENARIOS) |>
    summarise(value = mean(value), .by = c(code, scenario_id, policy_label, ensemble_member, sim_year)) |>
    reframe(exceedance_curve(value), .by = c(code, scenario_id, policy_label, ensemble_member)) |>
    group_by(code, scenario_id, policy_label, ensemble_member) |>
    filter(n() >= 2) |>
    group_modify(\(df, .key) {
      tibble(
        exceed_prob = common_probs,
        value       = approx(df$exceed_prob, df$value, xout = common_probs, rule = 2)$y
      )
    }) |>
    ungroup() |>
    summarise(
      value_median = median(value),
      .by          = c(code, scenario_id, policy_label, exceed_prob)
    )

  list(hist = hist_curve, fut = fut_curve, codes = unique(outcomes_ep$code))
}

plot_exceedance <- function(country_code, hist_curve, fut_curve, poverty_label, poverty_y_label) {
  h      <- hist_curve |> filter(code == country_code, exceed_prob <= 0.5)
  s      <- fut_curve  |> filter(code == country_code, exceed_prob <= 0.5)
  combos <- s |> distinct(scenario_id, policy_label)

  line_layers <- pmap(combos, \(scenario_id, policy_label) {
    sd          <- filter(s, .data$scenario_id == .env$scenario_id, .data$policy_label == .env$policy_label)
    policy_nice <- if (policy_label == "no_policy") "No policy" else exceed_policy_labels[[policy_label]]
    lbl         <- paste0(policy_nice, " – ", SCENARIO_LABELS[[scenario_id]])
    geom_line(
      data      = sd,
      aes(x = exceed_prob, y = value_median, colour = lbl, linetype = lbl),
      linewidth = 0.8, inherit.aes = FALSE
    )
  })

  combo_labels  <- paste0(
    ifelse(combos$policy_label == "no_policy", "No policy", exceed_policy_labels[combos$policy_label]),
    " – ", SCENARIO_LABELS[combos$scenario_id]
  )
  colour_values <- c(
    "Historical (no policy)" = "#000000",
    setNames(exceed_policy_colours[combos$policy_label], combo_labels)
  )
  lty_values <- c(
    "Historical (no policy)" = "solid",
    setNames(exceed_scenario_lty[combos$scenario_id], combo_labels)
  )

  right_edge_prob <- max(s$exceed_prob)

  fut_labels <- s |>
    filter(exceed_prob == right_edge_prob) |>
    mutate(
      lbl    = paste0(
        ifelse(policy_label == "no_policy", "No policy", exceed_policy_labels[policy_label]),
        " – ", SCENARIO_LABELS[scenario_id]
      ),
      colour = exceed_policy_colours[policy_label]
    ) |>
    select(exceed_prob, y = value_median, lbl, colour)

  hist_label <- h |>
    filter(exceed_prob == max(exceed_prob)) |>
    mutate(lbl = "Historical (no policy)", colour = "#000000") |>
    select(exceed_prob, y = value, lbl, colour)

  label_data <- bind_rows(fut_labels, hist_label)

  ggplot() +
    line_layers +
    geom_line(
      data = h,
      aes(x = exceed_prob, y = value,
          colour = "Historical (no policy)", linetype = "Historical (no policy)"),
      linewidth = 0.9, inherit.aes = FALSE
    ) +
    geom_text_repel(
      data               = label_data,
      aes(x = exceed_prob, y = y, label = lbl, colour = lbl),
      direction          = "y",
      hjust              = 0,
      nudge_x            = 0.18,
      segment.size       = 0.3,
      segment.color      = "grey60",
      segment.curvature  = 0,
      size               = 3,
      box.padding        = 0.4,
      point.padding      = 0.2,
      force              = 2,
      force_pull         = 0.5,
      min.segment.length = 0,
      inherit.aes        = FALSE,
      show.legend        = FALSE
    ) +
    scale_x_log10(
      name   = "Event frequency (years)",
      breaks = rp_breaks,
      labels = rp_break_labels,
      expand = expansion(mult = c(0.01, 0.15))
    ) +
    scale_y_continuous(
      name   = poverty_y_label,
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    scale_colour_manual(name = NULL, values = colour_values) +
    scale_linetype_manual(name = NULL, values = lty_values) +
    coord_cartesian(clip = "off") +
    labs(title = paste0(country_code, ": Exceedance probability — Poverty (", poverty_label, "/day)")) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      plot.margin      = margin(5, 160, 5, 5)
    )
}

for (pl in POVERTY_LINES) {
  poverty_agg     <- paste0("headcount_ratio_", pl)
  poverty_label   <- paste0("$", formatC(pl / 100, format = "f", digits = 2))
  poverty_y_label <- paste0("Poverty rate (", poverty_label, "/day, 2021 PPP)")

  curves <- make_curves(poverty_agg)

  walk(curves$codes, \(cc) {
    p <- plot_exceedance(cc, curves$hist, curves$fut, poverty_label, poverty_y_label)
    ggsave(
      file.path(EXCEED_DIR, paste0("exceedance_", poverty_agg, "_", cc, ".png")),
      plot = p, width = 9, height = 5, dpi = 300
    )
  })
}
