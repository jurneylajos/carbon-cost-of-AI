# =============================================================================
# 06_equity_viz.R
# The Centrepiece — Equity Visualisation
# Who Bears the Climate Health Burden?
# =============================================================================

if (!require("ggplot2"))  install.packages("ggplot2", repos="https://cloud.r-project.org")
if (!require("dplyr"))    install.packages("dplyr", repos="https://cloud.r-project.org")
if (!require("readr"))    install.packages("readr", repos="https://cloud.r-project.org")
if (!require("scales"))   install.packages("scales", repos="https://cloud.r-project.org")
if (!require("ggrepel"))  install.packages("ggrepel", repos="https://cloud.r-project.org")
if (!require("forcats"))  install.packages("forcats", repos="https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(ggrepel)
library(forcats)

message("=== 06_equity_viz.R starting ===")

# --- Consistent theme --------------------------------------------------------
theme_carbon <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 3,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(colour = "grey40", size = base_size - 1,
                                      margin = margin(b = 10)),
      plot.caption     = element_text(colour = "grey55", size = base_size - 3,
                                      hjust = 0),
      axis.title       = element_text(colour = "grey30", size = base_size - 1),
      axis.text        = element_text(colour = "grey40"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.4),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = base_size - 1),
      plot.margin      = margin(16, 16, 16, 16)
    )
}

pal_income <- c(
  "High income"          = "#2166ac",
  "Upper middle income"  = "#74add1",
  "Lower middle income"  = "#fdae61",
  "Low income"           = "#d73027"
)

# --- Load processed health equity data ---------------------------------------
health_path <- "data/processed/health_equity.csv"
if (!file.exists(health_path)) {
  stop("health_equity.csv not found. Run 05_health_burden.R first.")
}

health_df <- read_csv(health_path, show_col_types = FALSE) %>%
  mutate(
    income_group = factor(income_group,
                          levels = c("High income", "Upper middle income",
                                     "Lower middle income", "Low income"))
  )

message(sprintf("Loaded health equity data: %d countries.", nrow(health_df)))

# Countries to label on the centrepiece scatter
label_countries <- c(
  "United States", "China", "India", "Nigeria",
  "Bangladesh", "Chad", "Mozambique", "South Africa",
  "Germany", "Pakistan"
)

label_df <- health_df %>%
  filter(country %in% label_countries, income_group != "Other")

plot_df <- health_df %>%
  filter(income_group != "Other", !is.na(co2_per_capita),
         !is.na(air_pollution_deaths_per_100k), co2_per_capita > 0)

# ============================================================
# PLOT 11 — THE CENTREPIECE: equity scatter
# ============================================================
message("Generating Plot 11 (centrepiece): equity_scatter.png")

# Compute quadrant midpoints for annotations (log scale)
x_mid <- median(log10(plot_df$co2_per_capita), na.rm = TRUE)
y_mid <- median(plot_df$air_pollution_deaths_per_100k, na.rm = TRUE)

x_range  <- range(log10(plot_df$co2_per_capita[plot_df$co2_per_capita > 0]),
                  na.rm = TRUE)
y_range  <- range(plot_df$air_pollution_deaths_per_100k, na.rm = TRUE)

# Quadrant annotation positions (in data space)
x_low  <- 10^(x_range[1] + 0.12 * diff(x_range))
x_high <- 10^(x_range[1] + 0.75 * diff(x_range))
y_high <- y_range[1] + 0.88 * diff(y_range)
y_low  <- y_range[1] + 0.08 * diff(y_range)

p11 <- ggplot(plot_df,
              aes(x = co2_per_capita,
                  y = air_pollution_deaths_per_100k)) +
  # Loess smoother per income group (underneath points)
  geom_smooth(aes(colour = income_group, fill = income_group),
              method = "loess", span = 0.9, se = TRUE, alpha = 0.12,
              linewidth = 0.8, show.legend = FALSE) +
  # All points: size by population
  geom_point(aes(colour = income_group,
                 size   = population / 1e6),
             alpha = 0.7, stroke = 0.3) +
  # Highlight labelled countries
  geom_point(data = label_df,
             aes(colour = income_group, size = population / 1e6),
             alpha = 1, stroke = 0.8, shape = 21,
             fill  = NA) +
  # Labels with ggrepel
  geom_label_repel(
    data          = label_df,
    aes(label     = country, colour = income_group),
    size          = 4.2,
    fontface      = "bold",
    box.padding   = 1.0,
    point.padding = 0.8,
    force         = 3,
    force_pull    = 0.5,
    max.overlaps  = Inf,
    seed          = 2024,
    show.legend   = FALSE,
    label.size    = 0.25,
    fill          = alpha("white", 0.85)
  ) +
  # Reference lines at medians
  geom_vline(xintercept = 10^x_mid, linetype = "dashed",
             colour = "grey60", linewidth = 0.6) +
  geom_hline(yintercept = y_mid, linetype = "dashed",
             colour = "grey60", linewidth = 0.6) +
  # Scales
  scale_x_log10(
    labels = label_number(suffix = " t", accuracy = 0.1),
    name   = "CO2 Emissions per Capita (tonnes, log scale)"
  ) +
  scale_y_continuous(
    labels = label_comma(),
    name   = "Air Pollution Deaths per 100,000 population (age-standardised, ~2019)"
  ) +
  scale_colour_manual(values = pal_income, name = "Income group",
                      drop = TRUE) +
  scale_fill_manual(values  = pal_income, name = "Income group",
                    drop = TRUE, guide = "none") +
  scale_size_continuous(
    range  = c(1.5, 14),
    name   = "Population (millions)",
    breaks = c(10, 100, 500, 1000, 1500),
    labels = label_comma()
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 4)),
    size   = guide_legend(order = 2)
  ) +
  labs(
    title    = "Who Bears the Climate Health Burden?",
    subtitle = paste0(
      "Countries with the lowest per-capita CO2 emissions bear the greatest ",
      "air pollution mortality — CO2 data is economy-wide; AI compute is the ",
      "framing context, not the measured cause"
    ),
    caption  = paste0(
      "Sources: State of Global Air / Health Effects Institute (ambient PM2.5 mortality, 2015); ",
      "Our World in Data CO2 dataset (CO2 per capita, most recent year ≤ 2022).\n",
      "PM2.5 = outdoor particulate matter, more directly tied to fossil fuel combustion than combined indoor+outdoor measures. ",
      "Shaded bands show loess smoothers per income group (95% CI)."
    )
  ) +
  theme_carbon(base_size = 14) +
  theme(
    legend.box       = "horizontal",
    legend.position  = "bottom",
    legend.text      = element_text(size = 13),
    legend.title     = element_text(face = "bold", size = 14),
    legend.key.size  = unit(0.55, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    legend.margin    = margin(t = 8),
    plot.title       = element_text(face = "bold", size = 20,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(colour = "grey40", size = 14,
                                    margin = margin(b = 10)),
    plot.margin      = margin(20, 24, 20, 20)
  )

ggsave("outputs/plots/equity_scatter.png", p11,
       width = 3000 / 150, height = 1800 / 150, dpi = 150)
message("Saved: outputs/plots/equity_scatter.png")

# ============================================================
# PLOT 12 — Income group summary: the emissions-health paradox
# ============================================================
message("Generating Plot 12: income_group_summary.png")

# Compute group means — filter out "Other"
group_summary <- health_df %>%
  filter(income_group != "Other") %>%
  group_by(income_group) %>%
  summarise(
    mean_co2_per_capita      = mean(co2_per_capita, na.rm = TRUE),
    mean_air_deaths_per_100k = mean(air_pollution_deaths_per_100k, na.rm = TRUE),
    n                        = n(),
    .groups = "drop"
  ) %>%
  mutate(income_group = fct_reorder(income_group, mean_co2_per_capita))

# Normalise both metrics to 0-100 for dual display on same axis
max_co2    <- max(group_summary$mean_co2_per_capita)
max_deaths <- max(group_summary$mean_air_deaths_per_100k)

long_summary <- group_summary %>%
  mutate(
    co2_normalised    = mean_co2_per_capita / max_co2 * 100,
    deaths_normalised = mean_air_deaths_per_100k / max_deaths * 100
  ) %>%
  tidyr::pivot_longer(
    cols    = c(co2_normalised, deaths_normalised),
    names_to  = "metric",
    values_to = "normalised_value"
  ) %>%
  mutate(
    metric = recode(metric,
                    co2_normalised    = "CO2 per capita\n(normalised)",
                    deaths_normalised = "Air pollution deaths\nper 100k (normalised)")
  )

p12 <- ggplot(long_summary,
              aes(x = income_group, y = normalised_value,
                  fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65,
           colour = "white", linewidth = 0.3) +
  geom_text(aes(label = round(normalised_value, 1)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3.1, colour = "grey30") +
  # Actual value annotations on CO2 bars
  geom_text(
    data = group_summary %>%
      mutate(metric = "CO2 per capita\n(normalised)",
             normalised_value = mean_co2_per_capita / max_co2 * 100),
    aes(x = income_group, y = normalised_value,
        label = paste0(round(mean_co2_per_capita, 1), " t")),
    position = position_dodge(width = 0.75),
    vjust = 1.6, size = 2.6, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("CO2 per capita\n(normalised)"       = "#4393c3",
               "Air pollution deaths\nper 100k (normalised)" = "#d73027"),
    name = NULL
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    limits = c(0, 115)
  ) +
  labs(
    title    = "The Emissions-Health Paradox Across Income Groups",
    subtitle = paste0(
      "Both metrics normalised to 0-100. High-income groups emit more; ",
      "low-income groups bear more deaths."
    ),
    x        = "Income Group",
    y        = "Normalised Score (0-100)",
    caption  = paste0(
      "Sources: State of Global Air / HEI (ambient PM2.5 mortality, 2015); ",
      "Our World in Data CO2 dataset (CO2 per capita, most recent year ≤ 2022).\n",
      "Values show group means. White labels show actual CO2 per capita (tonnes)."
    )
  ) +
  theme_carbon() +
  theme(legend.position = "top")

ggsave("outputs/plots/income_group_summary.png", p12,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/income_group_summary.png")

# --- Print equity finding for README -----------------------------------------
hi_co2  <- group_summary$mean_co2_per_capita[group_summary$income_group == "High income"]
lo_co2  <- group_summary$mean_co2_per_capita[group_summary$income_group == "Low income"]
hi_mort <- group_summary$mean_air_deaths_per_100k[group_summary$income_group == "High income"]
lo_mort <- group_summary$mean_air_deaths_per_100k[group_summary$income_group == "Low income"]

message(sprintf(
  "\nEquity finding: High-income countries average %.1f t CO2/capita (%.1fx low-income)",
  hi_co2, hi_co2 / lo_co2))
message(sprintf(
  "  Low-income countries bear %.1f deaths/100k (%.1fx high-income burden)",
  lo_mort, lo_mort / hi_mort))

# ============================================================
# PLOT 13 — Small multiples: equity scatter 2015 vs most recent
# ============================================================
multiyear_path <- "data/processed/health_equity_multiyear.csv"

if (file.exists(multiyear_path)) {
  message("Generating Plot 13: equity_scatter_multiyear.png")

  health_multi <- read_csv(multiyear_path, show_col_types = FALSE) %>%
    filter(income_group != "Other", !is.na(co2_per_capita),
           !is.na(air_pollution_deaths_per_100k), co2_per_capita > 0) %>%
    mutate(
      income_group = factor(income_group,
                            levels = c("High income", "Upper middle income",
                                       "Lower middle income", "Low income")),
      year_label   = factor(year_label, levels = sort(unique(year_label)))
    )

  if (nrow(health_multi) > 10) {
    p13 <- ggplot(health_multi,
                  aes(x = co2_per_capita, y = air_pollution_deaths_per_100k)) +
      geom_smooth(aes(colour = income_group, fill = income_group),
                  method = "loess", span = 0.9, se = TRUE, alpha = 0.12,
                  linewidth = 0.8, show.legend = FALSE) +
      geom_point(aes(colour = income_group, size = population / 1e6),
                 alpha = 0.65, stroke = 0.3) +
      scale_x_log10(
        labels = label_number(suffix = " t", accuracy = 0.1),
        name   = "CO2 Emissions per Capita (tonnes, log scale)"
      ) +
      scale_y_continuous(
        labels = label_comma(),
        name   = "Air Pollution Deaths per 100,000"
      ) +
      scale_colour_manual(values = pal_income, name = "Income group", drop = TRUE) +
      scale_fill_manual(values   = pal_income, drop = TRUE, guide = "none") +
      scale_size_continuous(range = c(1.5, 10), name = "Population (millions)",
                            breaks = c(10, 100, 500, 1000),
                            labels = label_comma()) +
      facet_wrap(~year_label, ncol = 2) +
      labs(
        title    = "The Emissions–Health Burden Gap: 2000 vs 2015",
        subtitle = paste0("Persistent pattern across a 15-year window indicates structural, ",
                          "not transitory, inequality"),
        caption  = paste0(
          "Sources: State of Global Air / HEI (ambient PM2.5 mortality, 2000 and 2015); ",
          "Our World in Data CO2 dataset (CO2 per capita, matching years).\n",
          "CO2 data is economy-wide, not AI-specific. ",
          "Shaded bands show loess smoothers per income group (95% CI)."
        )
      ) +
      theme_carbon(base_size = 13) +
      theme(
        strip.text       = element_text(face = "bold", size = 13),
        legend.position  = "bottom",
        legend.box       = "horizontal"
      )

    ggsave("outputs/plots/equity_scatter_multiyear.png", p13,
           width = 3000 / 150, height = 1400 / 150, dpi = 150)
    message("Saved: outputs/plots/equity_scatter_multiyear.png")
  } else {
    message("  Skipping Plot 13 — insufficient multi-year data.")
  }
} else {
  message("  Skipping Plot 13 — health_equity_multiyear.csv not found (run 05_health_burden.R with internet access).")
}

# --- Summary -----------------------------------------------------------------
message("\n=== 06_equity_viz.R complete ===")
message("Outputs saved:")
message("  outputs/plots/equity_scatter.png         (CENTREPIECE)")
message("  outputs/plots/income_group_summary.png")
message("  outputs/plots/equity_scatter_multiyear.png (if multi-year data available)")
message("\n=== ALL SCRIPTS COMPLETE ===")
