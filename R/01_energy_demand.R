# =============================================================================
# 01_energy_demand.R
# US Data Centre Energy Demand — Layer 1
# Sources: LBNL 2024, IEA Energy and AI report, EIA
# =============================================================================

# --- Package setup -----------------------------------------------------------
if (!require("ggplot2"))   install.packages("ggplot2", repos="https://cloud.r-project.org")
if (!require("dplyr"))     install.packages("dplyr", repos="https://cloud.r-project.org")
if (!require("readr"))     install.packages("readr", repos="https://cloud.r-project.org")
if (!require("scales"))    install.packages("scales", repos="https://cloud.r-project.org")
if (!require("ggtext"))    install.packages("ggtext", repos="https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(ggtext)

message("=== 01_energy_demand.R starting ===")

# --- Consistent theme used across all scripts --------------------------------
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
      panel.grid.major = element_line(colour = "grey92"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = base_size - 1),
      plot.margin      = margin(16, 16, 16, 16)
    )
}

# Colour palette shared across project
pal_period   <- c("pre-AI (2014-2016)" = "#4393c3",
                  "early AI (2017-2020)" = "#f4a582",
                  "AI surge (2021-2023)" = "#d6604d")
pal_type     <- c("historical" = "#2166ac", "projected" = "#b2182b")
pal_income   <- c("High income"         = "#2166ac",
                  "Upper middle income" = "#74add1",
                  "Lower middle income" = "#fdae61",
                  "Low income"          = "#d73027")

# --- Load manual data --------------------------------------------------------
message("Loading manual CSVs...")

dc_twh   <- read_csv("data/manual/lbnl_datacenter_twh.csv",
                     show_col_types = FALSE)
us_elec  <- read_csv("data/manual/us_total_electricity_twh.csv",
                     show_col_types = FALSE)
water    <- read_csv("data/manual/water_consumption_billion_gallons.csv",
                     show_col_types = FALSE)

message(sprintf("  Loaded dc_twh: %d rows", nrow(dc_twh)))
message(sprintf("  Loaded us_elec: %d rows", nrow(us_elec)))
message(sprintf("  Loaded water: %d rows", nrow(water)))

# --- Compute derived columns -------------------------------------------------
# Join energy tables on year (only historical years have total_twh)
energy <- dc_twh %>%
  left_join(us_elec, by = "year") %>%
  mutate(
    datacenter_share_pct = twh_best / total_twh * 100,
    # Reference: Pakistan annual electricity consumption ≈ 180 TWh (IEA 2022)
    twh_equivalent = "Pakistan annual demand = 180 TWh"
  )

message("Computed datacenter_share_pct for historical years.")

# --- Save processed dataset --------------------------------------------------
write_csv(energy, "data/processed/energy_demand.csv")
message("Saved: data/processed/energy_demand.csv")

# ============================================================
# PLOT 1 — Energy trajectory 2014-2028
# ============================================================
message("Generating Plot 1: energy_trajectory.png")

hist_data <- energy %>% filter(type == "historical")
proj_data <- energy %>% filter(type == "projected")

# Build uncertainty ribbon only for projected years with bounds
ribbon_data <- proj_data %>%
  filter(!is.na(twh_low), !is.na(twh_high))

p1 <- ggplot(energy, aes(x = year, y = twh_best)) +
  # Uncertainty ribbon for projected years
  geom_ribbon(
    data = ribbon_data,
    aes(ymin = twh_low, ymax = twh_high),
    fill = "#d6604d", alpha = 0.18
  ) +
  # Historical line
  geom_line(data = hist_data, colour = "#2166ac", linewidth = 1.2) +
  geom_point(data = hist_data, colour = "#2166ac", size = 2.2) +
  # Projected line (dashed)
  geom_line(data = proj_data, colour = "#b2182b",
            linewidth = 1.2, linetype = "dashed") +
  geom_point(data = proj_data, colour = "#b2182b", size = 2.2) +
  # Reference line: Pakistan annual demand
  geom_hline(yintercept = 180, linetype = "dotted",
             colour = "#555555", linewidth = 0.9) +
  annotate("text", x = 2014.2, y = 186,
           label = "Pakistan annual demand (180 TWh)",
           hjust = 0, size = 3.4, colour = "#444444") +
  # Vertical separator: historical vs projected
  geom_vline(xintercept = 2023.5, linetype = "dashed",
             colour = "grey50", linewidth = 0.7) +
  annotate("text", x = 2023.7, y = 560,
           label = "Projected \u2192", hjust = 0,
           size = 3.4, colour = "grey45") +
  annotate("text", x = 2023.3, y = 560,
           label = "\u2190 Historical", hjust = 1,
           size = 3.4, colour = "grey45") +
  scale_x_continuous(breaks = seq(2014, 2028, 2)) +
  scale_y_continuous(labels = label_comma(suffix = " TWh"),
                     limits = c(0, 620)) +
  labs(
    title    = "US Data Centre Electricity Consumption 2014-2028",
    subtitle = "Historical figures (LBNL / IEA) with IEA and LBNL scenario projections",
    x        = NULL,
    y        = "Electricity Consumption (TWh)",
    caption  = "Sources: LBNL 2024, IEA Energy and AI 2024. Ribbon shows LBNL low-high range for 2028."
  ) +
  theme_carbon()

ggsave("outputs/plots/energy_trajectory.png", p1,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/energy_trajectory.png")

# ============================================================
# PLOT 2 — Data centre share of US electricity grid
# ============================================================
message("Generating Plot 2: share_of_grid.png")

share_data <- energy %>%
  filter(type == "historical", !is.na(datacenter_share_pct)) %>%
  mutate(
    period = case_when(
      year <= 2016 ~ "pre-AI (2014-2016)",
      year <= 2020 ~ "early AI (2017-2020)",
      TRUE         ~ "AI surge (2021-2023)"
    ),
    period = factor(period, levels = c("pre-AI (2014-2016)",
                                       "early AI (2017-2020)",
                                       "AI surge (2021-2023)"))
  )

p2 <- ggplot(share_data, aes(x = year, y = datacenter_share_pct,
                              fill = period)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  # Annotate the 2023 bar
  annotate("text", x = 2023, y = share_data$datacenter_share_pct[share_data$year == 2023] + 0.12,
           label = "4.4% of US electricity",
           hjust = 0.5, size = 3.3, fontface = "bold", colour = "#b2182b") +
  scale_fill_manual(values = pal_period, name = "Era") +
  scale_x_continuous(breaks = seq(2014, 2023, 1)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1),
                     limits = c(0, 5.5)) +
  labs(
    title    = "Data Centres as Share of US Electricity Consumption",
    subtitle = "Pre-AI growth was moderate; the AI surge has sharply increased demand",
    x        = NULL,
    y        = "Share of Total US Electricity (%)",
    caption  = "Sources: LBNL 2024, EIA Electric Power Monthly 2024."
  ) +
  theme_carbon() +
  theme(legend.position = "top")

ggsave("outputs/plots/share_of_grid.png", p2,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/share_of_grid.png")

# ============================================================
# PLOT 3 — Water consumption
# ============================================================
message("Generating Plot 3: water_consumption.png")

water_plot <- water %>%
  mutate(is_projected = type != "historical")

p3 <- ggplot(water_plot, aes(x = year, y = water_billion_gallons,
                              fill = is_projected)) +
  geom_col(width = ifelse(water_plot$year == 2028, 1.8, 0.75),
           colour = "white", linewidth = 0.3) +
  # Annotate the 2023 bar
  annotate("text", x = 2022.4, y = 17.6,
           label = "17 billion gallons in 2023\n\u2248 26 million Olympic pools",
           hjust = 0.5, size = 3.2, colour = "#333333") +
  scale_fill_manual(values = c("FALSE" = "#4393c3", "TRUE" = "#d6604d"),
                    labels = c("Historical", "Projected midpoint (2028)"),
                    name   = NULL) +
  scale_x_continuous(breaks = c(seq(2014, 2023, 1), 2028)) +
  scale_y_continuous(labels = label_comma(suffix = " bn gal")) +
  labs(
    title    = "US Data Centre Direct Water Consumption",
    subtitle = "Cooling towers, evaporation, and on-site water use",
    x        = NULL,
    y        = "Water Consumption (billion gallons)",
    caption  = "Source: LBNL 2024 United States Data Center Energy Usage Report."
  ) +
  theme_carbon() +
  theme(legend.position = "top")

ggsave("outputs/plots/water_consumption.png", p3,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/water_consumption.png")

# --- Summary -----------------------------------------------------------------
message("\n=== 01_energy_demand.R complete ===")
message("Outputs saved:")
message("  data/processed/energy_demand.csv")
message("  outputs/plots/energy_trajectory.png")
message("  outputs/plots/share_of_grid.png")
message("  outputs/plots/water_consumption.png")
