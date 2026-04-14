# =============================================================================
# 03_global_emissions.R
# Global Emissions Context — Layer 3A
# Source: Our World in Data CO2 dataset
# =============================================================================

if (!require("ggplot2"))  install.packages("ggplot2", repos="https://cloud.r-project.org")
if (!require("dplyr"))    install.packages("dplyr", repos="https://cloud.r-project.org")
if (!require("readr"))    install.packages("readr", repos="https://cloud.r-project.org")
if (!require("scales"))   install.packages("scales", repos="https://cloud.r-project.org")
if (!require("tidyr"))    install.packages("tidyr", repos="https://cloud.r-project.org")
if (!require("ggrepel"))  install.packages("ggrepel", repos="https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(tidyr)
library(ggrepel)

message("=== 03_global_emissions.R starting ===")

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

# --- Download Our World in Data CO2 dataset ----------------------------------
owid_url <- "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
owid_local <- "data/raw/owid_co2.csv"

message("Downloading OWID CO2 dataset...")
tryCatch({
  download.file(owid_url, owid_local, quiet = TRUE)
  message("  Download successful.")
}, error = function(e) {
  message(sprintf("  WARNING: Download failed (%s). Using cached copy if available.",
                  conditionMessage(e)))
})

if (!file.exists(owid_local)) {
  stop("OWID CO2 data not available. Check internet connection or place owid-co2-data.csv in data/raw/.")
}

co2_raw <- read_csv(owid_local, show_col_types = FALSE)
message(sprintf("  Loaded OWID CO2: %d rows, %d columns", nrow(co2_raw), ncol(co2_raw)))

# --- Filter to key entities and years ----------------------------------------
key_entities <- c("United States", "China", "India",
                  "European Union (27)", "World")

co2_key <- co2_raw %>%
  filter(country %in% key_entities, year >= 2000, year <= 2022) %>%
  select(country, year, co2, co2_per_capita, share_global_co2,
         cumulative_co2, population) %>%
  filter(!is.na(co2))

message(sprintf("  Filtered to %d rows for key entities 2000-2022.", nrow(co2_key)))

# --- Add US data centre emissions estimate -----------------------------------
# US data centre CO2 = twh_best * avg_us_grid_carbon_intensity
# US average grid intensity: 0.386 kg CO2 per kWh (EPA 2022)
# datacenter_co2_mt = TWh * 1e9 kWh/TWh * 0.386 kg/kWh / 1000 kg/t / 1e6 t/Mt
# = TWh * 0.386 * 1e9 / 1e9 Mt = TWh * 0.386 Mt/TWh ... actually:
# TWh * 1e9 kWh/TWh * 0.386 kgCO2/kWh = kg CO2 * 0.386e9
# convert to million metric tons: / 1e3 (kg→t) / 1e6 (t→Mt) = / 1e9
# => datacenter_co2_Mt = TWh * 0.386

dc_twh_hist <- data.frame(
  year    = 2014:2022,
  twh_best = c(58, 60, 60, 65, 76, 95, 108, 120, 134)
)

dc_co2 <- dc_twh_hist %>%
  mutate(
    datacenter_co2_mt = twh_best * 0.386,   # million metric tons CO2
    country = "US Data Centres (est.)"
  )

message(sprintf("  US data centre CO2 in 2022: ~%.1f Mmt (from %.0f TWh)",
                dc_co2$datacenter_co2_mt[dc_co2$year == 2022],
                dc_co2$twh_best[dc_co2$year == 2022]))

# Save processed global emissions
us_total_co2 <- co2_key %>%
  filter(country == "United States") %>%
  select(year, us_total_co2 = co2)

dc_with_share <- dc_co2 %>%
  left_join(us_total_co2, by = "year") %>%
  mutate(dc_share_of_us = datacenter_co2_mt / us_total_co2 * 100)

write_csv(co2_key, "data/processed/owid_co2_key.csv")
write_csv(dc_with_share, "data/processed/datacenter_co2_context.csv")
message("Saved: data/processed/owid_co2_key.csv")
message("Saved: data/processed/datacenter_co2_context.csv")

# ============================================================
# PLOT 6 — Global CO2 emissions trajectory
# ============================================================
message("Generating Plot 6: global_co2_context.png")

# Colour palette for countries
country_colours <- c(
  "World"                   = "#333333",
  "China"                   = "#d73027",
  "United States"           = "#4393c3",
  "European Union (27)"     = "#1a9850",
  "India"                   = "#f4a582"
)

# End-point labels for cleaner chart
label_data <- co2_key %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

p6 <- ggplot(co2_key, aes(x = year, y = co2, colour = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = label_data, size = 2.5) +
  geom_label_repel(
    data = label_data,
    aes(label = paste0(country, "\n", round(co2 / 1000, 1), " Gt")),
    size = 3.0, nudge_x = 0.5, hjust = 0,
    box.padding = 0.3, point.padding = 0.3,
    max.overlaps = 20, seed = 42,
    show.legend = FALSE, label.size = 0.2
  ) +
  scale_colour_manual(values = country_colours, name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2022, 4),
                     limits = c(2000, 2026)) +
  scale_y_continuous(labels = label_comma(suffix = " Mt"),
                     name   = "CO2 Emissions (million tonnes)") +
  labs(
    title    = "Global CO2 Emissions Trajectory 2000-2022",
    subtitle = "China's rapid industrialisation dominates global growth; US remains second-highest emitter",
    x        = NULL,
    caption  = "Source: Our World in Data CO2 and Greenhouse Gas Emissions dataset (2024)."
  ) +
  theme_carbon() +
  theme(legend.position = "none")

ggsave("outputs/plots/global_co2_context.png", p6,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/global_co2_context.png")

# ============================================================
# PLOT 7 — Data centre share in nested global context
# ============================================================
message("Generating Plot 7: datacenter_share_emissions.png")

# Build a nested contribution dataset for 2022
world_co2_2022 <- co2_key %>%
  filter(country == "World", year == 2022) %>%
  pull(co2)

us_co2_2022 <- co2_key %>%
  filter(country == "United States", year == 2022) %>%
  pull(co2)

dc_co2_2022 <- dc_with_share %>%
  filter(year == 2022) %>%
  pull(datacenter_co2_mt)

# Stacked perspective: share of world -> share of US -> DC share
share_df <- data.frame(
  level       = c("World total", "US share of world", "US DC share of US"),
  value_mt    = c(world_co2_2022,  us_co2_2022, dc_co2_2022),
  pct_of_world = c(100,
                   us_co2_2022 / world_co2_2022 * 100,
                   dc_co2_2022 / world_co2_2022 * 100),
  fill_col    = c("#bbbbbb", "#4393c3", "#d73027"),
  stringsAsFactors = FALSE
) %>%
  mutate(level = factor(level, levels = c("World total",
                                          "US share of world",
                                          "US DC share of US")))

# Time-series version: US data centre % of US total over time
dc_ts <- dc_with_share %>%
  filter(!is.na(dc_share_of_us)) %>%
  select(year, datacenter_co2_mt, us_total_co2, dc_share_of_us)

p7 <- ggplot(dc_ts, aes(x = year, y = dc_share_of_us)) +
  geom_col(fill = "#d73027", alpha = 0.75, width = 0.7) +
  geom_line(colour = "#b2182b", linewidth = 1.1) +
  geom_point(colour = "#b2182b", size = 2.5) +
  # Annotation for nested context
  annotate("text", x = 2015, y = max(dc_ts$dc_share_of_us) * 0.75,
           label = sprintf(
             "In 2022:\nUS total = %.1f Gt\n(%.1f%% of global CO2)\nDC share = %.1f Mmt\n(%.1f%% of US total)",
             us_co2_2022 / 1000,
             us_co2_2022 / world_co2_2022 * 100,
             dc_co2_2022,
             dc_co2_2022 / us_co2_2022 * 100
           ),
           hjust = 0, vjust = 1, size = 3.4,
           colour = "#333333",
           box.colour = "grey70") +
  scale_x_continuous(breaks = 2014:2022) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1)) +
  labs(
    title    = "Where Data Centre Emissions Sit in Global Context",
    subtitle = "US data centre CO2 as % of total US emissions — AI-era surge visible post-2020",
    x        = NULL,
    y        = "DC CO2 as % of US Total",
    caption  = paste0(
      "Sources: Our World in Data CO2 dataset; LBNL 2024; EPA grid intensity 0.386 kgCO2/kWh.\n",
      "Note: Data centre CO2 estimated from electricity consumption x average US grid intensity."
    )
  ) +
  theme_carbon()

ggsave("outputs/plots/datacenter_share_emissions.png", p7,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/datacenter_share_emissions.png")

# --- Summary -----------------------------------------------------------------
message("\n=== 03_global_emissions.R complete ===")
message("Outputs saved:")
message("  data/processed/owid_co2_key.csv")
message("  data/processed/datacenter_co2_context.csv")
message("  outputs/plots/global_co2_context.png")
message("  outputs/plots/datacenter_share_emissions.png")
message(sprintf("  US data centre CO2 in 2022: ~%.1f Mmt (%.1f%% of US total)",
                dc_co2_2022,
                dc_co2_2022 / us_co2_2022 * 100))
