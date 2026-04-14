# =============================================================================
# 04_climate_indicators.R
# Climate Shift Indicators — Layer 3B
# Sources: NOAA Global Temperature Anomaly; Our World in Data CO2
# =============================================================================

if (!require("ggplot2"))  install.packages("ggplot2", repos="https://cloud.r-project.org")
if (!require("dplyr"))    install.packages("dplyr", repos="https://cloud.r-project.org")
if (!require("readr"))    install.packages("readr", repos="https://cloud.r-project.org")
if (!require("scales"))   install.packages("scales", repos="https://cloud.r-project.org")
if (!require("tidyr"))    install.packages("tidyr", repos="https://cloud.r-project.org")
if (!require("ggrepel"))  install.packages("ggrepel", repos="https://cloud.r-project.org")
if (!require("mgcv"))     install.packages("mgcv", repos="https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(tidyr)
library(ggrepel)
library(mgcv)

message("=== 04_climate_indicators.R starting ===")

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
      panel.grid.major = element_line(colour = "grey92"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = base_size - 1),
      plot.margin      = margin(16, 16, 16, 16)
    )
}

# --- Download NOAA temperature anomaly data ----------------------------------
# NOAA Climate at a Glance: global land+ocean annual temperature anomaly
# vs 1901-2000 baseline

noaa_url   <- paste0(
  "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
  "global/time-series/globe/land_ocean/ann/0/1850-2024.csv"
)
noaa_local <- "data/raw/noaa_temp_anomaly.csv"

message("Downloading NOAA global temperature anomaly data...")
tryCatch({
  download.file(noaa_url, noaa_local, quiet = TRUE)
  message("  Download successful.")
}, error = function(e) {
  message(sprintf("  WARNING: Download failed (%s).", conditionMessage(e)))
})

# NOAA file has several header comment lines before the data
# Try to detect and skip them
parse_noaa <- function(path) {
  # Read raw lines to find where data starts
  raw_lines <- readLines(path, warn = FALSE)
  # Data starts after the line containing "Year,Value" or similar
  data_start <- which(grepl("^Year|^year", raw_lines))[1]
  if (is.na(data_start)) {
    # Try skipping first 4 rows (typical NOAA format)
    data_start <- 5
  }
  read_csv(path, skip = data_start - 1, col_names = TRUE,
           show_col_types = FALSE)
}

temp_df <- NULL

if (file.exists(noaa_local)) {
  tryCatch({
    temp_df <- parse_noaa(noaa_local)
    # Normalise column names
    names(temp_df) <- tolower(names(temp_df))
    names(temp_df)[1] <- "year"
    names(temp_df)[2] <- "anomaly"
    temp_df <- temp_df %>%
      mutate(year = as.integer(year),
             anomaly = as.numeric(anomaly)) %>%
      filter(!is.na(year), !is.na(anomaly), year >= 1850)
    message(sprintf("  Parsed NOAA data: %d rows (%d–%d)",
                    nrow(temp_df), min(temp_df$year), max(temp_df$year)))
  }, error = function(e) {
    message(sprintf("  WARNING: Parse failed (%s). Using hardcoded fallback.",
                    conditionMessage(e)))
    temp_df <<- NULL
  })
}

# Fallback: hardcoded NOAA global land+ocean temperature anomaly 1950-2023
# (vs 1901-2000 baseline, degrees C)
if (is.null(temp_df) || nrow(temp_df) == 0) {
  message("  Using hardcoded NOAA fallback data (1950-2023).")
  temp_df <- data.frame(
    year    = 1950:2023,
    anomaly = c(
      -0.16, -0.01, 0.02, -0.03, -0.05, -0.08, -0.14, -0.07, 0.06, 0.05, # 1950-59
       0.06,  0.06, 0.03, -0.03, -0.11, -0.07, -0.06, -0.04, 0.08, 0.11, # 1960-69
       0.08,  0.20, 0.02,  0.17,  0.07, -0.02, -0.11, 0.17,  0.07, 0.17, # 1970-79
       0.26,  0.32, 0.14,  0.32,  0.31,  0.14,  0.18, 0.34,  0.40, 0.29, # 1980-89
       0.44,  0.41, 0.23,  0.24,  0.31,  0.38,  0.33, 0.40,  0.61, 0.40, # 1990-99
       0.42,  0.54, 0.56,  0.62,  0.54,  0.68,  0.61, 0.62,  0.54, 0.64, # 2000-09
       0.72,  0.61, 0.65,  0.68,  0.75,  0.87,  1.00, 0.92,  0.85, 0.98, # 2010-19
       1.02,  0.85, 0.89,  1.17                                           # 2020-23
    )
  )
}

# Filter to 1950+ for the primary chart
temp_1950 <- temp_df %>% filter(year >= 1950)

# --- Save processed temperature data -----------------------------------------
write_csv(temp_1950, "data/processed/noaa_temp_anomaly.csv")
message("Saved: data/processed/noaa_temp_anomaly.csv")

# ============================================================
# PLOT 8 — Temperature anomaly 1950-2023
# ============================================================
message("Generating Plot 8: temperature_anomaly.png")

# Colour segments by anomaly value (blue < 0, yellow near 0, red > 0.5)
p8 <- ggplot(temp_1950, aes(x = year, y = anomaly)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.8) +
  geom_col(aes(fill = anomaly), width = 0.85) +
  geom_smooth(method = "loess", span = 0.3, colour = "#333333",
              linewidth = 1.2, se = FALSE) +
  # Annotate recent exceptional years
  annotate("text", x = 2016, y = 1.05,
           label = "2016: +1.00°C\n(El Niño year)", size = 3.0,
           colour = "#7f0000", hjust = 0.5) +
  annotate("text", x = 2023, y = 1.25,
           label = "2023: +1.17°C", size = 3.0,
           colour = "#7f0000", hjust = 0.5) +
  scale_fill_gradient2(
    low      = "#4575b4",
    mid      = "#ffffbf",
    high     = "#d73027",
    midpoint = 0.4,
    name     = "Anomaly (°C)",
    limits   = c(-0.3, 1.3)
  ) +
  scale_x_continuous(breaks = seq(1950, 2023, 10)) +
  scale_y_continuous(labels = label_number(suffix = "°C", accuracy = 0.1)) +
  labs(
    title    = "Global Mean Temperature Anomaly 1950-2023 (vs 1901-2000 baseline)",
    subtitle = "Every year since 1977 has been warmer than the 20th-century average",
    x        = NULL,
    y        = "Temperature Anomaly (°C)",
    caption  = "Source: NOAA National Centers for Environmental Information, Climate at a Glance (2024)."
  ) +
  theme_carbon()

ggsave("outputs/plots/temperature_anomaly.png", p8,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/temperature_anomaly.png")

# ============================================================
# PLOT 9 — Cumulative emissions vs temperature anomaly
# ============================================================
message("Generating Plot 9: emissions_vs_temperature.png")

# Load OWID CO2 data (downloaded in script 03)
owid_local <- "data/raw/owid_co2.csv"
if (!file.exists(owid_local)) {
  stop("OWID CO2 file missing. Run 03_global_emissions.R first.")
}

co2_world <- read_csv(owid_local, show_col_types = FALSE) %>%
  filter(country == "World", year >= 1950, year <= 2022) %>%
  select(year, cumulative_co2) %>%
  filter(!is.na(cumulative_co2))

# Join with temperature
scatter_df <- inner_join(co2_world, temp_1950, by = "year") %>%
  mutate(
    decade = paste0(floor(year / 10) * 10, "s"),
    decade = factor(decade, levels = c("1950s","1960s","1970s",
                                       "1980s","1990s","2000s",
                                       "2010s","2020s"))
  )

decade_palette <- c(
  "1950s" = "#313695", "1960s" = "#4575b4", "1970s" = "#74add1",
  "1980s" = "#abd9e9", "1990s" = "#fdae61", "2000s" = "#f46d43",
  "2010s" = "#d73027", "2020s" = "#a50026"
)

# Label a selection of years
label_years <- c(1960, 1970, 1980, 1990, 2000, 2010, 2015, 2020, 2022)
label_df <- scatter_df %>% filter(year %in% label_years)

p9 <- ggplot(scatter_df, aes(x = cumulative_co2, y = anomaly,
                               colour = decade)) +
  geom_smooth(method = "loess", span = 0.6, colour = "#333333",
              linewidth = 1.2, se = TRUE, fill = "grey85", alpha = 0.5) +
  geom_point(size = 2.8, alpha = 0.85) +
  geom_label_repel(
    data = label_df,
    aes(label = year),
    size = 3.0, box.padding = 0.3, point.padding = 0.3,
    max.overlaps = 20, seed = 42,
    show.legend = FALSE, label.size = 0.2
  ) +
  scale_colour_manual(values = decade_palette, name = "Decade") +
  scale_x_continuous(labels = label_comma(suffix = " Gt",
                                           scale = 1 / 1000)) +
  scale_y_continuous(labels = label_number(suffix = "°C", accuracy = 0.1)) +
  labs(
    title    = "Cumulative Emissions and Temperature: A Clear Relationship",
    subtitle = "As cumulative CO2 rises, global temperature follows — a structural link, not coincidence",
    x        = "Cumulative Global CO2 Emissions (Gt)",
    y        = "Temperature Anomaly vs 1901-2000 (°C)",
    caption  = paste0(
      "Sources: Our World in Data CO2 dataset (cumulative global CO2); ",
      "NOAA Climate at a Glance (temperature anomaly)."
    )
  ) +
  theme_carbon()

ggsave("outputs/plots/emissions_vs_temperature.png", p9,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/emissions_vs_temperature.png")

# --- Summary -----------------------------------------------------------------
message("\n=== 04_climate_indicators.R complete ===")
message("Outputs saved:")
message("  data/processed/noaa_temp_anomaly.csv")
message("  outputs/plots/temperature_anomaly.png")
message("  outputs/plots/emissions_vs_temperature.png")
message(sprintf("  Most recent anomaly: %.2f°C in %d",
                tail(temp_1950$anomaly, 1), tail(temp_1950$year, 1)))
