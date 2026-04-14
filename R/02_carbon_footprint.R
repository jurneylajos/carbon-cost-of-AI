# =============================================================================
# 02_carbon_footprint.R
# Carbon Footprint by US Region — Layer 2
# Source: EPA eGRID 2022 (hardcoded from summary tables)
# =============================================================================

if (!require("ggplot2"))  install.packages("ggplot2", repos="https://cloud.r-project.org")
if (!require("dplyr"))    install.packages("dplyr", repos="https://cloud.r-project.org")
if (!require("readr"))    install.packages("readr", repos="https://cloud.r-project.org")
if (!require("maps"))     install.packages("maps", repos="https://cloud.r-project.org")
if (!require("ggrepel"))  install.packages("ggrepel", repos="https://cloud.r-project.org")
if (!require("scales"))   install.packages("scales", repos="https://cloud.r-project.org")
if (!require("forcats"))  install.packages("forcats", repos="https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(ggrepel)
library(scales)
library(forcats)

message("=== 02_carbon_footprint.R starting ===")

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

# --- EPA eGRID 2022 regional data (lbs CO2 per MWh) -------------------------
# From EPA eGRID 2022 summary tables (Table 2 – Subregion output emission rates)
message("Loading eGRID 2022 hardcoded regional carbon intensity data...")

egrid <- data.frame(
  region          = c("NEWE", "NYUP", "RFCE", "RFCM", "RFCW",
                      "SRSO", "SRVC", "SPNO", "SPSO", "MROW",
                      "MRWE", "NWPP", "CAMX", "AZNM", "ERCT"),
  egrid_subregion = c("New England", "New York", "Mid-Atlantic/PJM",
                      "Great Lakes", "Ohio Valley", "Southeast",
                      "Virginia/Carolinas", "Central", "South Central",
                      "Midwest", "Missouri", "Northwest",
                      "California", "Southwest", "Texas"),
  co2_lb_per_mwh  = c(457, 510, 728, 1078, 1532,
                       952, 793, 1045, 869, 1049,
                       1215, 273, 519, 899, 818),
  primary_fuel    = c("Mix/Gas", "Mix/Hydro", "Gas/Coal",
                      "Coal", "Coal", "Gas/Coal",
                      "Gas/Nuclear", "Gas/Coal", "Gas",
                      "Coal/Wind", "Coal", "Hydro/Wind",
                      "Gas/Solar", "Gas/Coal", "Gas/Wind"),
  stringsAsFactors = FALSE
)

# --- Data centre geographic hubs ---------------------------------------------
# Approximate lon/lat centres for labelling on the map
dc_hubs <- data.frame(
  label        = c("Northern Virginia\n(largest DC market globally)",
                   "Phoenix AZ\n(fastest growing)",
                   "Pacific Northwest\n(cleanest grid, hyperscale)",
                   "Dallas TX",
                   "Silicon Valley CA"),
  region       = c("RFCE",  "AZNM",  "NWPP",   "ERCT",  "CAMX"),
  lon          = c(-77.4,   -112.0,  -122.3,   -96.8,   -122.0),
  lat          = c(38.9,    33.4,    45.5,     32.8,    37.4),
  stringsAsFactors = FALSE
)

# --- Derived metrics ---------------------------------------------------------
egrid <- egrid %>%
  mutate(
    carbon_cost_index = co2_lb_per_mwh / min(co2_lb_per_mwh),
    is_dc_hub = region %in% dc_hubs$region
  )

# Northern Virginia (RFCE/SRVC blend — use SRVC as primary VA grid)
# Pacific Northwest (NWPP)
va_intensity_lb_mwh   <- egrid$co2_lb_per_mwh[egrid$region == "SRVC"]
nwpp_intensity_lb_mwh <- egrid$co2_lb_per_mwh[egrid$region == "NWPP"]

# 1 TWh = 1e9 kWh; lbs CO2/MWh → metric tons CO2/TWh: * 1e6 / 2204.62
carbon_diff_mmt <- (va_intensity_lb_mwh - nwpp_intensity_lb_mwh) *
  1e6 / 2204.62 / 1e6  # million metric tons per TWh

message(sprintf(
  "  Virginia grid: %.0f lb CO2/MWh | Pacific NW: %.0f lb CO2/MWh",
  va_intensity_lb_mwh, nwpp_intensity_lb_mwh))
message(sprintf(
  "  Same 1 TWh workload emits %.2f Mmt MORE CO2 in Virginia vs Pacific NW",
  carbon_diff_mmt))

# --- Approximate region centroid coordinates for map colouring ---------------
# These centroids represent the rough geographic centre of each eGRID subregion
region_centroids <- data.frame(
  region = c("NEWE", "NYUP", "RFCE", "RFCM", "RFCW",
             "SRSO", "SRVC", "SPNO", "SPSO", "MROW",
             "MRWE", "NWPP", "CAMX", "AZNM", "ERCT"),
  lon    = c(-71.5, -74.5, -77.0, -83.0, -82.5,
             -86.5, -79.5, -97.5, -96.5, -93.5,
             -92.5, -121.0, -119.5, -111.5, -99.0),
  lat    = c(43.5,  43.5,  39.5,  42.0,  39.5,
             32.5,  36.0,  38.5,  31.5,  44.5,
             38.0,  46.5,  37.0,  34.0,  31.5),
  stringsAsFactors = FALSE
) %>%
  left_join(egrid %>% select(region, egrid_subregion, co2_lb_per_mwh,
                              carbon_cost_index, is_dc_hub),
            by = "region")

# ============================================================
# PLOT 4 — US regional map coloured by carbon intensity
# ============================================================
message("Generating Plot 4: grid_carbon_intensity_map.png")

us_map <- map_data("state")

p4 <- ggplot() +
  geom_polygon(data = us_map,
               aes(x = long, y = lat, group = group),
               fill = "grey88", colour = "white", linewidth = 0.3) +
  # Colour bubbles by CO2 intensity placed at region centroids
  geom_point(data = region_centroids,
             aes(x = lon, y = lat, colour = co2_lb_per_mwh,
                 size = co2_lb_per_mwh),
             alpha = 0.85) +
  # Data centre hub labels
  geom_point(data = dc_hubs,
             aes(x = lon, y = lat),
             shape = 8, size = 3.5, colour = "black", stroke = 1) +
  geom_label_repel(
    data = dc_hubs,
    aes(x = lon, y = lat, label = label),
    size = 2.7, fontface = "bold",
    box.padding = 0.5, point.padding = 0.4,
    max.overlaps = 20, seed = 42,
    fill = "white", colour = "black", alpha = 0.9,
    label.size = 0.2
  ) +
  scale_colour_gradient2(
    low      = "#1a9850",
    mid      = "#fee08b",
    high     = "#d73027",
    midpoint = 800,
    name     = "lbs CO2/MWh",
    labels   = label_comma()
  ) +
  scale_size_continuous(range = c(4, 14), guide = "none") +
  coord_fixed(ratio = 1.3, xlim = c(-125, -66), ylim = c(24, 50)) +
  labs(
    title    = "US Grid Carbon Intensity by Region with Major Data Centre Hubs",
    subtitle = "Same AI workload emits very different CO2 depending on where the server sits",
    caption  = paste0(
      "Source: EPA eGRID 2022. Stars mark major data centre markets.\n",
      "Virginia vs Pacific NW: same 1 TWh workload emits ~",
      round(carbon_diff_mmt, 2), " Mmt MORE CO2 in Virginia."
    )
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(colour = "grey40", size = 12,
                                    margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey55", size = 9, hjust = 0),
    legend.position  = "right",
    plot.margin      = margin(16, 16, 16, 16),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave("outputs/plots/grid_carbon_intensity_map.png", p4,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/grid_carbon_intensity_map.png")

# ============================================================
# PLOT 5 — Horizontal bar chart by region
# ============================================================
message("Generating Plot 5: region_comparison.png")

egrid_plot <- egrid %>%
  mutate(
    egrid_subregion = fct_reorder(egrid_subregion, co2_lb_per_mwh),
    hub_label = if_else(is_dc_hub, "Major DC hub", "Other region")
  )

p5 <- ggplot(egrid_plot,
             aes(x = co2_lb_per_mwh, y = egrid_subregion,
                 fill = hub_label)) +
  geom_col(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = label_comma()(co2_lb_per_mwh)),
            hjust = -0.1, size = 3.2, colour = "grey30") +
  scale_fill_manual(values = c("Major DC hub" = "#d73027",
                                "Other region" = "#4393c3"),
                    name = NULL) +
  scale_x_continuous(
    labels = label_comma(suffix = " lbs"),
    limits = c(0, 1750),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "Carbon Cost of Compute Varies Dramatically by Location",
    subtitle = "lbs CO2 per MWh \u2014 same AI workload, very different carbon cost",
    x        = "lbs CO2 per MWh",
    y        = NULL,
    caption  = "Source: EPA eGRID 2022 summary tables. Highlighted regions host major data centre markets."
  ) +
  theme_carbon() +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank())

ggsave("outputs/plots/region_comparison.png", p5,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/region_comparison.png")

# --- Save processed data -----------------------------------------------------
write_csv(egrid, "data/processed/egrid_carbon_intensity.csv")
message("Saved: data/processed/egrid_carbon_intensity.csv")

# --- Summary -----------------------------------------------------------------
message("\n=== 02_carbon_footprint.R complete ===")
message("Outputs saved:")
message("  data/processed/egrid_carbon_intensity.csv")
message("  outputs/plots/grid_carbon_intensity_map.png")
message("  outputs/plots/region_comparison.png")
message(sprintf(
  "\nKey finding: Ohio Valley (RFCW) is %.1fx dirtier than Pacific NW (NWPP)",
  egrid$carbon_cost_index[egrid$region == "RFCW"]))
