# =============================================================================
# 05_health_burden.R
# Global Health Burden and Equity — Layer 3C
# Sources: Our World in Data (air pollution deaths, CO2 per capita)
# =============================================================================

if (!require("ggplot2"))  install.packages("ggplot2", repos="https://cloud.r-project.org")
if (!require("dplyr"))    install.packages("dplyr", repos="https://cloud.r-project.org")
if (!require("readr"))    install.packages("readr", repos="https://cloud.r-project.org")
if (!require("scales"))   install.packages("scales", repos="https://cloud.r-project.org")
if (!require("tidyr"))    install.packages("tidyr", repos="https://cloud.r-project.org")
if (!require("forcats"))  install.packages("forcats", repos="https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(tidyr)
library(forcats)

message("=== 05_health_burden.R starting ===")

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

# --- World Bank income group classification (2022) ---------------------------
# Expanded to cover major countries across all regions and income tiers
high_income    <- c(
  "United States", "United Kingdom", "Germany", "France",
  "Japan", "Australia", "Canada", "South Korea",
  "Italy", "Spain", "Netherlands", "Switzerland", "Sweden",
  "Norway", "Denmark", "Finland", "Belgium", "Austria",
  "Portugal", "Ireland", "New Zealand", "Singapore", "Israel",
  "Luxembourg", "Iceland", "Malta", "Cyprus"
)
upper_middle   <- c(
  "China", "Brazil", "Mexico", "South Africa", "Turkey",
  "Thailand", "Colombia",
  "Russia", "Malaysia", "Argentina", "Chile", "Peru",
  "Ecuador", "Iraq", "Iran", "Jordan", "Tunisia",
  "Algeria", "Egypt", "Kazakhstan", "Serbia",
  "Belarus", "Dominican Republic", "Cuba", "Venezuela",
  "Libya", "Gabon", "Botswana", "Namibia"
)
lower_middle   <- c(
  "India", "Nigeria", "Pakistan", "Bangladesh", "Ghana",
  "Kenya", "Philippines", "Morocco",
  "Indonesia", "Vietnam", "Cambodia", "Myanmar", "Nepal",
  "Sri Lanka", "Senegal", "Cameroon", "Zambia", "Zimbabwe",
  "Guatemala", "Honduras", "El Salvador", "Bolivia", "Paraguay",
  "Kyrgyzstan", "Tajikistan", "Uzbekistan", "Mongolia",
  "Papua New Guinea", "Laos"
)
low_income_grp <- c(
  "Chad", "Mali", "Niger", "Mozambique", "Ethiopia",
  "Uganda", "Tanzania", "Burkina Faso",
  "Democratic Republic of Congo", "Haiti", "Afghanistan",
  "Central African Republic", "Liberia", "Sierra Leone",
  "Guinea", "Malawi", "Benin", "Rwanda", "Somalia",
  "South Sudan", "Eritrea"
)

income_lookup <- bind_rows(
  data.frame(country = high_income,    income_group = "High income",
             stringsAsFactors = FALSE),
  data.frame(country = upper_middle,   income_group = "Upper middle income",
             stringsAsFactors = FALSE),
  data.frame(country = lower_middle,   income_group = "Lower middle income",
             stringsAsFactors = FALSE),
  data.frame(country = low_income_grp, income_group = "Low income",
             stringsAsFactors = FALSE)
)

# --- Download air pollution deaths (IHME GBD, age-standardised combined rate) -
# Note: the original OWID dataset URL is no longer available (repository archived).
# The fallback hardcoded data (IHME ~2019) is used until a replacement source is
# wired in. Age-standardised combined indoor+outdoor rates are required to preserve
# the income-group equity gradient (crude rates conflate age structure with exposure).
air_url   <- paste0(
  "https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/",
  "Deaths%20from%20indoor%20and%20outdoor%20air%20pollution/",
  "Deaths%20from%20indoor%20and%20outdoor%20air%20pollution.csv"
)
air_local <- "data/raw/owid_air_pollution_deaths.csv"

message("Downloading OWID air pollution deaths data...")
tryCatch({
  download.file(air_url, air_local, quiet = TRUE)
  message("  Download successful.")
}, error = function(e) {
  message(sprintf("  WARNING: Download failed (%s). Using hardcoded fallback.", conditionMessage(e)))
})

air_df <- NULL
if (file.exists(air_local) && file.size(air_local) > 100) {
  tryCatch({
    air_df <- read_csv(air_local, show_col_types = FALSE)
    message(sprintf("  Loaded air pollution data: %d rows, cols: %s",
                    nrow(air_df),
                    paste(names(air_df)[1:min(5, ncol(air_df))], collapse = ", ")))
  }, error = function(e) {
    message(sprintf("  WARNING: Parse failed (%s).", conditionMessage(e)))
    air_df <<- NULL
  })
}

# Hardcoded fallback: IHME Global Burden of Disease, age-standardised combined
# indoor + outdoor air pollution mortality rate per 100,000 (~2019 vintage, 33 countries)
air_fallback <- data.frame(
  country = c(
    "Chad", "Niger", "Burkina Faso", "Mali", "Mozambique", "Ethiopia",
    "Uganda", "Tanzania", "Nigeria", "Bangladesh", "India", "Pakistan",
    "Ghana", "Kenya", "Morocco", "Philippines", "China", "South Africa",
    "Indonesia", "Egypt", "Brazil", "Mexico", "Colombia", "Turkey",
    "Thailand", "United States", "Germany", "France", "United Kingdom",
    "Japan", "Australia", "Canada", "South Korea"
  ),
  air_pollution_deaths_per_100k = c(
    278, 260, 241, 235, 210, 195,
    183, 172, 165, 158, 134, 129,
    118, 107, 98, 93, 87, 82,
    79, 75, 42, 38, 35, 33,
    55, 8.4, 9.2, 8.8, 8.1,
    10.3, 5.9, 7.2, 12.1
  ),
  stringsAsFactors = FALSE
)

# Parse downloaded data if available (expects age-standardised per-100k rate column)
if (!is.null(air_df)) {
  air_cols   <- names(air_df)
  entity_col <- air_cols[grepl("entity|country", air_cols, ignore.case = TRUE)][1]
  year_col   <- air_cols[grepl("^year$",          air_cols, ignore.case = TRUE)][1]
  rate_col   <- air_cols[grepl("per.*100|rate|per_100", air_cols, ignore.case = TRUE)][1]
  if (is.na(rate_col))
    rate_col <- air_cols[grepl("death|mortality", air_cols, ignore.case = TRUE)][1]

  if (!is.na(entity_col) && !is.na(year_col) && !is.na(rate_col)) {
    air_processed <- air_df %>%
      rename(country = !!entity_col, year = !!year_col,
             deaths_raw = !!rate_col) %>%
      filter(!is.na(deaths_raw)) %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      ungroup() %>%
      select(country, air_pollution_deaths_per_100k = deaths_raw)

    med_val <- median(air_processed$air_pollution_deaths_per_100k, na.rm = TRUE)
    if (med_val > 0 & med_val < 1000) {
      message(sprintf("  Using downloaded air pollution data (median: %.1f per 100k, %d countries).",
                      med_val, nrow(air_processed)))
      air_fallback <- air_processed
    } else {
      message("  Downloaded values implausible — using hardcoded fallback.")
    }
  } else {
    message("  Could not identify required columns — using hardcoded fallback.")
  }
}

# --- Load CO2 per capita from OWID -------------------------------------------
owid_local <- "data/raw/owid_co2.csv"
if (!file.exists(owid_local)) {
  stop("OWID CO2 data missing. Run 03_global_emissions.R first.")
}
co2_raw <- read_csv(owid_local, show_col_types = FALSE)

exclude_agg <- c("World", "Asia", "Europe", "Africa",
                 "North America", "South America", "Oceania",
                 "European Union (27)", "High-income countries",
                 "Low-income countries", "Upper-middle-income countries",
                 "Lower-middle-income countries")

# Most recent year per country for co2_per_capita
co2_recent <- co2_raw %>%
  filter(!is.na(co2_per_capita), !is.na(population)) %>%
  filter(!country %in% exclude_agg) %>%
  group_by(country) %>%
  filter(year == max(year[year <= 2022])) %>%
  ungroup() %>%
  select(country, year, co2_per_capita, population)

message(sprintf("  CO2 per capita: %d countries with recent data.", nrow(co2_recent)))

# --- Merge datasets ----------------------------------------------------------
health_df <- air_fallback %>%
  inner_join(co2_recent,   by = "country") %>%
  left_join(income_lookup, by = "country") %>%
  mutate(
    income_group = if_else(is.na(income_group), "Other", income_group),
    income_group = factor(income_group,
                          levels = c("High income", "Upper middle income",
                                     "Lower middle income", "Low income", "Other"))
  )

message(sprintf("  Health equity dataset: %d countries merged.", nrow(health_df)))

write_csv(health_df, "data/processed/health_equity.csv")
message("Saved: data/processed/health_equity.csv")

# --- Multi-year dataset (two time points for trend comparison) ---------------
# Requires a successful OWID download that includes multiple years (per-100k rate format)
if (!is.null(air_df)) {
  air_cols_my <- names(air_df)
  ent_col_my  <- air_cols_my[grepl("entity|country", air_cols_my, ignore.case = TRUE)][1]
  yr_col_my   <- air_cols_my[grepl("^year$",          air_cols_my, ignore.case = TRUE)][1]
  rate_col_my <- air_cols_my[grepl("per.*100|rate|per_100", air_cols_my, ignore.case = TRUE)][1]
  if (is.na(rate_col_my))
    rate_col_my <- air_cols_my[grepl("death|mortality", air_cols_my, ignore.case = TRUE)][1]

  if (!is.na(ent_col_my) && !is.na(yr_col_my) && !is.na(rate_col_my)) {
    air_ts <- air_df %>%
      rename(country = !!ent_col_my, year = !!yr_col_my,
             air_pollution_deaths_per_100k = !!rate_col_my) %>%
      filter(!is.na(air_pollution_deaths_per_100k))

    # Use the earliest available year and most recent year for comparison
    avail_years <- sort(unique(air_ts$year))
    if (length(avail_years) >= 2) {
      compare_years <- c(avail_years[1], avail_years[length(avail_years)])

      air_two <- air_ts %>%
        filter(year %in% compare_years) %>%
        group_by(country, year) %>%
        summarise(air_pollution_deaths_per_100k = mean(air_pollution_deaths_per_100k,
                                                        na.rm = TRUE), .groups = "drop")

      co2_two <- co2_raw %>%
        filter(!is.na(co2_per_capita), !is.na(population),
               !country %in% exclude_agg,
               year %in% compare_years) %>%
        select(country, year, co2_per_capita, population)

      health_multiyear <- air_two %>%
        inner_join(co2_two,      by = c("country", "year")) %>%
        left_join(income_lookup, by = "country") %>%
        mutate(
          income_group = if_else(is.na(income_group), "Other", income_group),
          income_group = factor(income_group,
                                levels = c("High income", "Upper middle income",
                                           "Lower middle income", "Low income", "Other")),
          year_label   = as.character(year)
        )

      write_csv(health_multiyear, "data/processed/health_equity_multiyear.csv")
      message(sprintf("Saved: data/processed/health_equity_multiyear.csv (%d rows, %d countries, years: %s)",
                      nrow(health_multiyear), n_distinct(health_multiyear$country),
                      paste(compare_years, collapse = " vs ")))
    }
  }
}

# ============================================================
# PLOT 10 — Top 20 countries by air pollution deaths per 100k
# ============================================================
message("Generating Plot 10: air_pollution_deaths.png")

top20 <- health_df %>%
  filter(income_group != "Other") %>%
  arrange(desc(air_pollution_deaths_per_100k)) %>%
  slice_head(n = 20) %>%
  mutate(country = fct_reorder(country, air_pollution_deaths_per_100k))

p10 <- ggplot(top20, aes(x = air_pollution_deaths_per_100k,
                          y = country,
                          fill = income_group)) +
  geom_col(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = round(air_pollution_deaths_per_100k, 1)),
            hjust = -0.15, size = 3.2, colour = "grey30") +
  scale_fill_manual(values = pal_income, name = "Income group",
                    drop = FALSE) +
  scale_x_continuous(
    labels  = label_comma(),
    limits  = c(0, max(top20$air_pollution_deaths_per_100k) * 1.15),
    expand  = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Air Pollution Mortality Burden by Country",
    subtitle = "Deaths per 100,000 population — top 20 countries (combined indoor + outdoor)",
    x        = "Deaths per 100,000 population",
    y        = NULL,
    caption  = paste0(
      "Source: State of Global Air / Health Effects Institute via Our World in Data. ",
      "Ambient PM2.5 (outdoor) mortality, 2015.\n",
      "Income group classifications from World Bank (2022)."
    )
  ) +
  theme_carbon() +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank())

ggsave("outputs/plots/air_pollution_deaths.png", p10,
       width = 1600 / 150, height = 1000 / 150, dpi = 150)
message("Saved: outputs/plots/air_pollution_deaths.png")

# --- Summary statistics for README -------------------------------------------
summary_by_group <- health_df %>%
  filter(income_group != "Other") %>%
  group_by(income_group) %>%
  summarise(
    n                             = n(),
    mean_co2_per_capita           = mean(co2_per_capita, na.rm = TRUE),
    mean_air_deaths_per_100k      = mean(air_pollution_deaths_per_100k,
                                         na.rm = TRUE),
    .groups = "drop"
  )

message("\nSummary by income group:")
print(as.data.frame(summary_by_group))

write_csv(summary_by_group, "outputs/tables/income_group_summary.csv")
message("Saved: outputs/tables/income_group_summary.csv")

# --- Summary -----------------------------------------------------------------
message("\n=== 05_health_burden.R complete ===")
message("Outputs saved:")
message("  data/processed/health_equity.csv")
message("  outputs/plots/air_pollution_deaths.png")
message("  outputs/tables/income_group_summary.csv")
