library(pacman)
pacman::p_load(data.table, dplyr, readr)

# read raw data
setwd("/Users/devin/Documents/eco-726/nyc-speed-cameras/qwnf7-osfstorage-archive/")
Speed_900 <- fread("Speed_900ft_11_22_24.csv")

# format date columns
Speed_900_monthly <- Speed_900 %>%
  mutate(
    first_appearance_month_year = format(as.Date(first_appearance), "%Y-%m"),
    year_month = paste0(format(as.Date(Date), "%Y-%m"), "-01")
  )

# aggregate events monthly
Speed_900_monthly <- Speed_900_monthly %>%
  group_by(ID, year_month) %>%
  summarise(
    total_collisions = sum(num_collisions, na.rm = TRUE),
    total_injuries = sum(total_injuries, na.rm = TRUE),
    first_appearance = first(first_appearance_month_year),
    .groups = "drop"
  )

# create indices for test assignment
Speed_900_monthly <- Speed_900_monthly %>%
  mutate(
    first_appearance = as.Date(paste(first_appearance, "01", sep = "-")),
    year_month = as.Date(year_month),
    t = as.numeric(format(year_month, "%Y%m")),
    G = as.numeric(format(first_appearance, "%Y%m"))
  )

# apply log transforms on event data
Speed_900_monthly <- Speed_900_monthly %>%
  mutate(
    log_total_collisions = log1p(total_collisions),
    log_total_injuries = log1p(total_injuries)
  )

# remove low activity intersections (< 10 unique units)
small_groups <- Speed_900_monthly %>%
  distinct(ID, G) %>%
  count(G) %>%
  filter(n < 10) %>%
  pull(G)

Speed_900_monthly <- Speed_900_monthly %>%
  filter(!(G %in% small_groups))

# 7. Write out clean dataset for downstream modeling
fwrite(Speed_900_monthly, "Speed_900_monthly_agg.csv")