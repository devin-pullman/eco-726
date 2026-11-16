library(pacman)
pacman::p_load(data.table, dplyr, did, ggplot2, patchwork)

# read pre-cleaned dataset directly
path <- ("/Users/devin/Documents/eco-726/nyc-speed-cameras/qwnf7-osfstorage-archive/Speed_900_monthly_agg.csv")
Speed_900_monthly <- fread(path)

# fit Models
att_collisions <- att_gt(yname = "log_total_collisions", tname = "t", idname = "ID", gname = "G", data = Speed_900_monthly, panel = TRUE, control_group = "notyettreated", allow_unbalanced_panel = TRUE)
att_injuries <- att_gt(yname = "log_total_injuries", tname = "t", idname = "ID", gname = "G", data = Speed_900_monthly, panel = TRUE, control_group = "notyettreated", allow_unbalanced_panel = TRUE)

# helper function to create tables S1 & S2
get_summary_df <- function(att_obj) {
  att_df <- data.frame(
    time = att_obj$tp,
    group = att_obj$group,
    estimate = att_obj$att,
    std.error = att_obj$se
  ) %>%
    mutate(event_time = time - group) %>%
    filter(event_time >= 0 & event_time <= 6)
  
  avg_att <- mean(att_df$estimate, na.rm = TRUE)
  se_att <- sqrt(mean(att_df$std.error^2, na.rm = TRUE) / nrow(att_df))
  pct_change <- (exp(avg_att) - 1) * 100
  
  data.frame(
    Metric = c("Pooled ATT (Log)", "Std. Error", "Est. % Change"),
    Value = c(round(avg_att, 4), round(se_att, 4), paste0(round(pct_change, 2), "%"))
  )
}

df_s1 <- get_summary_df(att_collisions)
df_s2 <- get_summary_df(att_injuries)

# convert data frames into tables
df_s1$Table <- "Table S1: Collisions"
df_s2$Table <- "Table S2: Injuries"
combined_tables_df <- rbind(df_s1, df_s2)

p_tables <- ggplot(combined_tables_df, aes(x = 0, y = Metric, label = Value)) +
  geom_text(hjust = 0, family = "mono", size = 3.5) +
  facet_wrap(~Table, ncol = 1) +
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 10))

# generate Fig 3 (event study plot)
es_collisions <- aggte(att_collisions, type = "dynamic", min_e = -6, max_e = 6)
p_fig3 <- ggdid(es_collisions) + 
  labs(title = "Figure 3: Event Study (Collisions)", x = "Months", y = "ATT") + 
  theme_minimal()

# combine tables and plots using patchwork
combined_pane <- p_tables | p_fig3

print(combined_pane)