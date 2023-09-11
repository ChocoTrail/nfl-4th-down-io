## Run or source 01_data.R before running code in this file

# This file is for some (useful?) data visualizations
# Most of the visualizations in this file are used in the paper

############
## Libraries
############

theme_set(theme_minimal())

######
## EDA
######

## Figure 1.1 in paper
# What coaches should be doing based on the current probabilities

plot_df <- clean_pbp_sub %>%
  filter(!is.na(fourth_down_bot)) %>% 
  group_by(yardline_100_group, ydstogo_group, fourth_down_bot) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  mutate(group_total = sum(n)) %>% 
  ungroup() %>% 
  mutate(freq = n / group_total) %>% 
  group_by(yardline_100_group, ydstogo_group) %>%
  slice_max(freq) %>% 
  ungroup()

plot_df %>%
  ggplot(aes(x = yardline_100_group,
             y = ydstogo_group,
             fill = fourth_down_bot)) +
  geom_tile(color = "white", size = 0.1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 7
    ),
    axis.text.y = element_text(size = 7),
    strip.text.y.right = element_text(angle = 90)
  ) +
  scale_fill_manual(name = "Decision",
                    values = c("#d53e4f", "gold", "#2C7FB8"),
                    labels = c("FGA", "GO", "PUNT")) +
  labs(x = "Yards to opponent endzone",
       y = "Yards to go",
       title = "What coaches should be doing")

## Figures 1.2 and 3.2 in paper
# What coaches are currently doing most frequently

plot_df2 <- clean_pbp_sub %>%
  group_by(yardline_100_group, ydstogo_group, fourth_down_decision) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  mutate(group_total = sum(n)) %>% 
  ungroup() %>% 
  mutate(freq = n / group_total) %>% 
  group_by(yardline_100_group, ydstogo_group) %>%
  slice_max(freq) %>% 
  ungroup()

plot_df2 %>%
  ggplot(aes(x = yardline_100_group,
             y = ydstogo_group,
             fill = fourth_down_decision)) +
  geom_tile(color = "white", size = 0.1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 7
    ),
    axis.text.y = element_text(size = 7),
    strip.text.y.right = element_text(angle = 90)
  ) +
  scale_fill_manual(name = "Decision",
                    values = c("#d53e4f", "gold", "#2C7FB8"),
                    labels = c("FGA", "GO", "PUNT")) +
  labs(x = "Yards to opponent endzone",
       y = "Yards to go",
       title = paste0("What coaches are doing"))

## This figure is not in the paper, but may be useful
# Make it a gradient instead of a solid block to show "uncertainty"

plot_df3 <- clean_pbp_sub %>%
  filter(!is.na(fourth_down_bot)) %>% 
  group_by(yardline_100_group, ydstogo_group, fourth_down_bot) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  mutate(group_total = sum(n)) %>% 
  ungroup() %>% 
  mutate(freq = n / group_total) %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  slice_max(freq, n = 2) %>% 
  mutate(diff = freq - min(freq)) %>% 
  slice_max(freq) %>% 
  ungroup()

plot_df3 %>% 
  ggplot(aes(x = yardline_100_group,
             y = ydstogo_group,
             fill = fourth_down_bot,
             alpha = diff)) +
  geom_tile(color = "white", size = 0.1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 7
    ),
    axis.text.y = element_text(size = 7),
    strip.text.y.right = element_text(angle = 90)
  ) +
  scale_fill_manual(name = "Decision",
                    values = c("#d53e4f", "gold", "#2C7FB8"),
                    labels = c("FGA", "GO", "PUNT")) +
  labs(x = "Yards to opponent endzone",
       y = "Yards to go",
       title = paste0("What coaches should be doing")) +
  scale_alpha(guide = "none")

## Figure 3.1 in the paper
# This is what coaches "should" be doing based on empirical frequencies of success

plot_df4 <- clean_pbp_sub2 %>%
  #filter(!is.na(fourth_down_bot)) %>% 
  group_by(yardline_100_group, ydstogo_group, fourth_down_decision) %>% 
  summarise(mean_epa = mean(new_epa),
            n = n()) %>% 
  ungroup() %>% 
  filter(fourth_down_decision != "idk") %>% 
  #filter(n > 30) %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  slice_max(mean_epa, n = 2) %>%
  mutate(diff = max(mean_epa) - min(mean_epa)) %>% 
  slice_max(mean_epa) %>% 
  ungroup()

plot_df4 %>% 
  ggplot(aes(x = yardline_100_group,
             y = ydstogo_group,
             fill = fourth_down_decision)) +
  geom_tile(color = "white", size = 0.1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 7
    ),
    axis.text.y = element_text(size = 7),
    strip.text.y.right = element_text(angle = 90)
  ) +
  scale_fill_manual(name = "Decision",
                    values = c("#d53e4f", "gold", "#2C7FB8"),
                    labels = c("FGA", "GO", "PUNT")) +
  labs(x = "Yards to opponent endzone",
       y = "Yards to go",
       title = "What coaches should be doing",
       subtitle = "Based on our new epa value")
