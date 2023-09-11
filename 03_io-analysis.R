# File to run a bunch on the server

############
## Libraries
############

library(tidyverse)
library(nflfastR)

theme_set(theme_minimal())

##############
## Data Needed
##############

change_df <- tibble(yardline_100_group = c(rep("1-10", 6),
                                           rep("11-20", 6),
                                           rep("21-30", 6),
                                           rep("31-40", 1),
                                           rep("41-50", 10),
                                           rep("51-60", 10),
                                           rep("61-70", 9),
                                           rep("71-80", 7),
                                           rep("81-90", 5),
                                           rep("91+", 5)),
                    ydstogo_group = c("2", "3", "4", "5", "7", "10+",
                                      "2", "3", "5", "7", "8", "9",
                                      "2", "3", "4", "5", "6", "7",
                                      "3",
                                      "1", "2", "3", "4", "5", "6", "7", "8", "9", "10+",
                                      "1", "2", "3", "4", "5", "6", "7", "8", "9", "10+",
                                      "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                      "1", "2", "3", "6", "8", "9", "10+",
                                      "1", "2", "3", "4", "9",
                                      "4", "5", "8", "9", "10+"),
                    change_from = c(rep("go", 6),
                                    rep("go", 6),
                                    rep("go", 6),
                                    "fga",
                                    "fga", rep("go", 3), "fga", rep("go", 4), "fga",
                                    rep("go", 10),
                                    rep("go", 9),
                                    rep("go", 7),
                                    rep("go", 5),
                                    rep("go", 5)),
                    change_to = c(rep("fga", 6),
                                  rep("fga", 6),
                                  rep("fga", 6),
                                  "go",
                                  "go", rep("punt", 9),
                                  rep("punt", 10),
                                  rep("punt", 9),
                                  rep("punt", 7),
                                  rep("punt", 5),
                                  rep("punt", 5)))

clean_pbp_df <- readRDS("clean_pbp_df.rds")

clean_pbp_sub <- clean_pbp_df %>% 
  mutate(season = str_sub(game_id, 1, 4) %>% as.numeric(),
         coach_name = ifelse(posteam == home_team, home_coach, away_coach),
         max_wp = pmax(go_wp, fg_wp, punt_wp, na.rm = TRUE)) %>% 
  dplyr::select(game_id, season, home_team, away_team, posteam, coach_name,
                season_type:play_type, penalty, third_down_converted,
                ep, epa, wp, wp_group, wpa,
                # from nfl4th
                fourth_down_decision, go_wp, fg_wp, punt_wp, max_wp,
                ydstogo_group:next_play_state_3)

clean_pbp_sub <- clean_pbp_sub %>% 
  mutate(fourth_down_bot = pmap_dbl(clean_pbp_sub %>% 
                                      dplyr::select(go_wp:max_wp),
                                    ~match(..4, c(..1,..2,..3)))) %>% 
  mutate(fourth_down_bot = ifelse(is.na(max_wp), NA, fourth_down_bot)) %>% 
  mutate(fourth_down_bot = case_when(
    is.na(fourth_down_bot) ~ NA_character_,
    fourth_down_bot == 1 ~ "go",
    fourth_down_bot == 2 ~ "fga",
    fourth_down_bot == 3 ~ "punt"
  ))

clean_pbp_sub2 <- clean_pbp_sub %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  mutate(new_ep = mean(ep, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(new_epa = ep + epa - new_ep)

#############
## IO Process
#############

while(TRUE) {

# Save a new data frame that will contain all the new weights
new_full_df <- clean_pbp_sub2

## Change from "go" to "fga"
# n = 18

filtered_change_df <- change_df %>% 
  filter(change_from == "go" & change_to == "fga")

for (k in 1:nrow(filtered_change_df)) {
  
  sub_df <- clean_pbp_sub2 %>% 
    filter(ydstogo_group == filtered_change_df$ydstogo_group[k] & 
             yardline_100_group == filtered_change_df$yardline_100_group[k])
  
  # In the following example data frame we want 
  eval_df <- sub_df %>% 
    group_by(fourth_down_decision) %>% 
    summarise(n = n(),
              avg_epa = mean(new_epa)) %>% 
    ungroup()
  
  # Original averages for each decision based on epa
  fga_avg <- eval_df %>% filter(fourth_down_decision == "fga") %>% pull(avg_epa)
  go_avg <- eval_df %>% filter(fourth_down_decision == "go") %>% pull(avg_epa)
  
  # Save original averages for each decision
  ogfga_avg <- fga_avg
  oggo_avg <- go_avg
  
  # keep track of how many bootstrap samples are taken
  i <- 0
  
  # keep track of how many bootstrap samples are saved
  j <- 0
  
  # keep track of Euclidean distance
  diff <- c()
  
  # keep track of the averages
  avg_fga <- c()
  avg_go <- c()
  ogavg_fga <- c()
  ogavg_go <- c()
  
  # place holder for "previous" data set
  prev_df <- sub_df %>% 
    mutate(weights = 1,
           epa_weighted = new_epa)
  
  # Original weight vectors
  w_vec <- rep(1, nrow(sub_df))
  
  tictoc::tic()
  while(fga_avg < go_avg) {
    
    # place holder metric for the previously used data set (or bootstrapped set)
    prev_go_avg <- prev_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "go") %>% 
      pull(avg_epa)
    
    prev_fga_avg <- prev_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "fga") %>% 
      pull(avg_epa)
    
    prev_diff <- prev_go_avg - prev_fga_avg
    
    # bootstrap sample from the current data set
    boot_ind <- sample(1:nrow(prev_df), size = 1, replace = TRUE)
    
    w_vec[boot_ind] <- w_vec[boot_ind] + 1
    
    boot_df <- prev_df %>% 
      mutate(weights = w_vec) %>% 
      mutate(epa_weighted = new_epa * weights)
    
    # what is the new fga_avg and go_avg?
    fga_avg <- boot_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "fga") %>% 
      pull(avg_epa)
    
    go_avg <- boot_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "go") %>% 
      pull(avg_epa)
    
    current_diff <- go_avg - fga_avg
    
    # save the bootstrap if the mean is going in the right direction
    if(current_diff < prev_diff) {
      
      prev_df <- boot_df
      
      # update number of bootstrap samples saved
      j <- j + 1
      
      # calculate and save the difference
      diff[j] <- sqrt(sum((rep(1, nrow(sub_df)) - prev_df$epa_weighted)^2))
      
      # save current boostrap's metrics
      avg_fga[j] <- fga_avg
      avg_go[j] <- go_avg
      ogavg_fga[j] <- ogfga_avg
      ogavg_go[j] <- oggo_avg
      
    }
    else {
      
      # undo the weighting vector back to the way it was
      w_vec[boot_ind] <- w_vec[boot_ind] - 1
      
    }
    
    # update number of bootstrap samples taken
    i <- i + 1
    
  }
  tictoc::toc()
  
  summary_df <- tibble(
    iteration = 1:j,
    diff = diff,
    avg_fga = avg_fga,
    avg_go = avg_go,
    ogavg_fga = ogavg_fga,
    ogavg_go = ogavg_go)
  
  # Take the new data frame for this bin and substitute it into the full data
  onechange_df <- new_full_df %>% 
    anti_join(sub_df) %>% 
    bind_rows(prev_df) %>% 
    # Substitute the epa_weighted NAs for the normal new_epa
    mutate(epa_weighted = ifelse(is.na(epa_weighted), new_epa, epa_weighted))
  
  # Save change in the new_full_df
  new_full_df <- onechange_df
}

# Visualize the new "data"
plot_df <- new_full_df %>%
  #filter(!is.na(fourth_down_bot)) %>% 
  group_by(yardline_100_group, ydstogo_group, fourth_down_decision) %>% 
  summarise(mean_epa = mean(epa_weighted),
            n = n()) %>% 
  ungroup() %>%
  filter(fourth_down_decision != "idk") %>% 
  #filter(n > 30) %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  slice_max(mean_epa, n = 2) %>%
  mutate(diff = max(mean_epa) - min(mean_epa)) %>% 
  slice_max(mean_epa) %>% 
  ungroup()

plot_df %>% 
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
       title = "What coaches \"should\" be doing",
       subtitle = "Based on our new epa value")


## Change from "go" to "punt"
# n = 43

filtered_change_df <- change_df %>% 
  filter(change_from == "go" & change_to == "punt")

for (k in 1:nrow(filtered_change_df)) {
  
  sub_df <- clean_pbp_sub2 %>% 
    filter(ydstogo_group == filtered_change_df$ydstogo_group[k] & 
             yardline_100_group == filtered_change_df$yardline_100_group[k])
  
  # In the following example data frame we want 
  eval_df <- sub_df %>% 
    group_by(fourth_down_decision) %>% 
    summarise(n = n(),
              avg_epa = mean(new_epa)) %>% 
    ungroup()
  
  # Original averages for each decision based on epa
  punt_avg <- eval_df %>% filter(fourth_down_decision == "punt") %>% pull(avg_epa)
  go_avg <- eval_df %>% filter(fourth_down_decision == "go") %>% pull(avg_epa)
  
  # Save original averages for each decision
  ogpunt_avg <- punt_avg
  oggo_avg <- go_avg
  
  # keep track of how many bootstrap samples are taken
  i <- 0
  
  # keep track of how many bootstrap samples are saved
  j <- 0
  
  # keep track of Euclidean distance
  diff <- c()
  
  # keep track of the averages
  avg_punt <- c()
  avg_go <- c()
  ogavg_punt <- c()
  ogavg_go <- c()
  
  # place holder for "previous" data set
  prev_df <- sub_df %>% 
    mutate(weights = 1,
           epa_weighted = new_epa)
  
  # Original weight vectors
  w_vec <- rep(1, nrow(sub_df))
  
  tictoc::tic()
  while(punt_avg < go_avg) {
    
    # place holder metric for the previously used data set (or bootstrapped set)
    prev_go_avg <- prev_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "go") %>% 
      pull(avg_epa)
    
    prev_punt_avg <- prev_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "punt") %>% 
      pull(avg_epa)
    
    prev_diff <- prev_go_avg - prev_punt_avg
    
    # bootstrap sample from the current data set
    boot_ind <- sample(1:nrow(prev_df), size = 1, replace = TRUE)
    
    w_vec[boot_ind] <- w_vec[boot_ind] + 1
    
    boot_df <- prev_df %>% 
      mutate(weights = w_vec) %>% 
      mutate(epa_weighted = new_epa * weights)
    
    # what is the new fga_avg and go_avg?
    punt_avg <- boot_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "punt") %>% 
      pull(avg_epa)
    
    go_avg <- boot_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "go") %>% 
      pull(avg_epa)
    
    current_diff <- go_avg - punt_avg
    
    # save the bootstrap if the mean is going in the right direction
    if(current_diff < prev_diff) {
      
      prev_df <- boot_df
      
      # update number of bootstrap samples saved
      j <- j + 1
      
      # calculate and save the difference
      diff[j] <- sqrt(sum((rep(1, nrow(sub_df)) - prev_df$epa_weighted)^2))
      
      # save current boostrap's metrics
      avg_punt[j] <- punt_avg
      avg_go[j] <- go_avg
      ogavg_punt[j] <- ogpunt_avg
      ogavg_go[j] <- oggo_avg
      
    }
    else {
      
      # undo the weighting vector back to the way it was
      w_vec[boot_ind] <- w_vec[boot_ind] - 1
      
    }
    
    # update number of bootstrap samples taken
    i <- i + 1
    
  }
  tictoc::toc()
  
  summary_df <- tibble(
    iteration = 1:j,
    diff = diff,
    avg_punt = avg_punt,
    avg_go = avg_go,
    ogavg_punt = ogavg_punt,
    ogavg_go = ogavg_go)
  
  # Take the new data frame for this bin and substitute it into the full data
  onechange_df <- new_full_df %>% 
    anti_join(sub_df) %>% 
    bind_rows(prev_df) %>% 
    # Substitute the epa_weighted NAs for the normal new_epa
    mutate(epa_weighted = ifelse(is.na(epa_weighted), new_epa, epa_weighted))
  
  # Save change in the new_full_df
  new_full_df <- onechange_df
}

# Visualize the new "data"
plot_df <- new_full_df %>%
  #filter(!is.na(fourth_down_bot)) %>% 
  group_by(yardline_100_group, ydstogo_group, fourth_down_decision) %>% 
  summarise(mean_epa = mean(epa_weighted),
            n = n()) %>% 
  ungroup() %>%
  filter(fourth_down_decision != "idk") %>% 
  #filter(n > 30) %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  slice_max(mean_epa, n = 2) %>%
  mutate(diff = max(mean_epa) - min(mean_epa)) %>% 
  slice_max(mean_epa) %>% 
  ungroup()

plot_df %>% 
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
       title = "What coaches \"should\" be doing",
       subtitle = "Based on our new epa value")


## Change from "fga" to "go"
# n = 2

filtered_change_df <- change_df %>% 
  filter(change_from == "fga" & change_to == "go")

for (k in 1:nrow(filtered_change_df)) {
  
  sub_df <- clean_pbp_sub2 %>% 
    filter(ydstogo_group == filtered_change_df$ydstogo_group[k] & 
             yardline_100_group == filtered_change_df$yardline_100_group[k])
  
  # In the following example data frame we want 
  eval_df <- sub_df %>% 
    group_by(fourth_down_decision) %>% 
    summarise(n = n(),
              avg_epa = mean(new_epa)) %>% 
    ungroup()
  
  # Original averages for each decision based on epa
  go_avg <- eval_df %>% filter(fourth_down_decision == "go") %>% pull(avg_epa)
  fga_avg <- eval_df %>% filter(fourth_down_decision == "fga") %>% pull(avg_epa)
  
  # Save original averages for each decision
  oggo_avg <- go_avg
  ogfga_avg <- fga_avg
  
  # keep track of how many bootstrap samples are taken
  i <- 0
  
  # keep track of how many bootstrap samples are saved
  j <- 0
  
  # keep track of Euclidean distance
  diff <- c()
  
  # keep track of the averages
  avg_go <- c()
  avg_fga <- c()
  ogavg_go <- c()
  ogavg_fga <- c()
  
  # place holder for "previous" data set
  prev_df <- sub_df %>% 
    mutate(weights = 1,
           epa_weighted = new_epa)
  
  # Original weight vectors
  w_vec <- rep(1, nrow(sub_df))
  
  tictoc::tic()
  while(go_avg < fga_avg) {
    
    # place holder metric for the previously used data set (or bootstrapped set)
    prev_fga_avg <- prev_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "fga") %>% 
      pull(avg_epa)
    
    prev_go_avg <- prev_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "go") %>% 
      pull(avg_epa)
    
    prev_diff <- prev_fga_avg - prev_go_avg
    
    # bootstrap sample from the current data set
    boot_ind <- sample(1:nrow(prev_df), size = 1, replace = TRUE)
    
    w_vec[boot_ind] <- w_vec[boot_ind] + 1
    
    boot_df <- prev_df %>% 
      mutate(weights = w_vec) %>% 
      mutate(epa_weighted = new_epa * weights)
    
    # what is the new fga_avg and go_avg?
    go_avg <- boot_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "go") %>% 
      pull(avg_epa)
    
    fga_avg <- boot_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "fga") %>% 
      pull(avg_epa)
    
    current_diff <- fga_avg - go_avg
    
    # save the bootstrap if the mean is going in the right direction
    if(current_diff < prev_diff) {
      
      prev_df <- boot_df
      
      # update number of bootstrap samples saved
      j <- j + 1
      
      # calculate and save the difference
      diff[j] <- sqrt(sum((rep(1, nrow(sub_df)) - prev_df$epa_weighted)^2))
      
      # save current boostrap's metrics
      avg_go[j] <- go_avg
      avg_fga[j] <- fga_avg
      ogavg_go[j] <- oggo_avg
      ogavg_fga[j] <- ogfga_avg
      
    }
    else {
      
      # undo the weighting vector back to the way it was
      w_vec[boot_ind] <- w_vec[boot_ind] - 1
      
    }
    
    # update number of bootstrap samples taken
    i <- i + 1
    
  }
  tictoc::toc()
  
  summary_df <- tibble(
    iteration = 1:j,
    diff = diff,
    avg_fga = avg_fga,
    avg_go = avg_go,
    ogavg_fga = ogavg_fga,
    ogavg_go = ogavg_go)
  
  # Take the new data frame for this bin and substitute it into the full data
  onechange_df <- new_full_df %>% 
    anti_join(sub_df) %>% 
    bind_rows(prev_df) %>% 
    # Substitute the epa_weighted NAs for the normal new_epa
    mutate(epa_weighted = ifelse(is.na(epa_weighted), new_epa, epa_weighted))
  
  # Save change in the new_full_df
  new_full_df <- onechange_df
}

# Visualize the new "data"
plot_df <- new_full_df %>%
  #filter(!is.na(fourth_down_bot)) %>% 
  group_by(yardline_100_group, ydstogo_group, fourth_down_decision) %>% 
  summarise(mean_epa = mean(epa_weighted),
            n = n()) %>% 
  ungroup() %>%
  filter(fourth_down_decision != "idk") %>% 
  #filter(n > 30) %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  slice_max(mean_epa, n = 2) %>%
  mutate(diff = max(mean_epa) - min(mean_epa)) %>% 
  slice_max(mean_epa) %>% 
  ungroup()

plot_df %>% 
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
       title = "What coaches \"should\" be doing",
       subtitle = "Based on our new epa value")



## Change from "fga" to "punt"
# n = 2

filtered_change_df <- change_df %>% 
  filter(change_from == "fga" & change_to == "punt")

for (k in 1:nrow(filtered_change_df)) {
  
  sub_df <- clean_pbp_sub2 %>% 
    filter(ydstogo_group == filtered_change_df$ydstogo_group[k] & 
             yardline_100_group == filtered_change_df$yardline_100_group[k])
  
  # In the following example data frame we want 
  eval_df <- sub_df %>% 
    group_by(fourth_down_decision) %>% 
    summarise(n = n(),
              avg_epa = mean(new_epa)) %>% 
    ungroup()
  
  # Original averages for each decision based on epa
  punt_avg <- eval_df %>% filter(fourth_down_decision == "punt") %>% pull(avg_epa)
  fga_avg <- eval_df %>% filter(fourth_down_decision == "fga") %>% pull(avg_epa)
  
  # Save original averages for each decision
  ogpunt_avg <- punt_avg
  ogfga_avg <- fga_avg
  
  # keep track of how many bootstrap samples are taken
  i <- 0
  
  # keep track of how many bootstrap samples are saved
  j <- 0
  
  # keep track of Euclidean distance
  diff <- c()
  
  # keep track of the averages
  avg_punt <- c()
  avg_fga <- c()
  ogavg_punt <- c()
  ogavg_fga <- c()
  
  # place holder for "previous" data set
  prev_df <- sub_df %>% 
    mutate(weights = 1,
           epa_weighted = new_epa)
  
  # Original weight vectors
  w_vec <- rep(1, nrow(sub_df))
  
  tictoc::tic()
  while(punt_avg < fga_avg) {
    
    # place holder metric for the previously used data set (or bootstrapped set)
    prev_fga_avg <- prev_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "fga") %>% 
      pull(avg_epa)
    
    prev_punt_avg <- prev_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "punt") %>% 
      pull(avg_epa)
    
    prev_diff <- prev_fga_avg - prev_punt_avg
    
    # bootstrap sample from the current data set
    boot_ind <- sample(1:nrow(prev_df), size = 1, replace = TRUE)
    
    w_vec[boot_ind] <- w_vec[boot_ind] + 1
    
    boot_df <- prev_df %>% 
      mutate(weights = w_vec) %>% 
      mutate(epa_weighted = new_epa * weights)
    
    # what is the new fga_avg and go_avg?
    punt_avg <- boot_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "punt") %>% 
      pull(avg_epa)
    
    fga_avg <- boot_df %>% 
      group_by(fourth_down_decision) %>% 
      summarise(n = n(),
                avg_epa = mean(epa_weighted)) %>% 
      ungroup() %>% 
      filter(fourth_down_decision == "fga") %>% 
      pull(avg_epa)
    
    current_diff <- fga_avg - punt_avg
    
    # save the bootstrap if the mean is going in the right direction
    if(current_diff < prev_diff) {
      
      prev_df <- boot_df
      
      # update number of bootstrap samples saved
      j <- j + 1
      
      # calculate and save the difference
      diff[j] <- sqrt(sum((rep(1, nrow(sub_df)) - prev_df$epa_weighted)^2))
      
      # save current boostrap's metrics
      avg_punt[j] <- punt_avg
      avg_fga[j] <- fga_avg
      ogavg_punt[j] <- ogpunt_avg
      ogavg_fga[j] <- ogfga_avg
      
    }
    else {
      
      # undo the weighting vector back to the way it was
      w_vec[boot_ind] <- w_vec[boot_ind] - 1
      
    }
    
    # update number of bootstrap samples taken
    i <- i + 1
    
  }
  tictoc::toc()
  
  summary_df <- tibble(
    iteration = 1:j,
    diff = diff,
    avg_punt = avg_punt,
    avg_fga = avg_fga,
    ogavg_punt = ogavg_punt,
    ogavg_fga = ogavg_fga)
  
  # Take the new data frame for this bin and substitute it into the full data
  onechange_df <- new_full_df %>% 
    anti_join(sub_df) %>% 
    bind_rows(prev_df) %>% 
    # Substitute the epa_weighted NAs for the normal new_epa
    mutate(epa_weighted = ifelse(is.na(epa_weighted), new_epa, epa_weighted))
  
  # Save change in the new_full_df
  new_full_df <- onechange_df
}


# Visualize the new "data"
plot_df <- new_full_df %>%
  #filter(!is.na(fourth_down_bot)) %>% 
  group_by(yardline_100_group, ydstogo_group, fourth_down_decision) %>% 
  summarise(mean_epa = mean(epa_weighted),
            n = n()) %>% 
  ungroup() %>%
  filter(fourth_down_decision != "idk") %>% 
  #filter(n > 30) %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  slice_max(mean_epa, n = 2) %>%
  mutate(diff = max(mean_epa) - min(mean_epa)) %>% 
  slice_max(mean_epa) %>% 
  ungroup()

plot_df %>% 
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
       title = "What coaches \"should\" be doing",
       subtitle = "Based on our new epa value")

# I need to make on correction manually for one bin because of it's "interesting" nature

new_full_df <- new_full_df %>% 
  mutate(weights = ifelse(ydstogo_group == "7" & 
                            yardline_100_group == "41-50" & 
                            fourth_down_decision == "fga" &
                            new_epa < 0,
                          2, 
                          weights)) %>% 
  mutate(epa_weighted = ifelse(ydstogo_group == "7" & 
                                 yardline_100_group == "41-50" & 
                                 fourth_down_decision == "fga" &
                                 new_epa < 0,
                               weights * new_epa,
                               epa_weighted)) %>% 
  # Fix so that data points without weights have a weight of 1
  mutate(weights = ifelse(is.na(weights), 1, weights))

# Visualize the new "data"
plot_df <- new_full_df %>%
  #filter(!is.na(fourth_down_bot)) %>% 
  group_by(yardline_100_group, ydstogo_group, fourth_down_decision) %>% 
  summarise(mean_epa = mean(epa_weighted),
            n = n()) %>% 
  ungroup() %>%
  filter(fourth_down_decision != "idk") %>% 
  #filter(n > 30) %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  slice_max(mean_epa, n = 2) %>%
  mutate(diff = max(mean_epa) - min(mean_epa)) %>% 
  slice_max(mean_epa) %>% 
  ungroup()

save_plot <- plot_df %>% 
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
       title = "What coaches \"should\" be doing",
       subtitle = "Based on our new epa value")

rand_num <- round(runif(1, min = 1, max = 10000))

ggsave(file = paste0("new_full_plot_", rand_num, ".png"),
       plot = save_plot)

write_csv(new_full_df %>% 
            dplyr::select(game_id, season, home_team, away_team, posteam, coach_name, 
                          week, defteam, game_date, quarter_seconds_remaining, 
                          half_seconds_remaining, game_half, qtr, down, goal_to_go, 
                          time, desc, ep, epa, wp, wpa, fourth_down_decision, 
                          ydstogo_group, yardline_100_group, fourth_down_bot, new_ep, 
                          new_epa, weights, epa_weighted),
          paste0("new_full_df_", rand_num, ".csv"))

}