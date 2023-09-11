# Data script

# Recreate Dr Sandholtz's Code (minor tweaks by me to help it make more sense)

############
## Libraries
############

library(tidyverse)
library(nflfastR)
#library(nfl4th)

#######
## Data
#######

# You can run this code to reload the data, but I saved it as .rds
# so that we don't have to wait for the nflfastR file to load everything (line 403)

# pbp_df <- nflfastR::load_pbp(seasons = 2014:2021) %>%
#   filter(down == 4) %>% 
#   nfl4th::add_4th_probs()

############
## Constants
############

ydstogo_bin <- 1
MAX_ydstogo <- 10
yardline_100_bin <- 10
MAX_yardline_100 <- 91

down_label <- c(1,2,3,4)
ydstogo_label <- c("1",   "2"  , "3" ,  "4"  , "5"  , 
                   "6"  , "7" ,  "8"  , "9"  , "10+")
yardline_label <- c("1-10", "11-20","21-30", "31-40", "41-50", 
                    "51-60", "61-70", "71-80", "81-90", "91+")
fix_yardline_label <- c("1-10", "11-20","21-30", "31-40", "41-50", 
                        "51-60", "61-70", "71-80", "81-90", "91-99")

score_states <- c("touchdown", "field_goal","safety")
kickoff_lines <- c(20,25,30,35,40,50)
fourth_actions <- c("go", "fga", "punt")

touchdown_value <- 6.95

#################
## Misc Functions
#################

# These come from Dr Sandholtz

yard_cut <- function(x,
                     lower = 1,
                     upper,
                     by = 10,
                     sep = "-",
                     above_char = "+") {
  
  if(lower == by) {
    labs <- c(paste0(seq(lower, upper - by, by = by)),
              paste(upper, above_char, sep = ""))
  } else {
    labs <- c(paste(seq(lower, upper - by, by = by),
                    seq(lower + by - 1, upper - 1, by = by),
                    sep = sep),
              paste(upper, above_char, sep = ""))
  }
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
  
}

clean_data <- function(data,
                       ydstogo_bin,
                       yardline_100_bin,
                       max_ydstogo,
                       max_yardline_100) {
  scoring_states = c(
    "touchdown",
    "field_goal",
    "safety",
    "opp_touchdown",
    "opp_field_goal",
    "opp_safety"
  )
  
  kickoff_states = c("kickoff1",
                     "kickoff2",
                     "kickoff3",
                     "kickoff4",
                     "kickoff5",
                     "kickoff6")
  
  # Remove PATs and any missing play_type:
  removed_pbp_data <- data %>%
    filter(
      !is.na(play_type),
      !is.na(ydstogo),
      !is.na(yardline_100),
      !(play_type %in% c("extra_point")),
      two_point_attempt == 0,
      timeout == 0 | (timeout == 1 & play_type != "no_play"),
      extra_point_attempt == 0,
      ydstogo >= 0
    ) %>%
    mutate(
      ydstogo_group = yard_cut(ydstogo, upper = max_ydstogo, by = ydstogo_bin),
      yardline_100_group = yard_cut(yardline_100, upper = max_yardline_100, by = yardline_100_bin)
    )
  
  # cleaning & generating features
  clean_pbp_data =
    removed_pbp_data %>%
    group_by(game_id, game_half) %>%
    mutate(
      play_index = row_number(),
      drive_length = n(),
      next_posteam = lead(posteam),
      previous_type = lag(play_type),
      next_down = lead(down),
      next_ydstogo = lead(ydstogo),
      next_yardline_100 = lead(yardline_100),
      next_ydstogo_group = lead(ydstogo_group),
      next_yardline_100_group = lead(yardline_100_group)
    ) %>%
    ungroup() %>%
    # combine state, down and yardline_100 to one variable
    unite(
      play_state,
      down,
      ydstogo_group,
      yardline_100_group,
      sep = "_",
      remove = FALSE
    ) %>%
    # set up the scoring states as absorption states
    mutate(
      absorption_state = case_when(
        # define Team B gets touchdown after Team A kickoff
        kickoff_attempt == 1 &
          td_team == posteam & touchdown == 1 ~ "opp_touchdown",
        td_team != posteam & touchdown == 1 ~ "opp_touchdown",
        td_team == posteam & touchdown == 1 ~ "touchdown",
        kickoff_attempt == 1 ~ "kickoff",
        field_goal_result == "made" ~ "field_goal",
        safety == 1 ~ "safety",
        TRUE ~ "end_of_time"
      )
    ) %>%
    # convert kickoff to kickoff at specific yardline
    mutate(
      play_state =
        case_when(
          kickoff_attempt == 1 & yardline_100 <= 20 ~ "kickoff1",
          kickoff_attempt == 1 &
            yardline_100 == 25 ~ "kickoff2",
          kickoff_attempt == 1 &
            yardline_100 == 30 ~ "kickoff3",
          kickoff_attempt == 1 &
            yardline_100 == 35 ~ "kickoff4",
          kickoff_attempt == 1 &
            yardline_100 == 40 ~ "kickoff5",
          kickoff_attempt == 1 &
            yardline_100 >= 50 ~ "kickoff6",
          TRUE ~ play_state
        )
    ) %>%
    # combine next_state, next_down and next_yardline_100 to one variable
    unite(
      next_play_state,
      next_down,
      next_ydstogo_group,
      next_yardline_100_group,
      sep = "_",
      remove = FALSE
    ) %>%
    # define next_play_state with off and opp
    mutate(next_play_state_2 = ifelse(
      posteam == next_posteam,
      glue::glue("off_{next_play_state}"),
      glue::glue("opp_{next_play_state}")
    )) %>%
    mutate(
      next_play_state_2 = case_when(
        # KICKOFF to OTHER TEAM
        kickoff_attempt == 1 &
          posteam == next_posteam ~ glue::glue("opp_{next_play_state}"),
        # KICKOFF to ONSIDES/FUMBLE RECOVERY
        kickoff_attempt == 1 &
          posteam != next_posteam ~ glue::glue("off_{next_play_state}"),
        TRUE ~ next_play_state_2
      )
    ) %>%
    # convert kickoff to kickoff at specific yardline
    mutate(
      absorption_state =
        case_when(
          absorption_state == "kickoff" & yardline_100 <= 20 ~ "kickoff1",
          absorption_state == "kickoff" &
            yardline_100 == 25 ~ "kickoff2",
          absorption_state == "kickoff" &
            yardline_100 == 30 ~ "kickoff3",
          absorption_state == "kickoff" &
            yardline_100 == 35 ~ "kickoff4",
          absorption_state == "kickoff" &
            yardline_100 == 40 ~ "kickoff5",
          absorption_state == "kickoff" &
            yardline_100 >= 50 ~ "kickoff6",
          TRUE ~ absorption_state
        )
    ) %>%
    mutate(
      prev_absorption_state = lag(absorption_state),
      next_absorption_state = lead(absorption_state)
    ) %>%
    mutate(
      next_play_state_2 =
        ifelse(
          absorption_state == "end_of_time" &
            (
              next_absorption_state %in% c(
                "kickoff1",
                "kickoff2",
                "kickoff3",
                "kickoff4",
                "kickoff5",
                "kickoff6"
              )
            ),
          yes = next_absorption_state,
          no = next_play_state_2
        )
    ) %>%
    mutate(
      prev_absorption_state = lag(absorption_state),
      next_absorption_state = lead(absorption_state)
    ) %>%
    # set next_play_state_3 to be absorption states such as touchdown, field_goal etc
    mutate(next_play_state_3 = ifelse(
      !(absorption_state) %in% c(
        "end_of_time",
        "kickoff1",
        "kickoff2",
        "kickoff3",
        "kickoff4",
        "kickoff5",
        "kickoff6"
      ),
      yes = (absorption_state),
      no = next_play_state_2
    )) %>%
    mutate(
      next_absorption_state = ifelse(
        absorption_state %in% c("field_goal",
                                "touchdown",
                                "opp_touchdown",
                                "safety"),
        yes = "kickoff",
        no = next_play_state_2
      )
    ) %>%
    mutate(
      next_absorption_state =
        case_when(
          next_absorption_state == "kickoff" &
            lead(yardline_100) <= 20 ~ "kickoff1",
          next_absorption_state == "kickoff" &
            lead(yardline_100) == 25 ~ "kickoff2",
          next_absorption_state == "kickoff" &
            lead(yardline_100) == 30 ~ "kickoff3",
          next_absorption_state == "kickoff" &
            lead(yardline_100) == 35 ~ "kickoff4",
          next_absorption_state == "kickoff" &
            lead(yardline_100) == 40 ~ "kickoff5",
          next_absorption_state == "kickoff" &
            lead(yardline_100) >= 50 ~ "kickoff6",
          TRUE ~ next_absorption_state
        ),
      wp_group = as.character(cut(
        wp,
        c(0, 0.2, 0.8, 1),
        include.lowest = F,
        right = T
      ))
    )
  
  # remove the end of period plays
  clean_pbp_data = clean_pbp_data %>%
    filter(!(
      str_detect(next_play_state_3, "kickoff") &
        next_play_state == "NA_NA_NA"
    ))
  
  return(clean_pbp_data)
}

assign_fourth_down_decision <- function(data) {
  
  data <- data %>% 
    mutate(fourth_down_decision = case_when(
      is.na(down) ~ NA_character_,
      down != 4 ~ NA_character_,
      # remove "qb_kneel"
      down == 4 & (play_type %in% c("run", "pass", "qb_spike", "no_play") &
                     (!str_detect(tolower(desc), "field_goal")) &
                     (!str_detect(tolower(desc), "punt"))
      ) ~ "go",
      down == 4 & (play_type %in% c("field_goal") |
                     (play_type %in% "no_play" & str_detect(tolower(desc), "field goal"))
      ) ~ "fga",
      down == 4 & (play_type %in% c("punt") |
                     (play_type %in% "no_play" & str_detect(tolower(desc), "punt"))
      ) ~ "punt",
      TRUE ~ "idk"
    )) %>% 
    # fake punts are assigned to go for it
    mutate(fourth_down_decision = ifelse(fourth_down_decision == "idk" & 
                                           play_type %in% c("run", "pass"),
                                         yes = "go",
                                         no = fourth_down_decision))
  
  return(data)
  
}

plot_decision_map <- function(data,
                              fill_variable = "discrete",
                              legend_name,
                              ...) {
  
  if(fill_variable == "discrete") {
    p <- data %>% 
      ggplot(aes(x = yardline_100_group, y = ydstogo_group, ...)) +
      geom_tile(color = "white", size = 0.1) + 
      scale_fill_manual(
        name = legend_name,
        #name = bquote(a^{"*"}~(sigma~","~hat(q)[tau]^{bar(pi)})),
        values = c("#d53e4f", "gold", "#2C7FB8"),
        #na.value = "grey90",
        limits = c("GO", "FGA", "PUNT")
      ) +
      theme_classic() +
      labs(x = "Yards to opponent endzone",
           y = "Yards to go") +
      coord_fixed()
  } else if(fill_variable == "continuous") {
    p <- data %>% 
      mutate(ydstogo_group = factor(ydstogo_group, levels = ydstogo_label)) %>% 
      ggplot(aes(x = ydstogo_group, y = yardline_100_group, ...)) +
      geom_tile(color = "white",
                size = 0.1) +
      scale_fill_gradient2(
        name = "value",
        low = "blue",
        mid = "white",
        midpoint = 0,
        high = "red",
        #na.value = "grey90"
      ) +
      scale_y_discrete(labels = as.character(fix_yardline_label)) +
      theme_classic() +
      coord_flip() +
      labs(x = "Yards to go",
           y = "Yards to opponent endzone")
  } else {
    print("check your spelling...")
  }
  
  p <- p +
    theme(plot.title = element_text(hjust = 0.5)) +
    # theme(legend.position = "none") +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1,
        size = 7
      ),
      axis.text.y = element_text(
        size = 7
      ),
      strip.text.y.right = element_text(angle = 90)
    )
  
  return(p)
  
}

#############
## Clean Data
#############

# clean_pbp_df <- clean_data(pbp_df,
#                            ydstogo_bin = ydstogo_bin,
#                            yardline_100_bin = yardline_100_bin,
#                            max_ydstogo = MAX_ydstogo,
#                            max_yardline_100 = MAX_yardline_100) %>%
#   assign_fourth_down_decision()

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

## Adjust the expected points for a play
# This is because we want EPA in general; 
# In the nflfastr data the EPA is adjusted for the decision made on fourth down
# For example, the EPA for a punt is based on the EPA model knowing they punted
# We want to find EPA agnostic of the decision made
# This is `new_epa`

clean_pbp_sub2 <- clean_pbp_sub %>% 
  group_by(yardline_100_group, ydstogo_group) %>% 
  mutate(new_ep = mean(ep, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(new_epa = ep + epa - new_ep)

# `clean_pbp_sub2` is the final data set used for the project