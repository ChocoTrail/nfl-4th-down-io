# Analyze IO data

library(tidyverse)
library(gt)
library(patchwork)
library(scales)

# be sure to change this so that the files with the IO data are available
setwd("io-data")

#######
## Data
#######

## Combine everything into one data frame

files <- list.files(pattern = "*.csv")

temp_num <- parse_number(files[1])

main_df <- read_csv(files[1]) %>% 
  dplyr::select(ydstogo_group,
                yardline_100_group,
                fourth_down_decision,
                desc,
                new_epa,
                ep,
                epa,
                weights
  ) %>% 
  rename_at(vars(weights), function(x) paste0(x, "_", temp_num))

for(i in 2:length(files)) {
  
  temp_num <- parse_number(files[i])
  
  current_df <- read_csv(files[i]) %>% 
    dplyr::select(weights) %>% 
    rename_at(vars(weights), function(x) paste0(x, "_", temp_num))
  
  main_df <- bind_cols(main_df, current_df)
  
}

##########
## Results
##########

## Look into some of the numbers:
main_df %>% 
  pivot_longer(cols = weights_11:weights_97, names_to = "weight_num") %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision, desc, new_epa) %>% 
  summarize(weight_avg = mean(value),
            weight_sd = sd(value)) %>% 
  ungroup() %>%
  mutate(epa_weighted = weight_avg * new_epa) %>%
  # stop here for some good insights
  group_by(ydstogo_group, yardline_100_group, fourth_down_decision) %>% 
  summarize(tot_avg_weight = sum(weight_avg),
            avg_avg_weight = mean(weight_avg),
            n = n())

## Figure 4.1 in the paper

main_df %>% 
  pivot_longer(cols = weights_11:weights_97, names_to = "weight_num") %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision, desc, new_epa) %>% 
  summarize(weight_avg = mean(value),
            weight_sd = sd(value)) %>%
  ungroup() %>% 
  mutate(success = ifelse(new_epa > 0, "success", "fail")) %>% 
  group_by(fourth_down_decision, success) %>% 
  summarize(avg_weight = mean(weight_avg),
            lwr_weight = quantile(weight_avg, probs = 0.025),
            upr_weight = quantile(weight_avg, probs = 0.975)) %>% 
  ungroup() %>% 
  filter(fourth_down_decision != "idk") %>% 
  ggplot(aes(x = success)) +
  geom_linerange(aes(ymin = lwr_weight, ymax = upr_weight),
                 linewidth = 2, alpha = 0.8, color = "steelblue4") +
  geom_point(aes(y = avg_weight), size = 4, color = "firebrick") +
  facet_wrap(~ fourth_down_decision,
             scales = "free_y") +
  ylim(1, NA) +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(x = NULL,
       y = "Weighting",
       title = "Average weighting for each decision",
       subtitle = "Bars show inner 95% range of values")

## Figure 4.2 in the paper
# Find the % of success and failure overall from the original data set
# Use data set from EDA.R titled clean_pbp_sub2

orig_tbl <- clean_pbp_sub2 %>% 
  group_by(fourth_down_decision) %>% 
  summarise(n = n(),
            success = sum(new_epa > 0)) %>% 
  ungroup() %>% 
  mutate(fail = n - success) %>% 
  filter(fourth_down_decision != "idk") %>% 
  mutate(success = success / n,
         fail = fail / n)

# Find the % of success and failure overall from newly weighted data set

weight_tbl <- main_df %>% 
  pivot_longer(cols = weights_11:weights_97, names_to = "weight_num") %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision, desc, new_epa) %>% 
  summarize(weight_avg = round(mean(value))) %>% 
  ungroup() %>%
  mutate(success_n = ifelse(new_epa > 0, weight_avg, 0),
         fail_n = ifelse(new_epa <= 0, weight_avg, 0)) %>% 
  group_by(fourth_down_decision) %>% 
  summarise(success = sum(success_n),
            fail = sum(fail_n)) %>% 
  filter(fourth_down_decision != "idk") %>% 
  mutate(n = success + fail) %>% 
  mutate(success = success / n,
         fail = fail / n)

# Combine two previous tables and make a gt output
dt_table <- orig_tbl %>% 
  relocate(n, .after = last_col()) %>% 
  mutate(`Data Set` = "Original") %>% 
  bind_rows(weight_tbl %>% 
              mutate(`Data Set` = "Reweighted")) %>% 
  rename("Decision" = fourth_down_decision,
         Success = success,
         Fail = fail) %>% 
  gt(rowname_col = "Decision") %>% 
  tab_row_group(
    label = "Reweighted",
    rows = `Data Set` == "Reweighted"
  ) %>% 
  tab_row_group(
    label = "Original",
    rows = `Data Set` == "Original"
  ) %>%
  cols_hide(`Data Set`) %>% 
  fmt_percent(
    columns = Success:Fail,
    decimals = 1
  ) %>% 
  fmt_number(
    columns = n,
    sep_mark = ",",
    decimals = 0
  ) %>% 
  tab_header(
    title = "Fourth down choices made",
    subtitle = "Based on EPA value on that play"
  )

## Figure 4.3 in the paper

p1 <- main_df %>% 
  pivot_longer(cols = weights_11:weights_97, names_to = "weight_num") %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision, desc, ep, epa) %>% 
  summarize(weight_avg = mean(value)) %>% 
  ungroup() %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision) %>% 
  mutate(EP2 = ep + mean(epa)) %>%
  ungroup() %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision) %>% 
  summarize(EP2 = mean(EP2),
            EP = mean(ep))  %>% 
  ungroup() %>% 
  filter(fourth_down_decision != "idk") %>% 
  mutate(ydstogo_group = as_factor(ydstogo_group)) %>%
  mutate(ydstogo_group = fct_relevel(ydstogo_group, "10+", after = Inf)) %>% 
  ggplot(aes(x = yardline_100_group,
             y = ydstogo_group,
             fill = EP2)) +
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
  facet_wrap(~ fourth_down_decision) +
  scale_fill_gradient2(low=('blue'), mid='white',midpoint = 0, high=('red'),
                       name = "EPA" ,na.value = "grey50") +
  labs(x = "Yards to opponent endzone",
       y = "Yards to go",
       title = "Original")


p2 <- main_df %>% 
  pivot_longer(cols = weights_11:weights_97, names_to = "weight_num") %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision, desc, ep, epa) %>% 
  summarize(weight_avg = mean(value)) %>% 
  ungroup() %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision) %>% 
  mutate(EP_w = ep + weighted.mean(epa, w = weight_avg)) %>%
  ungroup() %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision) %>% 
  summarize(EP_w = mean(EP_w))  %>% 
  ungroup() %>% 
  filter(fourth_down_decision != "idk") %>% 
  mutate(ydstogo_group = as_factor(ydstogo_group)) %>%
  mutate(ydstogo_group = fct_relevel(ydstogo_group, "10+", after = Inf)) %>% 
  ggplot(aes(x = yardline_100_group,
             y = ydstogo_group,
             fill = EP_w)) +
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
  facet_wrap(~ fourth_down_decision) +
  scale_fill_gradient2(low=('blue'), mid='white',midpoint = 0, high=('red'),
                       name = "EPA" ,na.value = "grey50") +
  labs(x = "Yards to opponent endzone",
       y = "Yards to go",
       title = "Weighted")

p3 <- main_df %>% 
  pivot_longer(cols = weights_11:weights_97, names_to = "weight_num") %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision, desc, ep, epa) %>% 
  summarize(weight_avg = mean(value)) %>% 
  ungroup() %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision) %>% 
  mutate(EP_w = ep + weighted.mean(epa, w = weight_avg),
         EP2 = ep + mean(epa)) %>%
  ungroup() %>%
  group_by(ydstogo_group, yardline_100_group, 
           fourth_down_decision) %>% 
  summarize(EP_w = mean(EP_w),
            EP2 = mean(EP2))  %>% 
  ungroup() %>% 
  mutate(Difference = EP_w - EP2) %>% 
  filter(fourth_down_decision != "idk") %>% 
  mutate(ydstogo_group = as_factor(ydstogo_group)) %>%
  mutate(ydstogo_group = fct_relevel(ydstogo_group, "10+", after = Inf)) %>% 
  ggplot(aes(x = yardline_100_group,
             y = ydstogo_group,
             fill = Difference)) +
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
  facet_wrap(~ fourth_down_decision) +
  scale_fill_gradient2(low=('blue'), mid='white',midpoint = 0, high=('red'),
                       name = "Difference" ,na.value = "grey50") +
  labs(x = "Yards to opponent endzone",
       y = "Yards to go",
       title = "Weighted - Original")

comp_plt <- p1 / p2 / p3