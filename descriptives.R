library(tidyverse)
library(patchwork)

source('clean.R')


p1 <- dat |>
  select(degree_in, education, stats_classes, use_stats, use_causal) |>
  map_df(as.character) |>
  tidyr::pivot_longer(1:5) |>
  mutate(name = ifelse(name == 'degree_in', 'Degree in Progress', name), 
         name = ifelse(name == 'education', 'Highest Degree Completed', name), 
         name = ifelse(name == 'stats_classes', 'Prior Statistics Courses', name), 
         name = ifelse(name == 'use_stats', 'Use of Statistics', name),
         name = ifelse(name == 'use_causal', 'Use of Causal Inference', name)) |>
  mutate(value = ifelse(value == 'N/A', 'None', value)) |>
  mutate(order = case_when(
    value == 'None' ~ 1, 
    T ~ 2
  )) |>
  group_by(name, value, order) |>
  count() |>
  ggplot(aes(reorder(value, order), n)) +
  geom_col(col = 'black', fill = 'dark grey') + 
  facet_wrap(~name, scales = 'free_x', ncol = 5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  labs(y = 'Frequency', x = NULL)


ggsave('fig1.pdf', width = 10, height = 3)





# dat |> 
#   select(use_causal, use_stats) |>
#   rename(`causal inference` = use_causal, 
#          `statistics` = use_stats
#          ) |>
#   pivot_longer(1:2) |>
#   ggplot(aes(value)) + 
#   geom_histogram() + 
#   facet_wrap(~name) + 
#   theme_bw() + 
#   theme(panel.grid = element_blank()) + 
#   labs(x = 'Use')

dat |>
  select(contains('understand')) |>
  rename(`Potential Outcomes` = understand_po, 
         BART = understand_bart, 
         Estimands = understand_estimands, 
         IPTW = understand_iptw,
          PSM = understand_mathcing, 
         Regression = understand_regression, 
         `Machine Learning` = understand_ml) |>
  pivot_longer(1:7) |>
  mutate(name = factor(name, levels = c( 
                                        'Potential Outcomes', 
                                        'Estimands', 
                                        'Regression',
                                        'PSM', 
                                        'IPTW',
                                        'Machine Learning', 'BART'))) |>
  group_by(name, value) |>
  count() |>
  ggplot(aes(value, n)) + 
  geom_col(color = 'black', fill = 'dark grey') + 
  facet_wrap(~name, ncol = 7) +  
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = 'Understanding', y = 'Frequency')
  
ggsave('fig2.pdf', width = 10, height = 3.5)

rm(list=ls())

source('clean.R')
dat |>
  select(treatment, standard_distance_satt,  satt_ci_length, cover_satt, confidence, ease) |>
  pivot_longer(2:6) |>
  mutate(z = case_when(
    treatment == 1 ~ 'thinkCausal', 
    treatment == 2 ~ 'bartCause', 
    treatment == 3 ~ 'CYOA'
  )) |>
  mutate(name = case_when(
    name == 'cover_satt' ~ 'Covered SATT', 
    name == 'satt_ci_length' ~ 'SATT 95% Interval Length', 
    name == 'standard_distance_satt' ~ 'Standardized Distance from SATT', 
    name == 'confidence' ~ 'Confidence', 
    name == 'ease' ~ 'Ease of Use'
  )) |>
  mutate(name = factor(name, levels = c('Standardized Distance from SATT', 'Covered SATT', 'SATT 95% Interval Length', 'Confidence', 'Ease of Use'))) |>
  group_by(treatment, name,z) |>
  summarise(point = mean(value, na.rm = T)) |>
  ggplot(aes(reorder(z, treatment), point)) + 
  geom_col(col = 'black', fill = 'dark grey') + 
  facet_wrap(~name, scales = 'free', ncol = 5) + 
  labs(x = NULL, y = 'Average') + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave('fig3.pdf', width = 12.5, height = 3)
rm(list=ls())



