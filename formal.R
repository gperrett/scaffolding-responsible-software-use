library(tidyverse)
library(dbarts)
library(ggridges)

source('clean.R')
source('estimate_cate.R')
library(dbarts)
library(tidyverse)
names(dat)


dat$thinkCausal <- ifelse(dat$treatment == 1, 1, 0)
dat$bartCause <- ifelse(dat$treatment == 2, 1, 0)
dat$CYOA <- ifelse(dat$treatment == 3, 1, 0)


dat$use_causal <- as.factor(dat$use_causal)
dat$use_stats <- as.factor(dat$use_stats)
dat$understand_mathcing <- as.factor(dat$understand_mathcing)
dat$understand_bart <- as.factor(dat$understand_bart)
dat$understand_estimands <- as.factor(dat$understand_estimands)
dat$understand_ml <- as.factor(dat$understand_ml)
dat$understand_po <- as.factor(dat$understand_po)
dat$understand_regression <- dat$understand_regression


# create indicators
dat$stats_classes <- ifelse(dat$stats_classes == "9+", "9", dat$stats_classes)
dat$stats_classes <- ifelse(dat$stats_classes == "3-4", "3_4", dat$stats_classes)
dat$stats_classes <- ifelse(dat$stats_classes == "1-2", "1_2", dat$stats_classes)
dat$stats_classes <- ifelse(dat$stats_classes == "7-8", "7_8", dat$stats_classes)
dat$stats_classes <- ifelse(dat$stats_classes == "5-6", "5_6", dat$stats_classes)
dat$language_preference <- ifelse(dat$language_preference == 'Running data analyses is not something I am currently actively engaged in', 'other', dat$language_preference)
dat$degree_in <- ifelse(dat$degree_in == 'N/A', 'none', dat$degree_in)

dat <-
  fastDummies::dummy_cols(
    .data = dat,
    select_columns = c(
      'language_preference',
      'hill',
      'R_regression',
      'R_documentation',
      'education',
      'stats_classes',
      'degree_in'
    ),
    remove_selected_columns = T
  )


X <- dat |>
  select(
    thinkCausal, 
    bartCause, 
    CYOA, 
    use_causal,
    use_stats,
    understand_po,
    understand_estimands,
    understand_regression,
    understand_mathcing,
    understand_ml,
    understand_bart,
    contains('degree_in'),
    contains('stats_classes'),
    contains('education'),
    contains('R_documentation'),
    contains('R_regression'),
    contains('hill'),
    contains('language_preference')
  )


# impute outcomes
df_test <- X[is.na(dat$distance_satt),]
df_train <- dat
eq <- paste0('standard_distance_satt~ ', paste0(names(X), collapse = '+'))
fit <- dbarts::bart2(as.formula(eq), data = df_train, test = df_test)
impute_bias_post <- extract(fit, 'ev', sample = 'test')

results_standard_distance_satt <- lapply(1:2000, function(i){
  df_train[is.na(dat$distance_satt),'standard_distance_satt'] <- impute_bias_post[i,]
  df_test <- rbind(X, X, X)
  # 
  df_test[1:71 ,'thinkCausal'] <- 1
  df_test[1:71 ,'bartCause'] <- 0
  df_test[1:71 ,'CYOA'] <- 0
  # 
  df_test[72:142,'thinkCausal'] <- 0
  df_test[72:142,'bartCause'] <- 1
  df_test[72:142,'CYOA'] <- 0
  # 
  df_test[143:213,'thinkCausal'] <- 0
  df_test[143:213,'bartCause'] <- 0
  df_test[143:213,'CYOA'] <- 1
  
  
  # bias from SATE
  eq <- paste0('standard_distance_satt~ ', paste0(names(X), collapse = '+'))
  fit <- dbarts::bart2(as.formula(eq), data = df_train, test = df_test)
  post <- extract(fit, 'ev', sample = 'test')
  
  icate1 <- post[, 1:71] - post[, 72:142]
  bias1 <- apply(icate1, 1, mean)
  
  icate2 <- post[, 1:71] - post[, 143:213]
  bias2 <- apply(icate2, 1, mean)
  
  icate3 <- post[, 72:142] - post[, 143:213]
  bias3 <- apply(icate3, 1, mean)
  
  data.frame(bias1, bias2, bias3)
  
})

results_standard_distance_satt <- results_standard_distance_satt |>
  bind_rows() 

bias1 <- results_standard_distance_satt$bias1
bias2 <- results_standard_distance_satt$bias2
bias3 <- results_standard_distance_satt$bias3


## standardized efficency (with imputed missing outcomes)
df_test <- X[is.na(dat$standard_satt_ci_length),]
df_train <- dat
eq <- paste0('standard_satt_ci_length~ ', paste0(names(X), collapse = '+'))
fit <- dbarts::bart2(as.formula(eq), data = df_train, test = df_test)
impute_eff_post <- extract(fit, 'ev', sample = 'test')

results_standard_efficency_satt <- lapply(1:2000, function(i){
  df_train[is.na(dat$standard_satt_ci_length),'standard_satt_ci_length'] <- impute_eff_post[i,]
  df_test <- rbind(X, X, X)
  # 
  df_test[1:71 ,'thinkCausal'] <- 1
  df_test[1:71 ,'bartCause'] <- 0
  df_test[1:71 ,'CYOA'] <- 0
  # 
  df_test[72:142,'thinkCausal'] <- 0
  df_test[72:142,'bartCause'] <- 1
  df_test[72:142,'CYOA'] <- 0
  # 
  df_test[143:213,'thinkCausal'] <- 0
  df_test[143:213,'bartCause'] <- 0
  df_test[143:213,'CYOA'] <- 1
  
  
  # efficency of SATE estimate
  eq <- paste0('standard_satt_ci_length ~ ', paste0(names(X), collapse = '+'))
  fit <- dbarts::bart2(as.formula(eq), data = dat, test = df_test)
  
  post <- extract(fit, 'ev', sample = 'test')
  icate1 <- post[, 1:71] - post[, 72:142]
  efficency1 <- apply(icate1, 1, mean)
  
  icate2 <- post[, 1:71] - post[, 143:213]
  efficency2 <- apply(icate2, 1, mean)
  
  icate3 <- post[, 72:142] - post[, 143:213]
  efficency3 <- apply(icate3, 1, mean)
  
  
  data.frame(efficency1, efficency2, efficency3)
  
})

results_standard_efficency_satt <- results_standard_efficency_satt |>
  bind_rows() 

efficency1 <- results_standard_efficency_satt$efficency1
efficency2 <- results_standard_efficency_satt$efficency2
efficency3 <- results_standard_efficency_satt$efficency3

# coverage
df_test <- rbind(X, X, X)
# 
df_test[1:71 ,'thinkCausal'] <- 1
df_test[1:71 ,'bartCause'] <- 0
df_test[1:71 ,'CYOA'] <- 0
# 
df_test[72:142,'thinkCausal'] <- 0
df_test[72:142,'bartCause'] <- 1
df_test[72:142,'CYOA'] <- 0
# 
df_test[143:213,'thinkCausal'] <- 0
df_test[143:213,'bartCause'] <- 0
df_test[143:213,'CYOA'] <- 1

eq <- paste0('cover_satt ~ ', paste0(names(X), collapse = '+'))
fit <- dbarts::bart2(as.formula(eq), data = dat, test = df_test)

post <- extract(fit, 'ev', sample = 'test')
icate1 <- post[, 1:71] - post[, 72:142]
cover1 <- apply(icate1, 1, mean)

icate2 <- post[, 1:71] - post[, 143:213]
cover2 <- apply(icate2, 1, mean)

icate3 <- post[, 72:142] - post[, 143:213]
cover3 <- apply(icate3, 1, mean)



# confidence in analysis 
eq <- paste0('confidence ~', paste0(names(X), collapse = '+'))
fit <- dbarts::bart2(as.formula(eq), data = dat, test = df_test)

post <- extract(fit, 'ev', sample = 'test')
icate1 <- post[, 1:71] - post[, 72:142]
confidence1 <- apply(icate1, 1, mean)

icate2 <- post[, 1:71] - post[, 143:213]
confidence2 <- apply(icate2, 1, mean)

icate3 <- post[, 72:142] - post[, 143:213]
confidence3 <- apply(icate3, 1, mean)


# ease of use
eq <- paste0('ease ~', paste0(names(X), collapse = '+'))
fit <- dbarts::bart2(as.formula(eq), data = dat, test = df_test)

post <- extract(fit, 'ev', sample = 'test')
icate1 <- post[, 1:71] - post[, 72:142]
ease1 <- apply(icate1, 1, mean)

icate2 <- post[, 1:71] - post[, 143:213]
ease2 <- apply(icate2, 1, mean)

icate3 <- post[, 72:142] - post[, 143:213]
ease3 <- apply(icate3, 1, mean)



rbind(
tibble(bias1, bias2, bias3, 
       efficency1, efficency2, efficency3) |>
pivot_longer(1:6),
tibble(cover1, cover2, cover3, 
       confidence1, confidence2, confidence3, 
       ease1, ease2, ease3) |>
  pivot_longer(1:9)) |>
  mutate(contrast = 
           ifelse(stringr::str_detect(name, '1'), 
                  'thinkCausal - bartCause', 
                  ifelse(stringr::str_detect(name, '2'), 
                         'thinkCausal - CYOA', 
                         'bartCause - CYOA'))) |>
  mutate(
    index = abs(as.numeric(stringr::str_sub(name, -1)) - 4),
    name = stringr::str_sub(name, end = -2)
    ) |>
  group_by(name, contrast) |>
  mutate(mean = mean(value)) |>
  mutate(name = case_when(
    name == 'cover' ~ 'Covered SATT', 
    name == 'efficency' ~ 'SATT 95% Interval Length', 
    name == 'bias' ~ 'Standardized Distance from SATT', 
    name == 'confidence' ~ 'Confidence', 
    name == 'ease' ~ 'Ease of Use'
  )) |>
  mutate(name = factor(name, levels = c('Standardized Distance from SATT', 'Covered SATT', 'SATT 95% Interval Length', 'Confidence', 'Ease of Use'))) |>
  ggplot(aes(x = value, y = reorder(contrast, index), fill = factor(stat(quantile)))) + 
  stat_density_ridges(scale = .85, 
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      rel_min_height = 0.0001) + 
  scale_fill_manual(
    name = "Probability", values = c("white", "grey", "white"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  ) + 
  #scale_fill_manual(values = c('white', 'grey')) + 
  #coord_flip() +
  facet_wrap(~name, scales = 'free_x', ncol = 5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  labs(x = 'Conditional Average Treatment Effect (CATE)', y = 'Contrast') + 
  theme_bw() + 
  theme(legend.position = 'none', 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave('fig4.pdf', width = 13.5, height = 3)


## contrast of effects
tibble(`thinkCausal - bartCause` = cover1, 
       `thinkCausal - CYOA` = cover2, 
       `bartCause - CYOA` = cover3) |>
  pivot_longer(1:3) |>
  rename(cover = value) |>
  mutate(name = factor(name, 
                       levels = c('thinkCausal - bartCause', 
                                  'thinkCausal - CYOA', 
                                  'bartCause - CYOA'
                       ))) |>
  ggplot(aes(name, value)) + 
  geom_boxplot() + 
  theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(x = 'Contrast', y = 'CATE')

ggsave('fig4.pdf', width = 15, height = 3)








