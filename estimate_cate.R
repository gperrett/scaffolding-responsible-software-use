estimate_cate <- function(formula){
  fit <- dbarts::bart2(as.formula(eq), data = dat, test = df_test, seed = 2)
  
  post <- extract(fit, 'ev', sample = 'test')
  
  icate1 <- post[, 1:71] - post[, 72:142]
  cate1 <- apply(icate1, 1, mean)
  
  icate2 <- post[, 1:71] - post[, 143:213]
  cate2 <- apply(icate2, 1, mean)
  
  icate3 <- post[, 72:142] - post[, 143:213]
  cate3 <- apply(icate3, 1, mean)
  
  
  
  tibble(est = c(mean(cate1), mean(cate2), mean(cate3)), 
         lci = c(quantile(cate1, probs = .025), quantile(cate2, probs = .025), quantile(cate3, probs = .025)), 
         uci = c(quantile(cate1, probs = .975), quantile(cate2, probs = .975), quantile(cate3, probs = .975)), 
         contrast = c('thinkCausal - bartCause', 'thinkCausal - COYA', 'bartCause - COYA')
  ) |>
    mutate(contrast = factor(contrast, levels = c('thinkCausal - COYA', 'thinkCausal - bartCause', 'bartCause - COYA')))
}



estimate_effect_size<- function(eq, y){
  fit <- dbarts::bart2(as.formula(eq), data = dat, test = df_test)
  
  post <- extract(fit, 'ev', sample = 'test')
  
  icate1 <- post[, 1:71] - post[, 72:142]
  cate1 <- apply(icate1, 1, mean)
  
  icate2 <- post[, 1:71] - post[, 143:213]
  cate2 <- apply(icate2, 1, mean)
  
  
  
  icate3 <- post[, 72:142] - post[, 143:213]
  cate3 <- apply(icate3, 1, mean)
  
  
  
  tibble(est = c(mean(cate1)/sd(y, na.rm = T), mean(cate2)/sd(y, na.rm = T), mean(cate3)/sd(y, na.rm = T)), 
         lci = c(quantile(cate1/sd(y, na.rm = T), probs = .025), quantile(cate2/sd(y, na.rm = T), probs = .025), quantile(cate3/sd(y, na.rm = T), probs = .025)), 
         uci = c(quantile(cate1/sd(y, na.rm = T), probs = .975), quantile(cate2/sd(y, na.rm = T), probs = .975), quantile(cate3/sd(y, na.rm = T), probs = .975)), 
         contrast = c('thinkCausal - bartCause', 'thinkCausal - COYA', 'bartCause - COYA')
  ) |>
    mutate(contrast = factor(contrast, levels = c('thinkCausal - COYA', 'thinkCausal - bartCause', 'bartCause - COYA')))
}
