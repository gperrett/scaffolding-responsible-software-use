library(aciccomp2016)
library(tidyverse)

post <- readr::read_csv('thinkCausal Evaluation Post-Study Form (Responses) - Form Responses 1-2.csv')
pre <- readr::read_csv('thinkCausal Evaluation Pre-Study Form (Responses) - Form Responses 1.csv')


pre <- pre |>
  rename(role = `What is your current role? (check all that apply)`, 
         education = `What is the highest educational qualification that you have already obtained?`, 
         degree_in = `If you are currently pursuing a degree, please specify the level below.`, 
         hill = `Did you take Prof. Jennifer Hill's causal inference course?`, 
         stats_classes = `How many other statistics courses have you taken?`, 
         use_causal = `How often do you use causal inference in your job or research?`, 
         use_stats = `How often do you use statistics in your job or research?`,
         language_preference = `Which language do you use most often in your job or research for data analysis?`, 
         R_regression = `Do you have experience fitting regression models in R?`, 
         R_documentation = `Do you have experience using R documentation for libraries and functions that are new to you?`, 
         #estimands = `Please rate your understanding of the concept of causal estimands (SATT, SATE, CACE, etc).`, 
         block = `Block; A=rand from three treatments, b=rand from either 1 or 3`, 
         treatment = `Randomized to treatment arm:`, 
         understand_ml = `Please rate your understanding of the machine learning approaches to prediction.`, 
         understand_bart = `Please rate your understanding of the concept of Bayesian Additive Regression Trees.`, 
         understand_iptw = `Please rate your understanding of the concept of  inverse probability of treatment weighting (IPTW) using propensity scores.`, 
         understand_mathcing = `Please rate your understanding of the concept of propensity score matching.`, 
         understand_regression = `Please rate your understanding of the concept of linear regression.`, 
         understand_estimands = `Please rate your understanding of the concept of causal estimands (SATT, SATE, CACE, etc).`, 
         understand_po = `Please rate your understanding of the concept of potential outcomes.`, 
         
  ) |>
  select(role, education, degree_in, hill, stats_classes, use_causal, use_stats, language_preference, 
         R_regression, R_documentation, 
         #estimands, 
         block, treatment, 
         ID, contains('understand'))

pre <- pre |> select(-14)


# drop duplicate pre-response
pre <- pre |>
  filter(row_number() != 12)


post <- post|>
  rename(
    ID = `Please enter your participant ID`, 
    satt_est = `Please enter your estimated average treatment effect for the treated (SATT)`, 
    satt_sd = `Please enter the standard deviation of the estimated SATT`, 
    satt_ci = `Please enter the 95% confidence interval for the estimated SATT`, 
    gsatt_est = `Please enter the point estimate of the SATT for \`mom_race\` at level "A".`, 
    gsatt_ci = `Please enter the 95% confidence interval of the estimated SATT for \`mom_race\` at level "A".`, 
    assumptions = `To interpret these results causally what assumptions would you have to make. Please summarize in one to two sentences at most.`, 
    language_use = `What language did you use in today in running the analysis as part of this study?`, 
    method = `What approach did you use for running the analysis as part of this study?`, 
    time = `How long did it take for you to run the data analysis needed to answer the causal question you were assigned with today?`, 
    proficient = `How proficient/familiar are you with the tool you used in today's analysis?`, 
    ease = `Please rate the ease of use of the tool you used in today's analysis.`, 
    confidence = `Please rate your confidence in the results of your analysis.`,
    help = `If were "stuck" at any point in your analysis, did the tool you were using have a user guide or documentation that was able to unblock you or did you need external assistance?`,
    recomend = `Would you recommend this tool to a fellow colleague/researcher for analysing their data to answer causal questions?`,
    useful_features = `What were the most useful features of the tool you used today?`
  ) |>
  dplyr::select(ID, satt_est, satt_ci, gsatt_est, gsatt_ci,
                assumptions, language_use, method, time, proficient, 
                ease, confidence, help, recomend, useful_features)


# join data 
dat <- post |>
  left_join(pre)

# true values
X <- aciccomp2016::input_2016
dgp <- aciccomp2016::dgp_2016(X, parameters = 49, random.seed = 49)
true_satt <- mean(dgp[['y.1']][dgp$z == 1] - dgp[['y.0']][dgp$z == 1])
true_gsatt <- mean(dgp[['y.1']][dgp$z == 1 & X$x_24 == 'A'] - dgp[['y.0']][dgp$z == 1 & X$x_24 == 'A'])



# crate cover variable

clean_ci <- function(x){
  x <- trimws(x)
  x <- gsub("\\(", "", x)
  x <- gsub("\\)", "", x)
  x <- gsub("\\[", "", x)
  x <- gsub("\\]", "", x)
  x <- gsub("lower-", "", x)
  x <- gsub("upper -", "", x)
  x <- gsub("-", ",", x)
  x <- gsub("to", ",", x)
  x <- gsub(";", ",", x)
  
  
  x
}


dat$satt_ci <- clean_ci(x = dat$satt_ci)
dat$gsatt_ci <- clean_ci(x = dat$gsatt_ci)


# clean up typo in case 8
dat$satt_ci[8] <- "4.41,4.99"
dat$gsatt_ci[8] <- "3.53,16.71"


# fix format in case 40
dat$satt_ci[40] <- "5.4311,5.4489"
dat$gsatt_ci[40] <- "0.4636,1.5612"


# remove all spaces
dat$satt_ci <- gsub(" ", "", dat$satt_ci)
dat$gsatt_ci <- gsub(" ", "", dat$gsatt_ci)

# seperate lic and uci into seperate variables
split_ci <- function(x){
  split <- stringr::str_split(x, ',')
  # this accounts for when respondent only entered a single number
  split[sapply(split, length) < 2] <-
    lapply(split[sapply(split, length) < 2],
           function(i) {
             c(i, i)
           })
  split
}

# warning is good here (tuns any null or "NA" into NA)
dat$satt_lci <- as.numeric(lapply(split_ci(dat$satt_ci),'[[', 1))
dat$satt_uci <- as.numeric(lapply(split_ci(dat$satt_ci),'[[', 2))
dat$gsatt_lci <- as.numeric(lapply(split_ci(dat$gsatt_ci),'[[', 1))
dat$gsatt_uci <- as.numeric(lapply(split_ci(dat$gsatt_ci),'[[', 2))

# create cover outcome variable (warnings are ok)
dat$satt_est <- as.numeric(dat$satt_est)
dat$gsatt_est <- as.numeric(dat$gsatt_est)

# clean time 
dat$time <- c(60, 10, 30, 15, 10, 55, 25, 35, 30, 90, 60, 5, 20, 10, 15, 26, 45, 5, 15, 10, 25, 30, 7, 25, 30, 60, 50, 50, 42, 50, 60, 60,40, 50, 15, 50,20, 10,20,50,50,30,15,60,5,20,40,60,25,30,20,20,65,10,25,60,15,25,60,10,15,35,30,40,20,15,50,30,35,20,5)

# create outcomes from responses
dat <- dat |>
  mutate(cover_satt = ifelse(satt_lci <= true_satt & true_satt <= satt_uci, 1, 0), 
         cover_gsatt = ifelse(gsatt_lci <= true_gsatt & true_gsatt <= gsatt_uci, 1, 0), 
         satt_ci_length = satt_uci - satt_lci, 
         gsatt_ci_length = gsatt_uci - gsatt_lci, 
         standard_satt_ci_length = ((satt_uci/sd(dgp$y)) - (satt_lci/sd(dgp$y))), 
         standard_gsatt_ci_length = ((gsatt_uci/sd(dgp$y)) - (gsatt_lci/sd(dgp$y))), 
         distance_satt = abs(satt_est - true_satt), 
         standard_distance_satt = abs(satt_est - true_satt)/sd(dgp$y),
         distance_gsatt = abs(gsatt_est - true_gsatt),
         standard_distance_gsatt = abs(gsatt_est - true_gsatt)/sd(dgp$y),
         recomend = ifelse(recomend == 'Yes', 1, 0), 
         answered_satt = ifelse(is.na(satt_est), 0, 1), 
         answered_gsatt = ifelse(is.na(gsatt_est), 0, 1)
  )

dat$satt_ci_length <- ifelse(round(dat$satt_ci_length, 3) == 0, NA, dat$satt_ci_length)
dat$gsatt_ci_length <- ifelse(round(dat$gsatt_ci_length, 3) == 0, NA, dat$gsatt_ci_length)

dat$cover_satt <- ifelse(is.na(dat$cover_satt), 0, dat$cover_satt)
dat$cover_gsatt <- ifelse(is.na(dat$cover_gsatt), 0, dat$cover_gsatt)

rm(pre, post)
rm(clean_ci, split_ci)
rm(root)
rm(X)
rm(dgp)

