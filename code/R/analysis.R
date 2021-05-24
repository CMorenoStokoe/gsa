## initialisation
library('dplyr')
library('readr')
library('psych')
library('broom')
setwd("C:/py/gsa") # Set WD

## Read in data
dat <- read_csv("data/cleaned/Q_scored.csv")

## analysis

  # descriptives
    
    # totals
    descriptives <- describe( dat %>% select(-cond, -FreeText) ) %>% select(n, mean, sd, min, max)
    
    descr_exp <- describe(dat %>% filter(cond == 'Exp')) %>% select(n, mean, sd, min, max)
    descr_ctr <- describe(dat %>% filter(cond == 'Ctr')) %>% select(n, mean, sd, min, max)
    
    par(mfrow=c(1,3))
    hist(dat$Motivation)
    hist(dat$Direction)
    hist(dat$PLEX_count)
  
  # usability
    #usability <- table(dat$Cond, dat$Usability)
    # data not extracted
  
  #h1: Structure learners
  h1_srq <- t.test(dat$Direction ~ dat$cond)
  
  #h2: Motivate learners
  h2_srq <- t.test(dat$Motivation ~ dat$cond)
  h2_dur <- t.test(dat$cond_dur ~ dat$cond)
  
  #h3: Better teach learners
  h3_mcq <- t.test(dat$test_score_adjusted ~ dat$cond)
  h3_ffe <- t.test(dat$test_ffe_total ~ dat$cond)
  h3_dur <- t.test(dat$test_dur  ~ dat$cond)
  
# Save
  write_csv(testLoadings, "./outputs/loadings.csv")
  write_csv(dat, "./outputs/table.csv")
  write_csv(
    rbind(
      broom::tidy(h1_srq)[0,],
      broom::tidy(h1_srq)[1,],
      broom::tidy(h2_srq)[1,],
      broom::tidy(h2_dur)[1,],
      broom::tidy(h3_mcq)[1,],
      broom::tidy(h3_ffe)[1,],
      broom::tidy(h3_dur)[1,]
    ),
    "./outputs/hypothesisTests.csv"
  )
