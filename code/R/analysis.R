## initialisation
library('psych')
library('dplyr')
library('readr')
library('broom')
setwd("C:/py/gsa") # Set WD

## Read in data
dat <- read_csv("data/cleaned/Q_scored.csv")

## analysis

  # descriptives
    
    # n in each condition
    table(dat$cond)
    
    # all descriptives
    descriptives <- describe( dat %>% select(-cond, -FreeText) ) %>% select(n, mean, sd, min, max)
    descr_exp <- describe(dat %>% filter(cond == 'Exp')) %>% select(n, mean, sd, min, max)
    descr_ctr <- describe(dat %>% filter(cond == 'Ctr')) %>% select(n, mean, sd, min, max)
  
  #h1: Structure learners
  h1_srq <- t.test(dat$Direction ~ dat$cond)
  
  #h2: Motivate learners
  h2_srq <- t.test(dat$Motivation ~ dat$cond)
  h2_dur <- t.test(dat$cond_dur ~ dat$cond)
  h2_plex <- t.test(dat$PLEX_count ~ dat$cond)
  h2_plexs <- rbind(
    broom::tidy( t.test(dat$Captivation ~ dat$cond) )[0,],
    broom::tidy( t.test(dat$Captivation ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Challenge ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Competition ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Completion ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Discovery ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Progression ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Exploration ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Fantasy ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Humor ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Nurture ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Relaxation ~ dat$cond) )[1,],
    broom::tidy( t.test(dat$Sensation ~ dat$cond) )[1,]
  )
  
  
  #h3: Better teach learners
  h3_mcq <- t.test(dat$test_score_z ~ dat$cond)
  h3_ffe <- t.test(dat$test_ffe_total ~ dat$cond)
  h3_dur <- t.test(dat$test_dur  ~ dat$cond)
  
# Save
  write_csv(dat, "./outputs/data.csv")
  write_csv(descriptives, "./outputs/descriptives_overall.csv")
  write_csv(descr_exp, "./outputs/descriptives_expmtl.csv")
  write_csv(descr_ctr, "./outputs/descriptives_control.csv")
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
  write_csv(h2_plexs,"./outputs/plexTTests.csv")
