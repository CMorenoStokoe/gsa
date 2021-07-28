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
    
    # correlate learning measures
    rcorr(dat$test_score_z, dat$test_ffe_total)
  
  #h1: Structure
  h1_srq <- t.test(dat$Direction ~ dat$cond, var.equal = TRUE)
  
  #h2: Motivation
  h2_srq <- t.test(dat$Motivation ~ dat$cond, var.equal = TRUE)
  h2_dur <- t.test(dat$cond_dur ~ dat$cond, var.equal = TRUE)
  h2_plex <- t.test(dat$PLEX_count ~ dat$cond, var.equal = TRUE)
  h2_plexs <- rbind(
    broom::tidy( t.test(dat$Captivation ~ dat$cond, var.equal = TRUE) )[0,],
    broom::tidy( t.test(dat$Captivation ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Challenge ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Competition ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Completion ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Discovery ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Progression ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Exploration ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Fantasy ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Humor ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Nurture ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Relaxation ~ dat$cond, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Sensation ~ dat$cond, var.equal = TRUE) )[1,]
  )
  
  
  #h3: Learning
  h3_mcq <- t.test(dat$test_score_z ~ dat$cond, var.equal = TRUE)
  h3_ffe <- t.test(dat$test_ffe_total ~ dat$cond, var.equal = TRUE)
  h3_dur <- t.test(dat$test_dur  ~ dat$cond, var.equal = TRUE)
  
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
