## initialisation
library('readxl')
library('psych')
library('dplyr')
library('readr')
library('broom')
library('Hmisc')
library('tidyverse')
library('ggplot2')
library('sm')
setwd("C:/git/gsa") # Set WD

## Read in data
dat <- read_csv("data/cleaned/Q_scored.csv")
gameplay <- read_excel("data/cleaned/gameplay.xlsx")
gameplay <- gameplay %>% rename(
  dur = time,
)

## analysis

  # descriptives
    
    # n in each condition
    table(dat$cond)
    
    # all descriptives
    #descriptives <- describe( dat %>% select(-cond, -FreeText) ) %>% select(n, mean, sd, min, max)
    #descr_exp <- describe(dat %>% filter(cond == 'Exp')) %>% select(n, mean, sd, min, max)
    #descr_ctr <- describe(dat %>% filter(cond == 'Ctr')) %>% select(n, mean, sd, min, max)
    
    # correlate learning measures
    rcorr(dat$test_score_z, dat$test_ffe_total)
  
  #h1: Structure
  h1_srq <- t.test(dat$Direction ~ dat$cond, var.equal = TRUE)
  
  #h2: Motivation
  h2_srq <- t.test(dat$Motivation ~ dat$cond, var.equal = TRUE)
  h2_dur <- t.test(dat$cond_dur ~ dat$cond, var.equal = TRUE)
  h2_plex <- t.test(dat$PLEX_count ~ dat$cond, var.equal = TRUE)
  h2_plexs <- rbind(
    c('HEAD', broom::tidy( t.test(dat$Captivation ~ dat$cond, var.equal = TRUE) )[0,]),
    c('Capitavation', broom::tidy( t.test(dat$Captivation ~ dat$cond, var.equal = TRUE) )[1,]),
      c('Challenge', broom::tidy( t.test(dat$Challenge ~ dat$cond, var.equal = TRUE) )[1,]),
        c('Competition', broom::tidy( t.test(dat$Competition ~ dat$cond, var.equal = TRUE) )[1,]),
          c('Completion', broom::tidy( t.test(dat$Completion ~ dat$cond, var.equal = TRUE) )[1,]),
            c('Discovery', broom::tidy( t.test(dat$Discovery ~ dat$cond, var.equal = TRUE) )[1,]),
    c('Progression', broom::tidy( t.test(dat$Progression ~ dat$cond, var.equal = TRUE) )[1,]),
      c('Exploration', broom::tidy( t.test(dat$Exploration ~ dat$cond, var.equal = TRUE) )[1,]),
        c('Fantasy', broom::tidy( t.test(dat$Fantasy ~ dat$cond, var.equal = TRUE) )[1,]),
          c('Humor', broom::tidy( t.test(dat$Humor ~ dat$cond, var.equal = TRUE) )[1,]),
    c('Nurture', broom::tidy( t.test(dat$Nurture ~ dat$cond, var.equal = TRUE) )[1,]),
      c('Relaxation', broom::tidy( t.test(dat$Relaxation ~ dat$cond, var.equal = TRUE) )[1,]),
        c('Sensation', broom::tidy( t.test(dat$Sensation ~ dat$cond, var.equal = TRUE) )[1,])
  )
  
  # Plex summaries
  dat_plex <- dat %>% 
    group_by(cond) %>%
    summarise_at(vars(Captivation:Sensation), sum, na.rm=TRUE)
  dat_plex <- t(as.matrix(dat_plex[-1]))
  colnames(dat_plex) <- c('Ctr', 'Exp')
  
  
  t.test(dat$PLEX_count ~ dat$cond)
  
   
  ## TIME
  # Observed dur
  times_raw <- gameplay %>% 
    group_by(user) %>%
    summarise (
      cond = first(condition),
      n = n(),
      dur_0 = min(dur),
      dur_1 = max(dur),
      dur = difftime( max(dur), min(dur), units = 'mins')
    )
  times_raw$dur[times_raw$dur<1] <- NA
  times_raw$dur[times_raw$dur>60] <- 60
  times_raw$dur <- as.numeric(times_raw$dur)
  times_raw$cond[times_raw$cond == 'game'] <- 'Exp'
  times_raw$cond[times_raw$cond == 'iv'] <- 'Ctr'
  
  dat_adj <- dat %>% filter(cond_dur > 1)
  t.test(dat_adj$cond_dur  ~ dat_adj$cond)
  
  # Combine with predicted dur
  times_combined <- rbind(
    dat %>% select(cond, cond_dur) %>% rename(Condition=cond), 
    times_raw %>% select(cond, dur) %>% rename(cond_dur=dur, Condition=cond)
  )
  times_combined$Condition[times_combined$Condition == 'Exp'] <- 'Game'
  times_combined$Condition[times_combined$Condition == 'Ctr'] <- 'Control'
  
  ggplot( times_combined ) +
    geom_density( aes(x = cond_dur, y = ..density.., fill = Condition, colour = Condition), alpha=.6) +
    scale_fill_manual(values=c("#d8d8d8", "#6cafb4")) +
    scale_colour_manual(values=c("#d8d8d8", "#6cafb4")) +
    theme_bw() + xlab('Duration (minutes)') + ylab('Participants (proportion)') +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  # Plot play experiences
  dat_for_plot <- dat %>% rename(Condition=cond)
  dat_for_plot$Condition[dat_for_plot$Condition == 'Exp'] <- 'Game'
  dat_for_plot$Condition[dat_for_plot$Condition == 'Ctr'] <- 'Control'
  
  ggplot( dat_for_plot ) +
    geom_density( aes(x = PLEX_count, y = ..density.., fill = Condition, colour = Condition), alpha=.6) +
    scale_fill_manual(values=c("#d8d8d8", "#6cafb4")) +
    scale_colour_manual(values=c("#d8d8d8", "#6cafb4")) +
    theme_bw() + xlab('Play experiences (n)') + ylab('Participants (proportion)') +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ## PLEX x Time
  lm(dat$cond_dur ~ dat$PLEX_count)
  
  #h3: Learning
  h3_mcq <- t.test(dat$test_score_z ~ dat$cond, var.equal = TRUE)
  h3_ffe <- t.test(dat$test_ffe_total ~ dat$cond, var.equal = TRUE)
  h3_dur <- t.test(dat$test_dur  ~ dat$cond, var.equal = TRUE)
  
  dat_game <- dat %>% filter(cond == 'Exp')
  durXplex <- lm(data=dat, cond_dur ~ PLEX_count)
  summary(durXplex)
  table()
  plot(dat$cond_dur ~ dat$PLEX_count)

  
# Save
  write_csv(as.data.frame(dat_plex), "./outputs/plex_summ.csv")
  write_csv(dat %>% select(cond, cond_dur, PLEX_count), "./outputs/plex_byMin.csv")
  
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
