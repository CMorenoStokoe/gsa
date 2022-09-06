## Initialisation
library('readxl')
library('psych')
library('dplyr')
library('readr')
library('broom')
library('Hmisc')
library('tidyverse')
library('ggplot2')
library('sm')
library('leaps')
setwd("C:/git/gsa") # Set WD

## Read in data
dat <- read_csv("data/cleaned/Q_scored.csv")
gameplay <- read_excel("data/cleaned/gameplay.xlsx")
gameplay <- gameplay %>% rename(
  dur = time
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
  
  h2_plexs_time <- as.data.frame(rbind(
    c('Capitavation', broom::tidy( t.test(dat$cond_dur ~ dat$Captivation , var.equal = TRUE) )[1,]),
    c('Challenge', broom::tidy( t.test(dat$cond_dur ~ dat$Challenge , var.equal = TRUE) )[1,]),
    c('Competition', broom::tidy( t.test(dat$cond_dur ~ dat$Competition , var.equal = TRUE) )[1,]),
    c('Completion', broom::tidy( t.test(dat$cond_dur ~ dat$Completion , var.equal = TRUE) )[1,]),
    c('Discovery', broom::tidy( t.test(dat$cond_dur ~ dat$Discovery , var.equal = TRUE) )[1,]),
    c('Progression', broom::tidy( t.test(dat$cond_dur ~ dat$Progression , var.equal = TRUE) )[1,]),
    c('Exploration', broom::tidy( t.test(dat$cond_dur ~ dat$Exploration , var.equal = TRUE) )[1,]),
    c('Fantasy', broom::tidy( t.test(dat$cond_dur ~ dat$Fantasy , var.equal = TRUE) )[1,]),
    c('Humor', broom::tidy( t.test(dat$cond_dur ~ dat$Humor , var.equal = TRUE) )[1,]),
    c('Nurture', broom::tidy( t.test(dat$cond_dur ~ dat$Nurture , var.equal = TRUE) )[1,]),
    c('Relaxation', broom::tidy( t.test(dat$cond_dur ~ dat$Relaxation , var.equal = TRUE) )[1,]),
    c('Sensation', broom::tidy( t.test(dat$cond_dur ~ dat$Sensation , var.equal = TRUE) )[1,])
  ))
  write_csv(data.frame(
    plex = h2_plexs_time$V1,
    game = h2_plexs_time$estimate2,
    ctr = h2_plexs_time$estimate1,
    t = h2_plexs_time$statistic,
    pval = h2_plexs_time$p.value,
    df = 173
  ), "./outputs/plexs_time.csv")
  
  dat_game <- dat %>% filter(cond=='Exp')
  h2_plexs_time_inGame <- as.data.frame(rbind(
    c('Capitavation', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Captivation , var.equal = TRUE) )[1,]),
    c('Challenge', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Challenge , var.equal = TRUE) )[1,]),
    c('Competition', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Competition , var.equal = TRUE) )[1,]),
    c('Completion', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Completion , var.equal = TRUE) )[1,]),
    c('Discovery', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Discovery , var.equal = TRUE) )[1,]),
    c('Progression', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Progression , var.equal = TRUE) )[1,]),
    c('Exploration', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Exploration , var.equal = TRUE) )[1,]),
    c('Fantasy', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Fantasy , var.equal = TRUE) )[1,]),
    c('Humor', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Humor , var.equal = TRUE) )[1,]),
    c('Nurture', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Nurture , var.equal = TRUE) )[1,]),
    c('Relaxation', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Relaxation , var.equal = TRUE) )[1,]),
    c('Sensation', broom::tidy( t.test(dat_game$cond_dur ~ dat_game$Sensation , var.equal = TRUE) )[1,])
  ))
  
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
  
  #Desciprtives
  grouped_by_time <- dat %>% 
    mutate(
      group = case_when(
        PLEX_count<3 ~ 3, 
        PLEX_count<6 ~ 6, 
        PLEX_count<9 ~ 9, 
        PLEX_count<12 ~ 12, 
        PLEX_count<15 ~ 15, 
        PLEX_count<18 ~ 18, 
        TRUE ~ 0
      )
    )
  plexOverTime <- grouped_by_time %>% 
    group_by(group) %>%
    filter(!is.na(cond_dur)) %>%
    summarise(
      mean = mean(cond_dur, na.rm=TRUE),
      sd = sd(cond_dur, na.rm = TRUE),
      n=n()
    )
  
  #Model
  fit <- summary(
    lm(dat$cond_dur ~ dat$PLEX_count)
  )
  
  loadings <- dat %>% 
    select(Captivation:Sensation) %>%
    summarise_all( 
      list(
        r2 = function(x){return( lm(x ~ dat$cond_dur)$coefficients[1] )},
        p = function(x){return( summary(lm(x ~ dat$cond_dur)$ )}
      )
      
    )
  loadings <- as.data.frame( t(loadings) )
  colnames(loadings) <- c('R2')
  loadings$plex <- rownames(loadings)
  loadings <- loadings %>% arrange(desc(R2)) %>%
    mutate(plex=factor(plex, levels=plex))
  
    # Scree
    ggplot(loadings, aes(x=plex, y=R2, group=1))+
      geom_point(size=4)+
      geom_line()+
      labs(title="Scree plot: PCA on scaled data")
    
    # Top hits are: Exploration, discovery, progression, fantasy, captivation, sensation
    models <- summary( # All
      lm(
        data = dat %>% select(cond_dur, Captivation:Sensation), 
         cond_dur ~ .
    ))
    summary( # Top experiences only
      lm(
        data = dat %>% select(cond_dur, Competition, Challenge, Completion, Progression), 
        cond_dur ~ .
      ))
    summary( # All sig experiences 
      lm(
        data = dat %>% select(cond_dur, Competition, Challenge, Completion, Progression, Fantasy, Sensation, Relaxation), 
        cond_dur ~ .
      ))
    summary( # Just the best
      lm(
        data = dat %>% select(cond_dur, Competition, Challenge, Completion), 
        cond_dur ~ .
      ))
    
    # t-test for individual PLEX categories
    
    summary( 
      aov(data=dat,
        cond_dur ~ Captivation + Challenge +
          Competition + Completion + Discovery + Progression +
          Exploration + Fantasy + Humor + Nurture + Relaxation + Sensation 
    ))
    
    
    a<-rbind(
      cbind(
      'Captivation',
      dat %>% filter(Captivation==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Captivation==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Challenge',
      dat %>% filter(Challenge==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Challenge==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
      dat %>% filter(Completion==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Completion==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Competition',
      dat %>% filter(Competition==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Competition==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Discovery',
      dat %>% filter(Discovery==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Discovery==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Progression',
      dat %>% filter(Progression==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Progression==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Fantasy',
      dat %>% filter(Fantasy==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Fantasy==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Humor',
      dat %>% filter(Humor==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Humor==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Nurture',
      dat %>% filter(Nurture==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Nurture==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Relaxation',
      dat %>% filter(Relaxation==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Relaxation==1) %>% summarise(mean= mean(cond_dur))),
      cbind(
        'Sensation',
      dat %>% filter(Sensation==0) %>% summarise(mean= mean(cond_dur)),
      dat %>% filter(Sensation==1) %>% summarise(mean= mean(cond_dur)))
    )
    
    # t-tests
    lapply(
      dat %>% select(Captivation:Sensation),
      function(x){
        t<- t.test(dat$cond_dur, x)
        return(cbind(
          t$estimate[1],
          t$estimate[2],
          t$statistic, 
          t$p.value
        ))
      }
    )
    
    
    lapply(
      dat %>% select(Captivation:Sensation),
      function(x){
        t<- t.test(dat$cond_dur, x)
        return(table(
          dat$cond_dur, 
          x
        ))
      }
    )
    table(dat$cond_dur, dat$Captivation)
      
    ggplot(models$coefficients, aes(x=plex, y=R2, group=1))+
      geom_point(size=4)+
      geom_line()+
      labs(title="Scree plot: PCA on scaled data")
    
  
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
