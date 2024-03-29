## initialisation
lapply(c("dplyr", "psych","readr","broom", "Hmisc", "corrplot","qpcR","ggplot2", "ggfortify", "gridExtra")
       , require, character.only = TRUE)
setwd("C:/git/gsa") # Set WD

## Read in data
dat <- read_csv("data/cleaned/gameplay.csv")
  print(c('initial rows', nrow(dat), nrow(distinct(dat['user']))))

## Rename cols
dat <- dat %>% rename(datetime = time)

## Convert time to seconds
dat$t <- as.numeric(dat$datetime, units="secs")

## Compute variables

  # Summarise relevant variables
  summ <- dat %>%
    group_by(user) %>%
    summarise(
      n = n(), 
      t_min = min(t), 
      t_max = max(t), 
      t_diff = max(t)-min(t),
      scores_max = max(score),
      scores_min = min(score),
      scores_mean = mean(score),
      scores_median = median(score),
      scores_sd = sd(score),
      scores_diff = max(score)-min(score),
      condition = first(condition)
    )~
  
  # Compute new variables
  dat <- dat %>%
    left_join(summ, by = "user") %>%
    mutate(
      t_0_s = (t - t_min), # Normlise time (t_0)
      t_0_m = ((t - t_min) / 60) + 1,
      score_sd = (score - scores_mean) / scores_sd, # Relative learning score (score_sd)
      score_mdiff = score - scores_mean
    )
  
  #DIST- Time
  unique(dat$user)
  nrow(unique(dat %>% filter(condition=='iv') %>% dplyr::select(user)))
  nrow(unique(dat %>% filter(condition=='game') %>% dplyr::select(user)))
  
  ggplot( 
    dat %>% group_by(user, condition) %>% summarise(t_0_m = max(t_0_m))
  ) +
    geom_density( aes(x = t_0_m, y = ..density.., fill = condition, colour = condition), alpha=.6) +
    scale_fill_manual(values=c("#6cafb4","#d8d8d8")) +
    scale_colour_manual(values=c("#6cafb4","#d8d8d8")) +
    theme_bw() + xlab('Duration (min)') + ylab('Participants (proportion)') +
    xlim(0,60) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  
  summ %>% 
    group_by(condition) %>% 
    summarise(mean = mean(t_diff)/60)
  
  ggplot( dat ) +
    geom_density( aes(x = t_0_m, y = ..density.., fill = condition, colour = condition), alpha=.6) +
    scale_fill_manual(values=c("#6cafb4","#d8d8d8")) +
    scale_colour_manual(values=c("#6cafb4","#d8d8d8")) +
    theme_bw() + xlab('Duration (min)') + ylab('Participants (proportion)') +
    xlim(0,60) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  #DIST - Score
  ggplot( dat) +
    geom_density( aes(x = score, y = ..density.., fill = condition, colour = condition), alpha=.6) +
    scale_fill_manual(values=c("#6cafb4","#d8d8d8")) +
    scale_colour_manual(values=c("#6cafb4","#d8d8d8")) +
    theme_bw() + xlab('Duration (min)') + ylab('Participants (proportion)') +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  # Quick estimate time x score measure
  summary(
    lm(dat$t_0_s ~ dat$score)
  )
  t.test(
    (dat %>% filter(condition=='game', t_0_m < 5))$score,
    (dat %>% filter(condition=='game', t_0_m >= 20))$score
  )
  mean((dat %>% filter(condition=='game', t_0_m < 5))$score)
  mean((dat %>% filter(condition=='game', t_0_m >= 20))$score)
  sd((dat %>% filter(condition=='game', t_0_m < 5))$score)
  sd((dat %>% filter(condition=='game', t_0_m >= 20))$score)
  grouped_by_time <- dat %>% 
    filter(condition=='game') %>% 
    mutate(
      group = case_when(
        t_0_m<5 ~ 5, 
        t_0_m<10 ~ 10, 
        t_0_m<15 ~ 15, 
        t_0_m<20 ~ 20, 
        t_0_m<25 ~ 25, 
        t_0_m<30 ~ 30, 
        t_0_m<35 ~ 35, 
        t_0_m<40 ~ 40, 
        t_0_m<45 ~ 45, 
        t_0_m<50 ~ 50, 
        t_0_m<55 ~ 55, 
        t_0_m<60 ~ 50, 
        TRUE ~ 0
      )
    )
  learningOverTime <- grouped_by_time %>% 
    group_by(group) %>%
    filter(!is.na(score)) %>%
    summarise(
      mean = mean(score, na.rm=TRUE),
      sd = sd(score, na.rm = TRUE),
      n=n()
    )
  
  # Create interval summary variables
  
    # Round minutes (t_0_5m)
    dat$t_0_5m <- as.numeric( as.character( cut_interval(
      dat$t_0_m, 
      breaks = 55, 
      length = 5, 
      labels=seq(0, 270, by = 5)
    )))
    
    # Average scores within 1 (_1m) and 5 minutes (_5m)
    dat <- dat %>%
      group_by(user, t_0_m) %>%
      mutate(
        score_1m = mean(score, na.rm=TRUE),
        score_sd_1m = mean(score_sd, na.rm=TRUE),
        score_mdiff_1m = mean(score_mdiff, na.rm=TRUE) 
    )
    
    dat <- dat %>%
      group_by(user, t_0_5m) %>%
      mutate(
        score_5m = mean(score, na.rm=TRUE),
        score_sd_5m = mean(score_sd, na.rm=TRUE),
        score_mdiff_5m = mean(score_mdiff, na.rm=TRUE)
      )
    
    # Round scores to nearest 5%
      
      # Define function
      roundTo5 <- function(v, l){
        labs <- seq(
          floor(min(v, na.rm=TRUE)), 
          ceiling(max(v, na.rm=TRUE)), 
          by = l
        )
        nBreaks <- length(labs)
        return(
          as.numeric( as.character( cut_interval(
            v, 
            breaks = nBreaks, 
            length = l,
            labels = labs
          )))
        )
      }
      
      # Use function
      dat$score_5m_round <- roundTo5(dat$score_5m, 5)
      dat$score_sd_5m_round <- roundTo5(dat$score_sd_5m, 0.5)
      dat$score_mdiff_5m_round <- roundTo5(dat$score_mdiff_5m, 5)
    
## Exclusion criteria
  # Did not play the game
  dat <- filter(dat, condition=='game')
    print(c('filter by gamers', nrow(dat), nrow(distinct(dat['user']))))

  # No difference in time
  dat <- filter(dat, t_diff>0)
    print(c('filter by time diff', nrow(dat), nrow(distinct(dat['user']))))
    
  # Spent too long
  dat <- filter(dat, t_0_m<=50)
    print(c('filter by excessive time', nrow(dat), nrow(distinct(dat['user']))))
    
## Plots
    
    # Plot scores (move lower)
    ggplot( dat ) +
    geom_density( aes(x = score, y = ..density.., fill = user, colour = user), alpha=.6) +
      scale_fill_manual(values=c("#d8d8d8", "#6cafb4")) +
      scale_colour_manual(values=c("#d8d8d8", "#6cafb4")) +
      theme_bw() + xlab('Play experiences (n)') + ylab('Participants (proportion)') +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    dat$t_0_5m[dat$t_0_5m>=30] <- '30+'
    dat$`Duration of play (mins)` <- as.factor(dat$t_0_5m)
    
    ggplot( dat %>% filter(`Duration of play (mins)` %in% c('0','30+')) ) +
      scale_fill_manual(values=c("#C4DFAA", "#73A9AD")) +
      scale_colour_manual(values=c("#C4DFAA", "#73A9AD")) +
      geom_density( aes(x = score_5m_round, y = ..density.., fill=`Duration of play (mins)`, colour=`Duration of play (mins)`), alpha=.33) +
      theme_bw() + xlab('Score') + ylab('Solutions (proportion)') +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    # Players average scores
    ggplot( summ ) +
      geom_density(aes(x=scores_mean, y=..density.., fill='#73A9AD'), alpha=.6) +
      scale_fill_manual(values=c("#73A9AD")) +
      theme_bw() + xlab('Mean score') + ylab('Players (proportion)') +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    # Players intevention n
    summ$n[summ$n>100] <- 100
    ggplot( summ ) +
      geom_density(aes(x=n, y=..density.., fill='#73A9AD'), alpha=.6) +
      scale_fill_manual(values=c("#73A9AD")) +
      theme_bw() + xlab('Number of solutions suggested') + ylab('Players (proportion)') +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
## Results

  # Distributions (non-normal)
  hist(dat$t_0_m)
  hist(dat$score)
  
  # Ave scores
  
    # Plot
    ggplot( # scores
      filter(summ, (t_diff>0)&(t_diff<7500)), aes(x=t_diff, y=scores_mean)
    ) + geom_smooth(method=lm) + geom_point()
  
  # Raw rates
    # Regression
    LR_raw_r <- cor(dat$t_0_m, dat$score, method = ("spearman")) # r=.145
    LR_raw_p <- cor.test(dat$t_0_m, dat$score, method = ("spearman")) # p=6.122e-12
    LR_raw_r2 <- LR_raw_r ^ 2 # 2% variance explained
    LR_raw_coef <- lm(dat$score ~ dat$t_0_m) # Learning rate per minute = .9%
    summary(LR_raw_coef)
    # Plot
    results <- ggplot(
      dat, aes(x=t_0_5m, y=score_5m, group=t_0_5m)
    ) + geom_boxplot(notch=FALSE, outlier.shape=NA, colour='red') + geom_jitter(shape=1, size=0.5, position=position_jitter(1)) + xlab('Playtime (m)') + ylab('Score')
    results
    
  # Individual learning rate (sd)
    # Regression
    LR_sd_r <- cor(dat$t_0_m, dat$score_sd, method = ("pearson")) # r=.100
    LR_sd_p <- cor.test(dat$t_0_m, dat$score_sd, method = ("pearson")) # p=2.149e-06
    LR_sd_r2 <- LR_sd_r ^ 2 # 1.0% variance explained
    LR_sd_coef <- lm(dat$score_sd ~ dat$t_0_m) # Learning rate per minute = .014 SD
    
    # Plot
    
  # Individual learning rate (%)
    
    # Regression
    LR_pct_r <- cor(dat$t_0_m, dat$score_mdiff, method = ("spearman")) # r=0.102
    LR_pct_p <- cor.test(dat$t_0_m, dat$score_mdiff, method = ("spearman")) # p=1.202e-06
    LR_pct_r2 <- LR_pct_r ^ 2 # r2=.00105   1.05% variance explained
    LR_pct_coef <- lm(dat$score_mdiff ~ dat$t_0_m) # Learning rate per minute = 0.593%/m
    
    # Plot
    
      # Other, older plots
      a_sd <- 
        ggplot(
          dat, aes(x=t_0_m, y=score_sd)
        ) + geom_point() + geom_smooth(method=lm)
      
      a_raw <- ggplot( # scores
        dat, aes(x=t_0_m, y=score)
      ) + geom_smooth(method=lm) + geom_count() + scale_size_area(max_size = 2)
      
      a1 <- grid.arrange(
      
        ggplot(
          dat, aes(x=t_0_m, y=score_mdiff)
        ) + geom_point() + geom_smooth(method=lm),
        
        ggplot(
          dat, aes(x=t_0_m, y=score_mdiff)
        ) + geom_point(alpha=1/10,colour = "red", size = 1) + geom_smooth(method=lm),
        
        ggplot(
          dat, aes(x=t_0_m, y=score_mdiff) 
        ) + geom_smooth(method=lm)  + geom_count() + scale_size_area(max_size = 2),
       
       ggplot(
          dat, aes(x=t_0_m, y=score_mdiff_5m_round) 
        ) + geom_smooth(method=lm)  + geom_count() + scale_size_area(max_size = 2),
        
        ncol=2
        
      )
      
      # Final score over time plot
      ggplot(dat, aes(x=t_0_m, y=score_5m_round) ) + 
        geom_count(colour='#595959') + 
        geom_smooth(method=lm, colour = '#73A9AD')  + 
        scale_size_area(max_size = 2) +
        xlab('Duration of play (game)') +
        ylab('Score')+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      
      # Lines
      scoreOverTime <- dat %>%
        group_by(user, t_0_5m) %>%
        distinct(t_0_5m, .keep_all = TRUE) %>%
        group_by(t_0_5m) %>%
        summarise(
          score_mean = mean(score),
          score_n = n()
        )
      scoreOverTime$t_0_5m <- scoreOverTime$t_0_5m + 2.5
      
      b <- ggplot(
        scoreOverTime, aes(x=t_0_5m, y=score_mean)
      ) + geom_line(shape=18, size = scoreOverTime$score_n / 10)
      
      # Multiple lines
      c <- ggplot(
        summ_scorePer5m, aes(x=t_0_5m, y=score_5m, group=user)
      ) + geom_line(aes(color=user), alpha=1/2) + theme(legend.position="none")
      
    