## INIT
library('readxl')
library('readr')
library('dplyr')
library('ggplot2')
library('psych')
library('tidyverse')
library('lme4')
library('ordinal')

setwd("C:/git/gsa") # Set WD

## DATA

# Load in data
dat <- read_csv("data/cleaned/Q_scored.csv")%>% 
  mutate(cond_bin = ifelse(cond=='Ctr', 1, 2)) %>%
  mutate_at(vars(Captivation:Sensation), ~.x+1 )
Q <- read_csv("data/cleaned/Q_excludedPpts.csv") %>% 
  mutate( 
    cond_bin = ifelse(cond=='Ctr', 1, 2),
    test_score_inv = 23-test_score
  )
gameplay <- read_excel("data/cleaned/gameplay.xlsx")
gameplay <- gameplay %>% 
  rename(timestamp = time) %>% 
  group_by(user) %>%
  mutate(
    n = row_number(),
    t_min = min(timestamp),
    t_diff = difftime(max(timestamp), min(timestamp), units = 'mins')
  ) %>% 
  ungroup() %>%
  mutate(
    t = difftime(timestamp, t_min, units = 'mins')
  ) %>%
  filter(
    t_diff<80,
    t_diff>0,
    condition=='game'
  )

## DESCRIPTIVES
# Summaries
sumry <- function(x){return( 
  cbind(
    x %>% ungroup %>% summarise_if(is.numeric, list(mean=mean,sd=sd,min=min,max=max, var=var) ),
    x %>% summarise_if(is.numeric, list(g_mean=mean,g_sd=sd,g_min=min,g_max=max, g_var=var) )
  )
)}
descriptives <- cbind( 
  duration = dat %>% group_by(cond) %>% dplyr::select(cond_dur) %>% sumry,
  plex =  dat %>% group_by(cond) %>% dplyr::select(PLEX_count) %>% sumry,
  usability =  Q %>% group_by(cond) %>% dplyr::select(Usability) %>% sumry,
  mcq = Q %>% group_by(cond) %>% dplyr::select(test_score) %>% sumry,
  score =  gameplay %>% group_by(condition) %>% dplyr::select(score) %>% sumry
)
scores<-gameplay %>% 
  filter(condition=='game') %>%
  group_by(objective) %>%
  summarise( n = n(), mean = mean(score), min = min(score), max = max(score), sd=sd(score) )

# Distributions
ggplot(dat, aes(x=cond_dur, fill=cond)) + 
  geom_density( alpha=.5 ) +
  theme_bw() + xlab('Duration (min)') + ylab('Participants') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggplot(dat, aes(x=PLEX_count, fill=cond)) + 
  geom_density( alpha=.5 ) +
  theme_bw() + xlab('Playfulness (1-12)') + ylab('Participants') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggplot(Q, aes(x=Usability, fill=cond)) + 
  geom_density( alpha=.5 ) +
  theme_bw() + xlab('Usability (%)') + ylab('Participants') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggplot(Q, aes(x=test_score, fill=cond)) + 
  geom_density( alpha=.5 ) +
  theme_bw() + xlab('MCQ test score (0-22)') + ylab('Participants') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggplot(gameplay, aes(x=score)) + 
  geom_density( alpha=.5 ) +
  theme_bw() + xlab('In-game score (0-100%)') + ylab('Participants') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Distribution tests
distributionParamaters <- cbind(
  rbind(
    bartlett.test(cond_dur ~ cond, data=dat),
    bartlett.test(PLEX_count ~ cond, data=dat),
    bartlett.test(Usability ~ cond, data=Q),
    bartlett.test(test_score ~ cond, data=Q),
    NA
  ),
  rbind(
    shapiro.test( (dat %>% filter(cond=='Ctr') )$cond_dur ),
    shapiro.test( (dat %>% filter(cond=='Ctr') )$PLEX_count ),
    shapiro.test( (Q %>% filter(cond=='Ctr') )$Usability ),
    shapiro.test( (Q %>% filter(cond=='Ctr') )$test_score ),
    shapiro.test( (gameplay )$score )
  ),
  rbind(
    shapiro.test( (dat %>% filter(cond=='Exp') )$cond_dur ),
    shapiro.test( (dat %>% filter(cond=='Exp') )$PLEX_count ),
    shapiro.test( (Q %>% filter(cond=='Exp') )$Usability ),
    shapiro.test( (Q %>% filter(cond=='Exp') )$test_score ),
    shapiro.test( (gameplay )$score )
  )
)

# Save
write.csv(descriptives, 'outputs/descriptives.csv')
write.csv(distributionParamaters, 'outputs/distributionParamaters.csv')

## ANALYSIS

#LM assumptions
# 1- linearity 
# 2- homoscedascity (variance is constant across values)
# 3- no autocorrelation
# 4- multivarate normality (normally distributed residuals)
# 5- no multicolinearaity (variables not highly correlated with eachother)
#GLM assumptions
# 1- indepdendence
# 2- distribution defined by family of glm
# 3- linear relationship after transformation by link function
  #Gamma - gamma dsitribution + no 0s
  #Ordinal logistic regression - proportional odds (explanatory variables have the same effect on the odds regardless of the threshold)

#Plots: 
# Residuals X Fitted Values = plot(XX, 1) 
  #Residual x fitted value graph shows:
  # 1- Random variance around the 0 line indicates no hetereroscedascity (& supports a linear relationship)
  # 2- A mean line around 0 indicates the variances of the error terms are equal
  # 3- An absence of extreme values indicates there are no outliers
# Q-Q = plot(XX, 2)
  #Q-Q graph shows:
  # Values fitting the line closely indicate error terms are normally distributed 

#Tests:
#Equal variances: bartlett.test(PLEX_count ~ cond, data=dat) 
#Normality: shapiro.test(dat$PLEX_count)

# Define functions to summarise results
summarise_wilcox <- function(res){return(broom::tidy(res)[,1:2])}
summarise_glm <- function(res){return(broom::tidy(res)[2,])}
summarise_logOrdReg <- function(res){
  coefs <- coef(summary(res))
  t <- cbind(
    coefs,
    p=pnorm(abs(coefs[, "t value"]), lower.tail = FALSE) * 2
  )
  colnames(t)=c("estimate", "std.error", "statistic", "p.value")
  t <- rownames_to_column(as.data.frame(t), var = "term")
  return(t[1,])
}
summarise_lmer <- function(res){
  coefs <- coef(summary(res))
  t <- cbind(
    coefs,
    p=pnorm(abs(coefs[, "t value"]), lower.tail = FALSE) * 2
  )
  colnames(t)=c("estimate", "std.error", "statistic", "p.value")
  t <- rownames_to_column(as.data.frame(t), var = "term")
  return(t[2,])
}

#Hypothesis tests
motivation_duration <- glm(cond_dur ~ cond_bin, family=Gamma, data=dat) # diagnostic plots   plot(dur, 1)   plot(dur, 2)
motivation_playfulness <- wilcox.test( PLEX_count ~ cond_bin, data=dat)
learning <- wilcox.test(formula=test_score~cond_bin, data=Q) 

#Other analyses
sensitivity_usability <- wilcox.test(formula=Usability~cond_bin, data=Q) 
research_descriptivesOfNs <- gameplay %>% summarise(total=n(),mean=mean(n, na.rm=TRUE), sd=sd(n, na.rm=TRUE), min=min(n, na.rm=TRUE), max=max(n, na.rm=TRUE))
research_descriptivesOfScores <- gameplay %>% summarise(mean=mean(score, na.rm=TRUE), se=sd(score, na.rm=TRUE)/sqrt(n()), min=min(score, na.rm=TRUE), max=max(score, na.rm=TRUE))
research_descriptivesByGoal <- gameplay %>% group_by(objective) %>% summarise(mean=mean(score, na.rm=TRUE), se=sd(score, na.rm=TRUE)/sqrt(n()), min=min(score, na.rm=TRUE), max=max(score, na.rm=TRUE))
research_typesOfSolutions <- NA

ggplot(gameplay, aes(x=t,y=score, size=n) ) +
  geom_point() +
  geom_abline( slope=  ) + 
  lims(x = c(0,25), y = c(0,100)) +
  theme_bw() + xlab('Time in-game (minute)') + ylab('Score (%)') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  # Scores over gameplay
    
    # Get scores over time / trials
    scoresOverTime<-gameplay %>% 
      filter(condition=='game') %>%
      group_by(user, t) %>%
      summarise( 
        score =mean(score),
        n_solutions = n()
      ) %>%
      group_by(t) %>%
      summarise(
        n_solutions = sum( n_solutions  ),
        n_users = n(), 
        mean = mean(score), min = min(score), max = max(score), sd=sd(score), 
        conf_hi=mean(score) + qt(0.95,df=n()-1)*sd(score)/sqrt(n()),
        conf_lo=mean(score) - qt(0.95,df=n()-1)*sd(score)/sqrt(n()) 
      ) %>%
      filter(n_users>10)
    scoresOverTrials<-gameplay %>% 
      filter(condition=='game') %>%
      group_by(user, n) %>%
      summarise( 
        score =mean(score),
        n_solutions = n()
      ) %>%
      group_by(n) %>%
      summarise(
        n_solutions = sum( n_solutions  ),
        n_users = n(), 
        mean = mean(score), min = min(score), max = max(score), sd=sd(score), 
        conf_hi=mean(score) + qt(0.95,df=n()-1)*sd(score)/sqrt(n()),
        conf_lo=mean(score) - qt(0.95,df=n()-1)*sd(score)/sqrt(n()) 
      ) %>%
      filter(n_users>10)
    
    # Describe distributions
    hist( scoresOverTime$mean )
    shapiro.test(scoresOverTime$mean)
    hist( scoresOverTrials$mean )
    shapiro.test(scoresOverTrials$mean)
    
    # Model 
    research_scores_byTime <- lm(mean~t, data=scoresOverTime)
    research_scores_byTrial <- lm(mean~n, data=scoresOverTrials)
    
    # Main plots
    ggplot(scoresOverTime, aes(x=t,y=mean, size=n_users) ) +
      geom_point() +
      geom_smooth(method = "glm", se = TRUE) +
      lims(x = c(0,25), y = c(0,100)) +
      theme_bw() + xlab('Time in-game (minute)') + ylab('Score (%)') +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    ggplot(scoresOverTrials, aes(x=n,y=mean, size=n_users) ) +
      geom_point() +
      geom_smooth(method = "glm", se = TRUE) +
      ylim( c(0, 100) ) +
      theme_bw() + xlab('In-game trial number') + ylab('Score (%)') +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

    # Diagnostic plots
    plot( research_scores_byTime, 1 )
    plot( research_scores_byTime, 2 )
    plot( research_scores_byTrial, 1 )
    plot( research_scores_byTrial, 2 )
    
  #Mixed ordinal logistic regression
  gameplay_quant <- gameplay %>%
    mutate(
      score = case_when(
        score == 0 ~ 0,
        score < 100 ~ 1,
        score == 100 ~ 2
      )
    )

  research_score_overTime <- clmm( 
    as.factor(score) ~ t + (1 | user), 
    data=gameplay_quant
  )
  research_score_overTrial <- clmm( 
    as.factor(score) ~ n + (1 | user), 
    data=gameplay_quant
  )
  
  
  table(gameplay$score, gameplay$t)
  table(gameplay$score, gameplay$n)
  
  summary(research_score_overTime)
  summary(research_score_overTrial)
  
  exp( coef(research_score_overTime) )
  
  coef(research_score_overTime)
  
  plot(research_score_overTime, type=c("p","smooth"), col.line=2, col=1)
  lattice::qqmath(research_score_overTime, col=1, col.line=2)
  
  summary(research_score_overTime)
  
  gameplay_quant_forPlot <- gameplay_quant %>%
    group_by(t) %>%
    summarise( pct = sum(score)/n()*100, num=n() ) %>%
    filter(num>10)
  ggplot(gameplay_quant_forPlot, aes(x=t,y=pct, size=num) ) +
    geom_point() +
    ylim( c(0, 100) ) +
    theme_bw() + xlab('Time in-game (minute)') + ylab('Valid solutions (%)') +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  
# VIEW RESULTS
results <- rbind(
  summarise_wilcox( motivation_playfulness ),
  summarise_wilcox( learning ),
  summarise_wilcox( sensitivity_usability )
)
results2 <- summarise_glm( motivation_duration )

results$term <- c("dur",  "learn",  "usability")