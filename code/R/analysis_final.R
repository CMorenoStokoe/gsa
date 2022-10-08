## INIT
library('readxl')
library('readr')
library('dplyr')
library('lmtest')
library('sandwich')
library('MASS')
library('ggplot2')
library('psych')
library('tidyverse')

setwd("C:/git/gsa") # Set WD

## DATA
# Loading
dat <- read_csv("data/cleaned/Q_scored.csv")
dat <- dat %>% 
  mutate(
    cond_binary = ifelse(cond =='Ctr', 0, 1)
  )
Q <- read_csv("data/cleaned/Q_excludedPpts.csv")
Q <- Q %>% mutate(
    cond_binary = ifelse(cond =='Ctr', 0, 1)
  )
gameplay <- read_excel("data/cleaned/gameplay.xlsx")
gameplay <- gameplay %>% 
  rename(timestamp = time) %>% 
  group_by(user) %>%
  mutate(
    t_max = max(timestamp),
    t_min = min(timestamp),
    t_diff = difftime(max(timestamp), min(timestamp), units = 'mins')
  ) %>%
  ungroup() %>%
  mutate(
    t = difftime(timestamp, t_min, units = 'mins'),
    tPct = as.numeric(difftime(timestamp, t_min, units = 'mins'))/as.numeric(difftime(t_max, t_min, units = 'mins')),
    condition_binary = ifelse(condition=='iv', 0, 1)
  ) %>%
  filter(
    t_diff<80,
    t_diff>0
  )
allInterventions <- as.data.frame(read_excel(
  "outputs/allPossibleInterventions_calculated.xlsx", 
  sheet = "allInterventionPools"
))
allInterventions <- allInterventions %>% filter(Group != 'Best solutions')
allInterventions$Group <- as.factor( allInterventions$Group )
levels(allInterventions$Group) <- c(1, 0)
randomGuessing <-  allInterventions %>% filter(Group==0)  # bootstrap random guesses to match sample size for player guesses
randomGuessing <- randomGuessing[rep(seq_len(nrow(randomGuessing)), each = 18), ]
allInterventions <- rbind(allInterventions, randomGuessing)

# Create player groups
createGroups <- function(){
  playerMeans <- gameplay %>% 
    filter(condition=='game') %>%
    group_by(user) %>% 
    summarise(
      score = mean(score, na.rm=TRUE),
      group = 1
    )  
  meanNInterventions <- gameplay %>% 
    filter(condition=='game') %>% 
    group_by(user) %>% summarise(n = n()) %>% 
    summarise(
      min = min(n),
      max = max(n),
      mean = mean(n),
      sd = sd(n)
    )
  randomScore <- function(){
    n_sample <- seq(meanNInterventions$min, meanNInterventions$max, by=1)
    n_weights <- dnorm(
      seq(meanNInterventions$min, meanNInterventions$max, by=1), 
      mean=meanNInterventions$mean, 
      sd=meanNInterventions$sd
    )
    n <- sample(n_sample, size=1, prob=n_weights)
    score_sample <- ( allInterventions %>% filter( Group==0 ) )$Score
    return(
      sample(score_sample, size=n, replace=TRUE)
    )
  }
  randomMeans <- data.frame(user=character(), score=numeric(), group=numeric() )
  for(i in 1:nrow( playerMeans )){
    randomScores <- randomScore()
    randomMeans <- rbind(
      randomMeans,
      data.frame(
        score = randomScores,
        user = paste('random-', i, sep=''),
        group = 0
      )
    )
  }
  return( rbind(playerMeans, randomMeans) )
}
#groupsForComparingScores <- createGroups() %>% filter(score > 0)

## DESCRIPTIVES
descriptives <- cbind( 
  dat %>%
      group_by(cond) %>%
      dplyr::select(cond_dur, PLEX_count) %>%
      summarise_all( list(mean=mean,sd=sd,min=min,max=max) ),
  Q %>%
    group_by(cond) %>%
    dplyr::select(test_score, Usability) %>%
    summarise_all( list(mean=mean,sd=sd,min=min,max=max) ),
  gameplay %>%
    group_by(user, condition) %>%
    summarise(
      t_diff=first(t_diff),
      score=mean(score),
      n=n()
    ) %>%
    group_by(condition) %>%
    dplyr::select(t_diff, score, n) %>%
    summarise_all( list(mean=mean,sd=sd,min=min,max=max) ),
  groupsForComparingScores %>%
    group_by(group) %>%
    dplyr::select(score) %>%
    summarise( n=n(), mean=mean(score),sd=sd(score),min=min(score),max=max(score) ),
  gameplay %>%
    filter(condition=='game') %>%
    group_by( t ) %>% 
    summarise( n=n(), mean=mean(score),sd=sd(score),min=min(score),max=max(score) )
)

## ANALYSIS

#MOTIVATION - plot(duration_q, 1) ; plot(duration_q, 2)
duration_q <- glm(cond_dur ~ cond, family=Gamma, data=dat)
duration_sw <- glm(as.numeric(t_diff) ~ condition, family=Gamma, data=gameplay)
#PLEX - plot(plex, 1) ; plot(plex, 2)
#More playful experiences in game
plex <- glm(formula=cond_binary~PLEX_count, family=poisson, data=dat) # not normally distributed but equal variances: bartlett.test(PLEX_count ~ cond, data=dat) && shapiro.test(dat$PLEX_count)
#PLEX x Duration
#These were linked to users spending longer with software
plexXDuration <- glm(
    formula = cond_dur ~ Captivation + Challenge + Competition + Completion + Discovery + Progression + Exploration + Fantasy + Humor + Nurture + Relaxation + Sensation,
    data = dat, family = Gamma)
#And one (completion) was a significant moderator where players experiencing this played for longer 
# plot(plexXDuration_mod, 1) ; plot(plexXDuration_mod, 2)
plexXDuration_mod <- glm(
    cond_dur ~ 
      cond + 
      Captivation + Challenge + Competition + Completion + Discovery + Progression + Exploration + Fantasy + Humor + Nurture + Relaxation + Sensation, 
    family=Gamma, 
    data=dat
) #This serves to demonstrate the subjective nature of play 
#Usability
usability <- wilcox.test(data = Q, Usability ~ cond) # symmetrical bimodal distribution
usability_lo <- t.test(data = Q %>% filter(Usability<55), Usability ~ cond, var.equal=TRUE) # normally distributed and equal variances: bartlett.test(Usability ~ cond, data=Q %>% filter(Usability>50) ) && shapiro.test(( Q %>% filter(Usability<55) )$Usability )
usability_hi <- t.test(data = Q %>% filter(Usability>60), Usability ~ cond, var.equal=TRUE) # normally distributed and equal variances: bartlett.test(Usability ~ cond, data=Q %>% filter(Usability>50) ) && shapiro.test(( Q %>% filter(Usability>60) )$Usability )

#LEARNING
#Mcq ; plot(mcq, 1) ; plot(mcq, 2)
mcq <- glm( # assumes linearity but relaxes assumption for constant variance
  formula=test_score~cond_binary,
  data=Q,
  family=poisson
) # Not normal and not homogeneous variance: bartlett.test(cond_dur ~ cond, data=Q) :   K-squared = 109.46, df = 1, p-value < 2.2e-16 ALSO not bimodal: plot(table(Q$test_score)) ALSO not normal: shapiro.test(Q$test_score)
#Learning Over Time
learningInGame_dat <- gameplay %>%
  filter(condition=='game') %>%
  group_by( t=as.numeric( t ) ) %>% 
  summarise( n=n(), mean=mean(score),sd=sd(score),min=min(score),max=max(score) ) %>%
  filter(n>10, t>0)
learningInGame <- lm( # normal and linear BUT heteroscescastic : plot(learningInGame, 1) ; plot(learningInGame, 2) ; shapiro.test(residuals(learningInGame))
  formula = learningInGame_dat$t ~ learningInGame_dat$mean, 
  data = gameplay 
)
learningInGame_adj <- coeftest(learningInGame, vcov=vcovHC(learningInGame) )

#RESEARCH
#Players make more valid solutions than random guessing
playerScores_dat <- allInterventions %>% mutate(
  Score_group = case_when(
    Score==0 ~ 1,
    Score==100 ~ 3,
    TRUE ~ 2
  )
)
playerScores_test_chi <- chisq.test(
  playerScores_dat$Score_group, 
  playerScores_dat$Group,
  simulate.p.value = TRUE
)

#And of these valid solutions they suggest more effective solutions 
plot(table(
  playerScores_dat$Score_group
))
playerScores <- glm(
  formula=Group ~ Score,
  family=binomial,
  data=playerScores_dat %>% 
    filter(Score>0) 
) # plot(playerScores, 2)

## SUMMARY
duration_q
duration_sw 
plex
plexXDuration
plexXDuration_mod
usability 
usability_lo
usability_hi
mcq
learningInGame
playerScores 

## SAVE
write.csv(descriptives, 'outputs/descriptives.csv')
write.csv(
  rbind(
    broom::tidy(duration_q), #glm - gamma
    broom::tidy(duration_sw), #glm - "
    broom::tidy(plexXDuration), #glm - logistic regression
    broom::tidy(plexXDuration_mod), #glm - moderator 
    broom::tidy(learningInGame) # lm - corrected for heteroscescadascicity
  ), 'outputs/results_LMs.csv')
write.csv(
  rbind(
    broom::tidy(usability_lo), #t - parametric
    broom::tidy(usability_hi) #t - "
  ), 'outputs/results_Ts.csv')
write.csv(
  rbind(
    broom::tidy(mcq), #wilcox - not normal but homogenous
    broom::tidy(usability), #wilcox - slightly bimodal but symmetrical
    broom::tidy(plex), #glm - negative binomial; not normal and unequal vars
    broom::tidy(playerScores_test) # wilcox - large differences in sample size and not symmetrical
  ), 'outputs/results_Wil.csv')