## initialise
library('dplyr')
library('readr')
setwd("C:/py/gsa") # Set WD

## read in data
Q_hc <- read_csv("data/raw/Q_halfClean.csv")

## clean data
  # combine condition info
  Q_hc <- Q_hc %>% 
    mutate(cond = case_when(
      !is.na(G) ~ 'Exp',
      !is.na(IV) ~ 'Ctr'
    ))
  Q_hc <- Q_hc %>% 
    mutate(cond_dur = case_when(
      G_dur - (G_break_dur*60) > 0 ~ G_dur - (G_break_dur*60),
      !is.na(G) ~ G_dur,
      IV_dur - (IV_break_dur*60) > 0 ~ IV_dur - (IV_break_dur*60),
      !is.na(IV) ~ IV_dur,
  ))
  
  # combine total time on assessment
  Q_hc <- Q_hc %>% 
    rowwise() %>% 
    mutate(test_dur = sum(c(s1dur,s2dur,s3dur,s4dur,s5dur,s6dur,s7dur), na.rm=TRUE))
  
  # fix scoring on Q1
  par(mfrow=c(1,2))
  hist(Q_hc$Q1, main='Raw Q1 score', xlab='Score')
  Q_hc$Q1[which(Q_hc$Q1 > 1)] <- 1
  Q_hc$Q1[which(Q_hc$Q1 <= 0.8)] <- 0
  hist(Q_hc$Q1, xlim=c(0, 1.2), main='Fixed Q1 score', xlab='Score')
  
  # combine total mcq assessment score
  Q_hc <- Q_hc %>% 
    rowwise() %>% 
    mutate(test_score = sum(c(Q1,	Q2,	Q3,	Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q11,	Q12,	Q13,	Q14,	Q15,	Q16, Q17,	Q18,	Q19,	Q20,	Q21,	Q22), na.rm=TRUE))
  
  # convert intervention data into more useful values
  Q_hc[Q_hc == 'Increase wellbeing'] <- 'Wellbeing'
  Q_hc[Q_hc == 'Increase eveningness'] <- 'Eveningness'
  Q_hc[Q_hc == 'Reduce alcohol'] <- 'Alcohol'
  Q_hc[Q_hc == 'Increase intelligence'] <- 'Intelligence'
  Q_hc[Q_hc == 'Increase exercise'] <- 'Exercise'
  Q_hc[Q_hc == 'Increase coffee consumption'] <- 'Coffee intake'
  Q_hc[Q_hc == 'Increase socialisation'] <- 'Not socialising'
  Q_hc[Q_hc == 'Reduce BMI'] <- 'BMI'
  Q_hc[Q_hc == 'Reduce diabetes'] <- 'Diabetes'
  Q_hc[Q_hc == 'Reduce heart disease'] <- 'CHD'
  Q_hc[Q_hc == 'Reduce smoking'] <- 'Smoking'
  Q_hc[Q_hc == 'Increase education'] <- 'Education'
  Q_hc[Q_hc == 'Reduce insomnia'] <- 'Sleeplessness'
  Q_hc[Q_hc == 'Reduce loneliness'] <- 'Loneliness'
  Q_hc[Q_hc == 'Reduce neuroticism'] <- 'Neuroticism'
  Q_hc[Q_hc == 'Reduce depression'] <- 'Depression'
  Q_hc[Q_hc == 'Reduce worry'] <- 'Worry'
  
  # convert plex scores into binary
  binPlex <- function(col){
    col[col > 1] <- 1
    col[is.na(col)] <- 0
    return (col)
  }
  Q_hc$Captivation <- binPlex(Q_hc$Captivation)
  Q_hc$Challenge <- binPlex(Q_hc$Challenge)
  Q_hc$Competition <- binPlex(Q_hc$Competition)
  Q_hc$Completion <- binPlex(Q_hc$Completion)
  Q_hc$Discovery <- binPlex(Q_hc$Discovery)
  Q_hc$Progression <- binPlex(Q_hc$Progression)
  Q_hc$Exploration <- binPlex(Q_hc$Exploration)
  Q_hc$Fantasy <- binPlex(Q_hc$Fantasy)
  Q_hc$Humor <- binPlex(Q_hc$Humor)
  Q_hc$Nurture <- binPlex(Q_hc$Nurture)
  Q_hc$Relaxation <- binPlex(Q_hc$Relaxation)
  Q_hc$Sensation <- binPlex(Q_hc$Sensation)
  
  # select used cols
  o <- Q_hc %>% select(
    cond, cond_dur,
    Motivation, Direction, 
    FreeText,
    test_score,test_dur,
    test_ffe1_1,test_ffe1_2,test_ffe1_3,
    test_ffe2_1,test_ffe2_2,test_ffe2_3,
    test_ffe3_1,test_ffe3_2,test_ffe3_3,
    test_ffe4_1,test_ffe4_2,test_ffe4_3,
    Usability, 
    Q1,	Q2,	Q3,	Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q11,	Q12,	Q13,	Q14,	Q15,	Q16, Q17,	Q18,	Q19,	Q20,	Q21,	Q22,
    Captivation,Challenge,Competition,Completion,Discovery,Progression,Exploration,Fantasy,Humor,Nurture,Relaxation,Sensation
  )

## Save
write_csv(o, "./data/cleaned/Q.csv")