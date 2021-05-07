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

  # convert plex into more useful arrangement
  perPLEX <- function(df, plexN, plexNm){
    return (df %>% 
      mutate(!!plexNm := case_when(
        PLEX1 == plexN ~ 1,
        PLEX2 == plexN ~ 1,
        PLEX3 == plexN ~ 1,
        PLEX4 == plexN ~ 1,
        PLEX5 == plexN ~ 1,
        PLEX6 == plexN ~ 1,
        PLEX7 == plexN ~ 1,
        PLEX8 == plexN ~ 1,
        PLEX9 == plexN ~ 1,
        PLEX10 == plexN ~ 1,
        PLEX11 == plexN ~ 1,
        PLEX12 == plexN ~ 1,
        TRUE ~ 0
        )
      )
    )
  }
  Q_hc <- perPLEX(Q_hc, 1,  'Captivation')
  Q_hc <- perPLEX(Q_hc, 2,  'Challenge')
  Q_hc <- perPLEX(Q_hc, 3,  'Competition')
  Q_hc <- perPLEX(Q_hc, 4,  'Completion')
  Q_hc <- perPLEX(Q_hc, 5,  'Discovery')
  Q_hc <- perPLEX(Q_hc, 6,  'Progression')
  Q_hc <- perPLEX(Q_hc, 7,  'Exploration')
  Q_hc <- perPLEX(Q_hc, 8,  'Fantasy')
  Q_hc <- perPLEX(Q_hc, 9,  'Humor')
  Q_hc <- perPLEX(Q_hc, 10,  'Nurture')
  Q_hc <- perPLEX(Q_hc, 11,  'Relaxation')
  Q_hc <- perPLEX(Q_hc, 12,  'Sensation')
  
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
  
  # select used cols
  o <- Q_hc %>% select(
    cond, cond_dur,
    Motivation, Direction, 
    'Captivation','Challenge','Competition','Completion','Discovery','Progression','Exploration','Fantasy','Humor','Nurture','Relaxation','Sensation',
    FreeText,
    test_score,test_dur,
    test_ffe1_1,test_ffe1_2,test_ffe1_3,
    test_ffe2_1,test_ffe2_2,test_ffe2_3,
    test_ffe3_1,test_ffe3_2,test_ffe3_3,
    test_ffe4_1,test_ffe4_2,test_ffe4_3
  )

## Save
write_csv(o, "./data/cleaned/Q.csv")