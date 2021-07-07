# Initialise
library('dplyr')
library('readr')
setwd("C:/py/gsa") # Set WD

data <- read_csv("data/cleaned/gameplay.csv")
g <- read_csv("data/cleaned/network_properties.csv")

## Data preparation #########################################

  # Filter out control data
  data <- filter(data, condition == 'game')
  
  # Remove col target3 due to low sample size
  data <- select(data, -target3)
  
  # Rename cols
  data <- rename(data, cmplx_interventionMax = n_interventions)
  
  # Rename traits
  data[data == 'ieu_a_1187']<-'Depression'
  data[data == 'ukb_b_6519']<-'Worry'
  data[data == 'ieu_a_1018']<-'Wellbeing'
  data[data == 'ukb_b_8476']<-'Loneliness'
  data[data == 'ukb_b_3957']<-'Sleeplessness'
  data[data == 'ukb_d_SLEEP']<-'ICD10 Insomni'
  data[data == 'ukb_b_4062']<-'Happiness'
  data[data == 'ieu_a_118']<-'Neuroticism'
  data[data == 'ukb_b_5779']<-'Alcohol'
  data[data == 'ieu_a_1239']<-'Education'
  data[data == 'ukb_b_19953']<-'BMI'
  data[data == 'ukb_b_5238']<-'Intelligence'
  data[data == 'ukb_b_4956']<-'Eveningness'
  data[data == 'ukb_b_5076']<-'Not socialising'
  data[data == 'ieu_a_961']<-'Smoking'
  data[data == 'ukb_b_4710']<-'Exercise'
  data[data == 'ukb_b_5237']<-'Coffee intake'
  data[data == 'ieu_a_7']<-'CHD'
  data[data == 'ukb_b_4424']<-'Sleep duration'
  data[data == 'ieu_a_24']<-'Diabetes'
  
  # Separate target 2 onto second line
  data_p1 <- select(data, -target2)
  data_p1 <- rename(data_p1, target = target1)
  data_p1$cmplx_interventionN <- 1

  data_p2 <- select(data, -target1)
  data_p2 <- rename(data_p2, target = target2)
  data_p2 <- filter(data_p2, !is.na(target))
  data_p2$cmplx_interventionN <- 2
  
  data <- rbind(data_p1, data_p2)
  
  
## Combine data #########################################
  
  data <- left_join(data, g, by = c("target" = "ORIGIN"))
  data <- rename(data, score_obj = score)
  data <- rename(data, score_side = SCORE)
  data <- rename(data, cmplx_steps = STEPS)
  
## Analyse data #########################################
  
  data
  
  library('Hmisc')
  results_corr <- rcorr(as.matrix(data %>% select(where(is.numeric))))
  
  library('corrplot')
  corrplot(results_corr$r, 
           type = "upper", 
           p.mat = results_corr$P, 
           sig.level = 0.05,
           insig = "blank",
           diag=FALSE,
           main='')
  