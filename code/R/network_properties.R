# Initialise
library('dplyr')
library('readr')
setwd("C:/git/gsa") # Set WD

data <- read_csv("data/cleaned/gameplay.csv")
g <- read_csv("data/cleaned/network_properties.csv")
AI <- read_csv("data/cleaned/allPossibleInterventions.csv")

## Prepare human data #########################################

  # Seperate game and control data
  data <- filter(data, condition == 'game')
  data_IV <- filter(data, condition == 'iv')
  
  # Remove col target3 due to low sample size (n=2)
  data <- select(data, -target3)
  
  # Remove invalid data (n=62)
  data <- filter(data, n_interventions != 0) 
  
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
  
  # Enrich with network properties
  data <- left_join(data, g, by = c("target" = "ORIGIN"))
  data <- rename(data, score_obj = score)
  data <- rename(data, score_side = SCORE)
  data <- rename(data, cmplx_steps = STEPS)
  
## Prepare AI data #########################################
  
  # Remove initial intervention effects
  AI[AI == -1]<-NA
  AI[AI == 1]<-NA
  
  # Get AI responses
  
  # To single intervention policies
    # Some impossible objectives omitted:
    #AI %>% arrange( desc(Depression) ) %>% slice(1), Intervention on Depression is not possible
    #AI %>% arrange( desc(BMI) ) %>% slice(1), These traits below are neutral
    #AI %>% arrange( desc(`Coffee intake`) ) %>% slice(1),
    #AI %>% arrange( desc(Eveningness) ) %>% slice(1),
    objectiveAIResponses <- rbind (
      cbind(objective='Worry', AI %>% arrange( Worry ) %>% slice(1)),
      cbind(objective='Wellbeing', AI %>% arrange( Wellbeing ) %>% slice(1)),
      cbind(objective='Loneliness', AI %>% arrange( Loneliness ) %>% slice(1)),
      cbind(objective='Sleeplessness', AI %>% arrange( Sleeplessness ) %>% slice(1)),
      cbind(objective='Neuroticism', AI %>% arrange( Neuroticism ) %>% slice(1)),
      cbind(objective='Alcohol', AI %>% arrange( Alcohol ) %>% slice(1)),
      cbind(objective='Education', AI %>% arrange( Education ) %>% slice(1)),
      cbind(objective='Intelligence', AI %>% arrange( Intelligence ) %>% slice(1)),
      cbind(objective='Not socialising', AI %>% arrange( `Not socialising` ) %>% slice(1)),
      cbind(objective='Smoking', AI %>% arrange( Smoking ) %>% slice(1)),
      cbind(objective='Exercise', AI %>% arrange( Exercise ) %>% slice(1)),
      cbind(objective='CHD', AI %>% arrange( CHD ) %>% slice(1)),
      cbind(objective='Diabetes', AI %>% arrange( Diabetes ) %>% slice(1))
    )
    objectiveAIResponses$cmplx_interventionMax = 1
    objectiveAIResponses$cmplx_interventionN = 1
    objectiveAIResponses$score_obj = 100
    
    # Bootstrap responses to bring # up to player count
    objectiveAIResponses_sample <- sample_n(objectiveAIResponses, sum(data$cmplx_interventionMax == 1), replace=TRUE)
    
    # Simplify 
    objectiveAIResponses_sample <- select(objectiveAIResponses_sample, c(objective, ORIGIN, cmplx_interventionMax, score_obj, cmplx_interventionN))
    
  # To double intervention policies
    
    # Get AI responses where 2 interventions are required
    dbleobjectiveAIResponses <- rbind (
      cbind(objective='Worry', AI %>% arrange( Worry ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Worry ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Wellbeing', AI %>% arrange( Wellbeing ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Wellbeing ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Loneliness', AI %>% arrange( Loneliness ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Loneliness ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Sleeplessness', AI %>% arrange( Sleeplessness ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Sleeplessness ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Neuroticism', AI %>% arrange( Neuroticism ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange(  Neuroticism  ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Alcohol', AI %>% arrange( Alcohol ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange(Alcohol) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Education', AI %>% arrange( Education ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Education ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Intelligence', AI %>% arrange( Intelligence ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Intelligence ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Not socialising', AI %>% arrange( `Not socialising` ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( `Not socialising` ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Smoking', AI %>% arrange( Smoking ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Smoking ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Exercise', AI %>% arrange( Exercise ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Exercise ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='CHD', AI %>% arrange( CHD ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( CHD ) %>% slice(2) %>% select(ORIGIN)),
      cbind(objective='Diabetes', AI %>% arrange( Diabetes ) %>% slice(1) %>% select(ORIGIN), AI %>% arrange( Diabetes ) %>% slice(2) %>% select(ORIGIN))
    )
    colnames(dbleobjectiveAIResponses) <- c("objective", "target1", "target2")
    dbleobjectiveAIResponses$cmplx_interventionMax = 2
    dbleobjectiveAIResponses$score_obj = 100
    
    # Bootstrap responses to bring # up to player count
    dbleobjectiveAIResponses_sample <- sample_n(dbleobjectiveAIResponses, sum(data$cmplx_interventionMax == 2), replace=TRUE)
    
    # Separate interventions onto two lines
    dbleobjectiveAIResponses_p1 <- select(dbleobjectiveAIResponses_sample, -target2)
    dbleobjectiveAIResponses_p1$cmplx_interventionN <- 1
    dbleobjectiveAIResponses_p1 <- rename(dbleobjectiveAIResponses_p1, ORIGIN = target1)
    
    dbleobjectiveAIResponses_p2 <- select(dbleobjectiveAIResponses_sample, -target1)
    dbleobjectiveAIResponses_p2$cmplx_interventionN <- 2
    dbleobjectiveAIResponses_p2 <- rename(dbleobjectiveAIResponses_p2, ORIGIN = target2)
    
    dbleobjectiveAIResponses_sample_joined <- rbind(dbleobjectiveAIResponses_p1, dbleobjectiveAIResponses_p2)
    
    # Simplify 
    dbleobjectiveAIResponses_sample_joined <- select(dbleobjectiveAIResponses_sample_joined, c(objective, ORIGIN, cmplx_interventionMax, cmplx_interventionN, score_obj))
    
  # Enrich data with network properties 
  data_AI <- rbind(
    left_join(objectiveAIResponses_sample, g, by = c("ORIGIN" = "ORIGIN")),
    left_join(dbleobjectiveAIResponses_sample_joined, g, by = c("ORIGIN" = "ORIGIN"))
  )
  data_AI <- rename(data_AI, target = ORIGIN)
  data_AI <- rename(data_AI, score_side = SCORE)
  data_AI <- rename(data_AI, cmplx_steps = STEPS)
  
  # Combine data
  data$group <- 'human'
  
  data_AI$group <- 'computer'
  data_AI$user <- 'AI_v1.1'
  data_AI$condition <- 'game'
  data_AI$time <- NA
  
  dat <- rbind(data, data_AI)
  
## Analyze data #########################################
  
  # Descriptives
  summarise(data)
  summarise(data_AI)
  
    # Most common interventions
    dat %>% group_by(group) %>% count(target)
    # inspect non mathematical reasons including valence and real-world actions
    sum(data$intervention_valence == 'Good')
    sum(data_AI$intervention_valence == 'Good')

  # Compare groups
  compareGroups <- rbind(
    broom::tidy( t.test(dat$score_obj ~ dat$group, var.equal = TRUE) )[0,],
    broom::tidy( t.test(dat$score_obj ~ dat$group, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$score_side ~ dat$group, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$cmplx_steps ~ dat$group, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Predeessors ~ dat$group, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Successors ~ dat$group, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$Neighbours ~ dat$group, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$betweennessCentrality ~ dat$group, var.equal = TRUE) )[1,],
    broom::tidy( t.test(dat$eigenvectorCentrality ~ dat$group, var.equal = TRUE) )[1,]
  )
  compareGroups$Measures = c('score_obj', 'score_side', 'cmplx_steps', 'Predeessors', 'Successors', 'Neighbours', 'betweennessCentrality', 'eigenvectorCentrality')
    
  write_csv(  compareGroups[, c(11,1,2,3,4,5,6,7,8,9, 10)], 'outputs/solutions.csv')

  ## Heatmap of interventions
  write_csv(  
    dat %>% group_by(group, target) %>% summarise( 
      n = n(),
      centrality = mean(eigenvectorCentrality),
      neighbours = mean(Neighbours)
    ),
    'outputs/solutions_heatmap.csv'
  )
  
  
  mode <- function(x) { # https://rpubs.com/Mentors_Ubiqum/using_mode
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  write_csv(  
    dat %>% group_by(group, objective) %>% summarise( mode = mode(target) ),
    'outputs/solutions_forEach.csv'
  )
  
  sd ( 
    (
    dat %>% filter(group=='human') %>% group_by(user) %>% summarise( n = n() ) 
    )$n
  )
  
  dat %>% filter(group=='human') %>% summarise(
    max = max(score_obj),
    min = min(score_obj),
    mean = mean(score_obj),
    sd = sd(score_obj) 
    )
  
  # Within subjects comparisons
  # make data individual level
  
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
  