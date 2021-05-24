## initialisation
library('readr')
library('psych')
library('broom')
library('Hmisc')
library('corrplot')
library('qpcR')
library('dplyr')
setwd("C:/py/gsa") # Set WD

## scoring
Q <- read_csv("data/cleaned/Q_excludedPpts.csv")

## Score and transform measures

  # Convert time measures
    # Convert seconds to minutes
    Q <- Q %>% 
      mutate(cond_dur = cond_dur/60)
    Q <- Q %>% 
      mutate(test_dur = test_dur/60)
    
  # Plot
    # Time using software
    par(mfrow=c(1,2)) 
    hist(Q$cond_dur[Q$cond == 'Exp'], main='Time playing game', xlab='Minutes')
    hist(Q$cond_dur[Q$cond == 'Ctr'], main='Time using visualisation', xlab='Minutes')
    
    # Time on assessment
    par(mfrow=c(1,1))
    hist(Q$test_dur, main='Time on assessment', xlab='Minutes')

  # Score likerts
    
    # Positively score likerts
    Q <- Q %>% 
      mutate(Motivation = 11 - Motivation)
    Q <- Q %>% 
      mutate(Direction = 11 - Direction)
    
    # Plot
    par(mfrow=c(2,2))
    hist(Q$Motivation[Q$cond == 'Exp'], ylim=c(0,40), main='Playing game', xlab='Agreement')
    hist(Q$Motivation[Q$cond == 'Ctr'], ylim=c(0,40), main='Using control', xlab='Agreement')
    title("'I felt motivated...'", line = -1, outer = TRUE)
    
    hist(Q$Direction[Q$cond == 'Exp'], ylim=c(0,40), main='Playing game', xlab='Agreement')
    hist(Q$Direction[Q$cond == 'Ctr'], ylim=c(0,40), main='Using control', xlab='Agreement')
    title("'I felt guided...'", line = -14, outer = TRUE)
    
  # PLEXs
    
    # Compute sum
    Q <- Q %>% 
      rowwise() %>% 
      mutate(PLEX_count = 
               sum(c(Captivation,Challenge,Competition,Completion,Discovery,Progression,Exploration,Fantasy,Humor,Nurture,Relaxation,Sensation),na.rm=TRUE) 
             )
  
    # Plot
    plexs <- rbind(data.frame(
      Exploration=sum(Q$Exploration[Q$cond == 'Exp']),
      Discovery=sum(Q$Discovery[Q$cond == 'Exp']),
      Progression=sum(Q$Progression[Q$cond == 'Exp']),
      Fantasy=sum(Q$Fantasy[Q$cond == 'Exp']),
      Sensation=sum(Q$Sensation[Q$cond == 'Exp']),
      Challenge=sum(Q$Challenge[Q$cond == 'Exp']),
      Captivation=sum(Q$Captivation[Q$cond == 'Exp']),
      Nurture=sum(Q$Nurture[Q$cond == 'Exp']),
      Relaxation=sum(Q$Relaxation[Q$cond == 'Exp']),
      Completion=sum(Q$Completion[Q$cond == 'Exp']),
      Competition=sum(Q$Competition[Q$cond == 'Exp']),
      Humor=sum(Q$Humor[Q$cond == 'Exp'])
    ), data.frame(
      Exploration=sum(Q$Exploration[Q$cond == 'Ctr']),
      Discovery=sum(Q$Discovery[Q$cond == 'Ctr']),
      Progression=sum(Q$Progression[Q$cond == 'Ctr']),
      Fantasy=sum(Q$Fantasy[Q$cond == 'Ctr']),
      Sensation=sum(Q$Sensation[Q$cond == 'Ctr']),
      Challenge=sum(Q$Challenge[Q$cond == 'Ctr']),
      Captivation=sum(Q$Captivation[Q$cond == 'Ctr']),
      Nurture=sum(Q$Nurture[Q$cond == 'Ctr']),
      Relaxation=sum(Q$Relaxation[Q$cond == 'Ctr']),
      Completion=sum(Q$Completion[Q$cond == 'Ctr']),
      Competition=sum(Q$Competition[Q$cond == 'Ctr']),
      Humor=sum(Q$Humor[Q$cond == 'Ctr'])
    ))
    rownames(plexs) <- c('Game', 'Control')
    write_csv(plexs, "data/cleaned/descr_plex.csv") # Excel for plot
    
  # Explore usability
    par(mfrow=c(1,2))
    hist(Q$Usability[Q$cond == 'Exp'], ylim=c(0,20), main='Playing game', xlab='Usability')
    hist(Q$Usability[Q$cond == 'Ctr'], ylim=c(0,20), main='Using control', xlab='Usability')
    title("Usability", line = -1, outer = TRUE)
    
  # Score MCQ
  
    # Try different methods of scoring
      # Raw
      # Raw, as percent
      Q <- Q %>% 
        mutate(test_score_pct = test_score / 22 * 100 ) 
      # Adjusted for guessing
      Q <- Q %>% 
        mutate(test_score_adjusted = test_score - 8 ) 
      # Adjusted for guessing, as percent
      Q <- Q %>% 
        mutate(test_score_adjusted_pct = test_score_adjusted / 16 * 100 ) # 16.64 highest possible score
      # Z-score
      Q <- Q %>%  
        mutate(test_score_z = (
          (test_score - 15.672) / 1.977013
          # mean = 15.672 # mean(Q$test_score, na.rm=TRUE)
          # sd = 1.977013 # sd(Q$test_score, na.rm=TRUE)
      ))
    
    # Plot
    par(mfrow=c(2,3))
    hist(Q$test_score, main='Raw', xlab='Score (Correct answers)', xlim=c(0,22))
    hist(Q$test_score_pct, main='Raw (%)', xlab='Score (Percent)', xlim=c(0,100))
    hist(Q$test_score_z, main='Z-score', xlab='Score (SD)')
    hist(Q$test_score_adjusted, main='Adjusted for guessing', xlab='Score (Correct answers)', xlim=c(0,22))
    hist(Q$test_score_adjusted_pct, main='Adjusted for guessing (%)', xlab='Score (Percent)', xlim=c(0,100))
    title("Scoring methods for MCQ", line = -1, outer = TRUE)
    
    # Validity
    
      # Hardest Qs
      par(mfrow=c(1,1))
      mcq_answers <- data.frame(
        Q1 = sum(Q$Q1, na.rm=TRUE),
        Q2 = sum(Q$Q2, na.rm=TRUE),
        Q3 = sum(Q$Q3, na.rm=TRUE),
        Q4 = sum(Q$Q4, na.rm=TRUE),
        Q5 = sum(Q$Q5, na.rm=TRUE),
        Q6 = sum(Q$Q6, na.rm=TRUE),
        Q7 = sum(Q$Q7, na.rm=TRUE),
        Q8 = sum(Q$Q8, na.rm=TRUE),
        Q9 = sum(Q$Q9, na.rm=TRUE),
        Q10 = sum(Q$Q10, na.rm=TRUE),
        Q11 = sum(Q$Q11, na.rm=TRUE),
        Q12 = sum(Q$Q12, na.rm=TRUE),
        Q13 = sum(Q$Q13, na.rm=TRUE),
        Q14 = sum(Q$Q14, na.rm=TRUE),
        Q15 = sum(Q$Q15, na.rm=TRUE),
        Q16 = sum(Q$Q16, na.rm=TRUE),
        Q17 = sum(Q$Q17, na.rm=TRUE),
        Q18 = sum(Q$Q18, na.rm=TRUE),
        Q19 = sum(Q$Q19, na.rm=TRUE),
        Q20 = sum(Q$Q20, na.rm=TRUE),
        Q21 = sum(Q$Q21, na.rm=TRUE),
        Q22 = sum(Q$Q22, na.rm=TRUE)
      )
      barplot(as.matrix(mcq_answers),
              las=2, xlab='Question', 
              main='Correct answers per question', 
              ylab='Participants answered correctly (n)', 
              ylim=c(0,175),
              axes=FALSE
      )
      mean(t(mcq_answers))
      axis(2,at=seq(0,175,25))# Identified Q14, Q18 as potentially problematic
      
      # Correlation matrix
      matrix_mcq <- rcorr(as.matrix( Q %>%
                select(test_score,Q1,	Q2,	Q3,	Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q11,	Q12,	Q13,	Q14,	Q15,	Q16, Q17,	Q18,	Q19,	Q20,	Q21,	Q22)
      ))
      colnames(matrix_mcq$P)[1] <- 'Total Score'; colnames(matrix_mcq$r)[1] <- 'Total Score'; rownames(matrix_mcq$P)[1] <- 'Total Score'; rownames(matrix_mcq$r)[1] <- 'Total Score'
      par(mfrow=c(1,1))
      corrplot(matrix_mcq$r, 
               type = "upper", 
               p.mat = matrix_mcq$P, 
               sig.level = 0.05,
               insig = "blank",
               diag=FALSE,
               main='')# Identified Q1,3,17,20 as potentially problematic
    
      # Internal consistency
      
        # Original estimate
        mcq_alpha <- psych::alpha(Q %>%
            select(Q1,	Q2,	Q3,	Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q11,	Q12,	Q13,	Q14,	Q15,	Q16, Q17,	Q18,	Q19,	Q20,	Q21,	Q22),
            na.rm=TRUE
        )
        # Remove problematic Qs (14,18,1,3,17,20)
        mcq_alpha2 <- psych::alpha(Q %>%
           select(Q2, Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q11,	Q12,	Q13,	Q15,	Q16, Q19,	Q21, Q22),
           na.rm=TRUE
        )
        # Remove negatively correlated with new total score (11,19)
        mcq_alpha3 <- psych::alpha(Q %>%
           select(Q2, Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q12,	Q13,	Q15,	Q16,	Q21, Q22),
           na.rm=TRUE
        )
        # Remove least loading (10,22)
        mcq_alpha4 <- psych::alpha(Q %>%
           select(Q2, Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q12,	Q13,	Q15,	Q16,	Q21),
           na.rm=TRUE
        )
        # Plot
        par(mfrow=c(1,1))
        boxplot(qpcR:::cbind.na(
            mcq_alpha$item.stats$raw.r,
            mcq_alpha2$item.stats$raw.r,
            mcq_alpha3$item.stats$raw.r,
            mcq_alpha4$item.stats$raw.r
          ), 
          ylim=c(0,0.6), ylab='Correlation with total score (r)', names=FALSE
        )
        mtext("Original \n n=22 \n a=.43", line=2, side=1, at=1, cex=0.8)
        mtext("Step 1 (prob.) \n n=18 \n a=.53", line=2, side=1, at=2, cex=0.8)
        mtext("Step 2 (-ve) \n n=12 \n a=.58", line=2, side=1, at=3, cex=0.8)
        mtext("Step 3 (poor) \n n=10 \n a=.60", line=2, side=1, at=4, cex=0.8)
        title("Individual MCQ question loadings", line=-1, outer = TRUE)
    
        # Alpha by subsection
        #mcq_alpha <- data.frame(
        #    "1) Graph reading" = psych::alpha(Q %>% select(Q1,	Q2,	Q3,	Q4,	Q5,	Q6),na.rm=TRUE)$total$raw_alpha,
        #    "2) Direct effects" = psych::alpha(Q %>% select(Q7, Q8, Q9),na.rm=TRUE)$total$raw_alpha,
        #    "3) Network properties" = psych::alpha(Q %>% select(Q10, Q11),na.rm=TRUE)$total$raw_alpha,
        #    "4) Interactions" = psych::alpha(Q %>% select(Q12, Q13),na.rm=TRUE)$total$raw_alpha,
        #    "5) Negating effects" = psych::alpha(Q %>% select(Q14, Q15),na.rm=TRUE)$total$raw_alpha,
        #    "6) Indirect effects" = psych::alpha(Q %>% select(Q16, Q17, Q18, Q19, Q20, Q21, Q22),na.rm=TRUE)$total$raw_alpha
        #  )
        
  # Score intervention task
  
    # Define function
    primaryScore <- function(trait, objective){
      i <- AI %>% filter(ORIGIN == trait)
      return (as.double(i[objective]))
    }
    sideScore <- function(trait){
      i <- AI %>% filter(ORIGIN == trait)
      return (as.double(i$SCORE))
    }
    scoreFfe <- function(df, test, objective){
      addPrimaryScore <- df %>% 
        rowwise() %>% 
        mutate(!!paste('test_ffe',test,'_primary',sep='') := sum(c(
          primaryScore(!!sym(paste('test_ffe',test,'_1',sep='')), objective),
          primaryScore(!!sym(paste('test_ffe',test,'_2',sep='')), objective),
          primaryScore(!!sym(paste('test_ffe',test,'_3',sep='')), objective) 
        ), na.rm=TRUE)
        )
      addSideScore <- addPrimaryScore %>% 
        rowwise() %>% 
        mutate(!!paste('test_ffe',test,'_side',sep='') := sum(c(
          sideScore(!!sym(paste('test_ffe',test,'_1',sep=''))),
          sideScore(!!sym(paste('test_ffe',test,'_2',sep=''))),
          sideScore(!!sym(paste('test_ffe',test,'_3',sep=''))) 
        ), na.rm=TRUE)
        )
      return(addSideScore)
    }
    
    # Explore scoring methods
    
      # Score all with raw scores
      Q <- scoreFfe(Q, 1, 'Wellbeing')
      Q <- scoreFfe(Q, 2, 'Not socialising') # Note: Reducing not socialisation is the same as increasing socialisation
      Q <- scoreFfe(Q, 3, 'Smoking')
      Q <- scoreFfe(Q, 4, 'CHD')
      
      # Score primary
      
        # Score as Pct of max
        Q <- Q %>% 
          mutate(test_ffe1_primary_pct = test_ffe1_primary / 1.063948313 * 100)
        Q <- Q %>% 
          mutate(test_ffe2_primary_pct = test_ffe2_primary / -0.590338799 * 100)
        Q <- Q %>% 
          mutate(test_ffe3_primary_pct = test_ffe3_primary / -6.93150945 * 100)
        Q <- Q %>% 
          mutate(test_ffe4_primary_pct = test_ffe4_primary / -3.263130341 * 100)
        
        # Score as Z
        Q <- Q %>% 
          mutate(test_ffe1_primary_z = (test_ffe1_primary_pct - mean(Q$test_ffe1_primary_pct, na.rm=TRUE)) / sd(Q$test_ffe1_primary_pct, na.rm=TRUE) )
        Q <- Q %>% 
          mutate(test_ffe2_primary_z = (test_ffe2_primary_pct - mean(Q$test_ffe2_primary_pct, na.rm=TRUE)) / sd(Q$test_ffe2_primary_pct, na.rm=TRUE) )
        Q <- Q %>% 
          mutate(test_ffe3_primary_z = (test_ffe3_primary_pct - mean(Q$test_ffe3_primary_pct, na.rm=TRUE)) / sd(Q$test_ffe3_primary_pct, na.rm=TRUE) )
        Q <- Q %>% 
          mutate(test_ffe4_primary_z = (test_ffe4_primary_pct - mean(Q$test_ffe4_primary_pct, na.rm=TRUE)) / sd(Q$test_ffe4_primary_pct, na.rm=TRUE) )
      
        # Plot
        par(mfrow=c(3,4))
        hist(Q$test_ffe1_primary, main='1. Increase wellbeing', xlab='Beta', ylim=c(0,80))
        hist(-Q$test_ffe2_primary, main='2. Increase socialisation', xlab='Beta', ylim=c(0,80))
        hist(Q$test_ffe3_primary, main='3. Reduce smoking', xlab='Beta', ylim=c(0,80), xlim=c(0,-7))
        hist(Q$test_ffe4_primary, main='4. Reduce heart disease', xlab='Beta', ylim=c(0,80), xlim=c(0,-4))
        title("Raw", line = -1, outer = TRUE)
        hist(Q$test_ffe1_primary_pct, xlab='% of optimum', xlim=c(0,100), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe2_primary_pct, xlab='% of optimum', xlim=c(0,100), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe3_primary_pct, xlab='% of optimum', xlim=c(0,100), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe4_primary_pct, xlab='% of optimum', xlim=c(0,100), ylim=c(0,80), main=NULL)
        title("Percentage", line = -15, outer = TRUE)
        hist(Q$test_ffe1_primary_z, xlab='SD', xlim=c(-3,3), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe2_primary_z, xlab='SD', xlim=c(-3,3), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe3_primary_z, xlab='SD', xlim=c(-3,3), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe4_primary_z, xlab='SD', xlim=c(-3,3), ylim=c(0,80), main=NULL)
        title("Z-Score", line = -25, outer = TRUE)
      
    # Score side effects
        
        # Score as Pct of max
        Q <- Q %>% 
          mutate(test_ffe1_side_pct = test_ffe1_side / 21.82497 * 100)
        Q <- Q %>% 
          mutate(test_ffe2_side_pct = test_ffe2_side / 21.82497 * 100)
        Q <- Q %>% 
          mutate(test_ffe3_side_pct = test_ffe3_side / 21.82497 * 100)
        Q <- Q %>% 
          mutate(test_ffe4_side_pct = test_ffe4_side / 21.82497 * 100)
        
        # Score as Z
        Q <- Q %>% 
          mutate(test_ffe1_side_z = (test_ffe1_side_pct - mean(Q$test_ffe1_side_pct, na.rm=TRUE)) / sd(Q$test_ffe1_side_pct, na.rm=TRUE) )
        Q <- Q %>% 
          mutate(test_ffe2_side_z = (test_ffe2_side_pct - mean(Q$test_ffe2_side_pct, na.rm=TRUE)) / sd(Q$test_ffe2_side_pct, na.rm=TRUE) )
        Q <- Q %>% 
          mutate(test_ffe3_side_z = (test_ffe3_side_pct - mean(Q$test_ffe3_side_pct, na.rm=TRUE)) / sd(Q$test_ffe3_side_pct, na.rm=TRUE) )
        Q <- Q %>% 
          mutate(test_ffe4_side_z = (test_ffe4_side_pct - mean(Q$test_ffe4_side_pct, na.rm=TRUE)) / sd(Q$test_ffe4_side_pct, na.rm=TRUE) )
        
        # Plot
        par(mfrow=c(3,4))
        hist(Q$test_ffe1_side, main='1. Increase wellbeing', xlab='Beta', ylim=c(0,80))
        hist(Q$test_ffe2_side, main='2. Increase socialisation', xlab='Beta', ylim=c(0,80))
        hist(Q$test_ffe3_side, main='3. Reduce smoking', xlab='Beta', ylim=c(0,80))
        hist(Q$test_ffe4_side, main='4. Reduce heart disease', xlab='Beta', ylim=c(0,80))
        title("Raw", line = -1, outer = TRUE)
        hist(Q$test_ffe1_side_pct, xlab='% of optimum', xlim=c(0,100), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe2_side_pct, xlab='% of optimum', xlim=c(0,100), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe3_side_pct, xlab='% of optimum', xlim=c(0,100), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe4_side_pct, xlab='% of optimum', xlim=c(0,100), ylim=c(0,80), main=NULL)
        title("Percentage", line = -15, outer = TRUE)
        hist(Q$test_ffe1_side_z, xlab='SD', xlim=c(-3,3), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe2_side_z, xlab='SD', xlim=c(-3,3), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe3_side_z, xlab='SD', xlim=c(-3,3), ylim=c(0,80), main=NULL)
        hist(Q$test_ffe4_side_z, xlab='SD', xlim=c(-3,3), ylim=c(0,80), main=NULL)
        title("Z-Score", line = -25, outer = TRUE)
        
        # Correlate
        cbind(
          cor(Q$test_ffe1_primary, Q$test_ffe1_primary_pct),
          cor(Q$test_ffe1_primary, Q$test_ffe1_primary_z),
          cor(Q$test_ffe1_side, Q$test_ffe1_side_pct),
          cor(Q$test_ffe1_side, Q$test_ffe1_side_z)
        )

    # Total scores
      Q <- Q %>% 
          rowwise() %>% 
          mutate(ffe_total_primary = 
            mean(c(test_ffe1_primary_z,test_ffe2_primary_z,test_ffe3_primary_z,test_ffe4_primary_z),na.rm=TRUE) 
          )
      Q <- Q %>% 
          rowwise() %>% 
          mutate(ffe_total_side = 
              mean(c(test_ffe1_side_z,test_ffe2_side_z,test_ffe3_side_z,test_ffe4_side_z),na.rm=TRUE) 
          )
        
      # Plot
      par(mfrow=c(1,2))
      hist(Q$ffe_total_primary, main='Total objective scores', xlab='SD', ylim=c(0,50))
      hist(Q$ffe_total_side, main='Total side effect scores', xlab='SD', ylim=c(0,50))
      
## Loadings
testLoadings <- as.data.frame(
  cor(cbind(
    Q$test_score_z,
    Q$test_ffe_primaryTotal, 
    Q$test_ffe_SideTotal
  )
  ))
colnames(testLoadings) <- c('MCQ score', 'FFE primary score', 'FFE side score')
rownames(testLoadings) <- c('MCQ score', 'FFE primary score', 'FFE side score')

## save
dat <- Q %>% select(
  cond, cond_dur,
  Motivation,
  Direction, 
  FreeText,
  PLEX_count,
  Captivation,Challenge,Competition,Completion,Discovery,Progression,Exploration,Fantasy,Humor,Nurture,Relaxation,Sensation,
  test_score_z, test_dur,
  test_ffe_total
)
write_csv(dat, "data/cleaned/Q_scored.csv")