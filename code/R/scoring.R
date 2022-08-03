## Initialisation
lapply(c("psych","readr","broom", "Hmisc", "corrplot","qpcR","dplyr","ggplot2", "ggfortify")
       , require, character.only = TRUE)
setwd("C:/git/gsa") # Set WD

## Read in cleaned data
Q <- read_csv("data/cleaned/Q_excludedPpts.csv")

## Score and transform measures

  # Convert time measures from seconds to minutes
  Q <- Q %>% 
    mutate(cond_dur = cond_dur/60)
  Q <- Q %>% 
    mutate(test_dur = test_dur/60)

  # Score likerts positively so a higher value indicates greater agreement
  Q <- Q %>% 
    mutate(Motivation = 11 - Motivation)
  Q <- Q %>% 
    mutate(Direction = 11 - Direction)
    
  # PLEXs
    
    # Compute sum of total reported playful experiences
    Q <- Q %>% 
      rowwise() %>% 
      mutate(PLEX_count = 
               sum(c(Captivation,Challenge,Competition,Completion,Discovery,Progression,Exploration,Fantasy,Humor,Nurture,Relaxation,Sensation),na.rm=TRUE) 
             )
  
    # Plot differences in reported experiences across game and non-game conditions
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
    
  # Plot usability
    describe(Q$Usability)
    sd(Q$Usability)
    t.test(Q$Usability[Q$cond == 'Exp'], Q$Usability[Q$cond == 'Ctr'],  var.equal = TRUE)
    par(mfrow=c(1,2))
    hist(Q$Usability[Q$cond == 'Exp'], ylim=c(0,20), main='Playing game', xlab='Usability')
    hist(Q$Usability[Q$cond == 'Ctr'], ylim=c(0,20), main='Using control', xlab='Usability')
    title("Usability", line = -1, outer = TRUE)
  
  # Compare answers on each question
      # Means
      Q %>% group_by(cond) %>% summarise(mean=mean(test_score))
      t.test(Q$cond, Q$test_score, na.rm=TRUE)
      
      # Individual Qs
      Q_mcq <- data.frame(Ctr=number, )
      for(q in Q %>% dplyr::select(Q1:Q22) ){
        tab <- table(Q$cond, q)
        chi <- chisq.test( tab )
        print(tidy(chi))
      }
      
      chi_fn <- function(x, stat){
        chi <- tidy( chisq.test(table( Q$cond, x )) )
        output <- as.numeric(chi[1,stat])
        return(output)
      }
      mcq_chi <- rbind(
        lapply( Q %>% filter(cond=='Exp') %>% dplyr::select(Q1:Q22) , mean, na.rm=TRUE),
        lapply( Q%>% filter(cond=='Ctr') %>% dplyr::select(Q1:Q22) , mean, na.rm=TRUE),
        lapply( Q %>% dplyr::select(Q1:Q22), chi_fn, 1),
        lapply( Q %>% dplyr::select(Q1:Q22), chi_fn, 2),
        lapply( Q %>% dplyr::select(Q1:Q22), chi_fn, 3)
      )
      rownames(mcq_chi) <- c(
        'Game',
        'Control',
        colnames(tidy( chisq.test(table( Q$cond, Q$Q1 )) ))[1],
        colnames(tidy( chisq.test(table( Q$cond, Q$Q1 )) ))[2],
        colnames(tidy( chisq.test(table( Q$cond, Q$Q1 )) ))[3]
      )
      mcq_chi <- as.data.frame(mcq_chi)
      write_csv(as.data.frame(mcq_chi), 'outputs/mcq_chi.csv')
      
  # Score MCQ
      
    #t
    t.test(Q$test_score ~ Q$cond)
  
    # Convert score to z-score
    Q <- Q %>% # Z-score
      mutate(test_score_z = (
        (test_score - mean(Q$test_score, na.rm=TRUE)) / sd(Q$test_score, na.rm=TRUE)
    ))
    
    # Explore question difficulty
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
      
    # Check inter item correlations
    matrix_mcq <- rcorr(as.matrix( Q %>%
              select(test_score,Q1,	Q2,	Q3,	Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q11,	Q12,	Q13,	Q14,	Q15,	Q16, Q17,	Q18,	Q19,	Q20,	Q21,	Q22)
    ))
    write.csv(matrix_mcq$r, 'data/cleaned/mcq_corrs_r.csv', row.names = FALSE)
    write.csv(matrix_mcq$P, 'data/cleaned/mcq_corrs_P.csv', row.names = FALSE)
    colnames(matrix_mcq$P)[1] <- 'Total Score'; colnames(matrix_mcq$r)[1] <- 'Total Score'; rownames(matrix_mcq$P)[1] <- 'Total Score'; rownames(matrix_mcq$r)[1] <- 'Total Score'
    par(mfrow=c(1,1))
    corrplot(matrix_mcq$r, 
             type = "upper", 
             p.mat = matrix_mcq$P, 
             sig.level = 0.05,
             insig = "blank",
             diag=FALSE,
             main='')# Identified Q1,3,17,20 as potentially problematic
    
    # Check internal consistency
    mcq_alpha <- psych::alpha(Q %>%
        select(Q1,	Q2,	Q3,	Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q11,	Q12,	Q13,	Q14,	Q15,	Q16, Q17,	Q18,	Q19,	Q20,	Q21,	Q22),
        na.rm=TRUE
    )
        
    # Explore multi-dimensionality of assessment with PCA
    allQs <- Q %>% select(Q1,	Q2,	Q3,	Q4,	Q5,	Q6,	Q7,	Q8,	Q9,	Q10,	Q11,	Q12,	Q13,	Q14,	Q15,	Q16, Q17,	Q18,	Q19,	Q20,	Q21,	Q22)
    #pca <- prcomp(na.omit(allQs), scale=TRUE)
    mcq_pca <- psych::principal(allQs, rotate="varimax", nfactors=4, scores=TRUE)
    pa <- fa(allQs,2,fm="pa" ,rotate="varimax")  #principal axis 
    mcq_scree <- VSS.scree(allQs)
    plot(pa$loadings)
    
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
    
    # Score responses
    
      Q <- scoreFfe(Q, 1, 'Wellbeing')
      Q <- scoreFfe(Q, 2, 'Not socialising') # Note: In the test participants were asked to increase socialisation but in the data the actual trait is "not socialising" but reducing not socialising is the same as increasing socialisation
      Q <- scoreFfe(Q, 3, 'Smoking')
      Q <- scoreFfe(Q, 4, 'CHD')
      
      # Score responses as percentage of best possible response
      Q <- Q %>% 
        mutate(test_ffe1_primary_pct = test_ffe1_primary / 1.063948313 * 100)
      Q <- Q %>% 
        mutate(test_ffe2_primary_pct = test_ffe2_primary / -0.590338799 * 100)
      Q <- Q %>% 
        mutate(test_ffe3_primary_pct = test_ffe3_primary / -6.93150945 * 100)
      Q <- Q %>% 
        mutate(test_ffe4_primary_pct = test_ffe4_primary / -3.263130341 * 100)
      
      # Transform to z score
      Q <- Q %>% 
        mutate(test_ffe1_primary_z = (test_ffe1_primary_pct - mean(Q$test_ffe1_primary_pct, na.rm=TRUE)) / sd(Q$test_ffe1_primary_pct, na.rm=TRUE) )
      Q <- Q %>% 
        mutate(test_ffe2_primary_z = (test_ffe2_primary_pct - mean(Q$test_ffe2_primary_pct, na.rm=TRUE)) / sd(Q$test_ffe2_primary_pct, na.rm=TRUE) )
      Q <- Q %>% 
        mutate(test_ffe3_primary_z = (test_ffe3_primary_pct - mean(Q$test_ffe3_primary_pct, na.rm=TRUE)) / sd(Q$test_ffe3_primary_pct, na.rm=TRUE) )
      Q <- Q %>% 
        mutate(test_ffe4_primary_z = (test_ffe4_primary_pct - mean(Q$test_ffe4_primary_pct, na.rm=TRUE)) / sd(Q$test_ffe4_primary_pct, na.rm=TRUE) )
      
    # Check individual item correlations
    matrix_ffe <- rcorr(as.matrix( Q %>%
        select(test_ffe1_primary_pct, test_ffe2_primary_pct, test_ffe3_primary_pct, test_ffe4_primary_pct),
    ))
    par(mfrow=c(1,1))
    matrix_ffe$r
    matrix_ffe$P
    corrplot(matrix_ffe$r, 
             type = "upper",
             method='number',
             p.mat = matrix_ffe$P, 
             sig.level = 0.05,
             insig = "blank",
             diag=FALSE,
             main='')
    
    # Check internal consistency
    ffe_alpha_primary <- psych::alpha(Q %>%
         select(test_ffe1_primary_pct,test_ffe2_primary_pct,test_ffe3_primary_pct,test_ffe4_primary_pct),
         na.rm=TRUE
    )

    # Total score
      Q <- Q %>% 
          rowwise() %>% 
          mutate(ffe_total_primary = 
            mean(c(test_ffe1_primary_z,test_ffe2_primary_z,test_ffe3_primary_z,test_ffe4_primary_z),na.rm=TRUE) 
          )
      
      # Get mean of scores
      Q <- Q %>% 
        rowwise() %>% 
        mutate(ffe_total_raw_primary = 
          mean(c(test_ffe1_primary_pct,test_ffe2_primary_pct,test_ffe3_primary_pct,test_ffe4_primary_pct),na.rm=TRUE) 
        )
      
      # Originally two scores were produced, a primary and side score, but since these loaded strongly on eachother and did not add anything the side score was removed for clarity of communication
      #cor(Q$ffe_total_primary, Q$ffe_total_side)
      Q <- Q %>% mutate(test_ffe_total = ffe_total_primary)
      
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